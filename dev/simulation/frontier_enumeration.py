"""
frontier_enumeration.py — exact enumeration / counting of connected subgraphs
via a frontier sweep (frontier-based search, a.k.a. the ZDD construction).

WHY THIS EXISTS
---------------
grid_validation.R answers two questions exactly on a toy grid:
    (1) does the MCMC sample from its target distribution?
    (2) is the block library complete?
Question (2) currently loops over all 2^n subsets, which caps the toy graph at
4x4 (65,535 subsets). This module replaces that loop with a sweep whose cost
scales with the number of *feasible* subgraphs, not with 2^n.

It is the Python twin of frontier_enumeration.R — same node numbering (1-based,
row-major, matching grid_edges() in grid_validation.R), same semantics, but fast
enough to actually push past 4x4. Intended workflow: enumerate here, dump to
CSV, load into R alongside the MCMC samples for the comparison.

THE IDEA IN ONE PARAGRAPH
-------------------------
Decide parcels one at a time in a fixed order: in-district or out. After
deciding the first i parcels, the only thing the *future* can still touch is the
set of decided parcels that still have an undecided neighbour — the FRONTIER.
Everything behind it is buried and unreachable. Two prefixes are therefore
interchangeable when they agree on (a) which frontier parcels are in, (b) which
of those are already connected to each other through buried parcels, and (c) any
running totals a constraint needs. Memoise on that and the 2^n tree collapses to
a small DAG. A subgraph is a path through the DAG: walking paths enumerates, and
a DP over the DAG counts.

HONEST LIMITS
-------------
* COUNTING never materialises anything and scales a long way (7x7, 8x8 fine).
  Use it for exact library-recall denominators.
* ENUMERATION costs O(number of solutions). A 6x6 grid has 1.73e9 connected
  subgraphs — hopeless for ANY algorithm. Real thresholds collapse that fast.
  ALWAYS count() BEFORE enumerate().
* Running totals in the state key cost node sharing. Integer capacities/areas
  keep the DAG small; continuous weights shatter it. grid_validation.R already
  uses integer weights (capacity 1-5, area 3-10) — keep it that way.

NOT HANDLED IN THE SWEEP: min_lcc_fraction. It is a ratio
(lcc_capacity / total_capacity), and you cannot prune on a ratio — a future
component can always fix it, so you pay all the state-splitting cost and get no
pruning back. That is precisely the failure mode that pushed Kawahara et al.
(WALCOM 2017) to 1.38e9 ZDD nodes and out of memory on a 44-vertex graph.
The fix is to CONDITION on the LCC — see enumerate_secondaries().
"""

from __future__ import annotations

import argparse
import csv
import math
import random
from dataclasses import dataclass, field
from itertools import product
from typing import Callable, Iterator, Sequence

FALSE, TRUE = 0, 1  # terminal node ids


# ============================================================================
# 1. GRAPH
# ============================================================================

@dataclass
class Graph:
    """Vertices are 1..n, matching grid_validation.R's node names."""
    n: int
    adj: dict[int, list[int]]
    capacity: dict[int, int]
    area: dict[int, int]

    def induced_connected(self, nodes: Sequence[int]) -> bool:
        S = set(nodes)
        if not S:
            return False
        start = next(iter(S))
        seen, stack = {start}, [start]
        while stack:
            v = stack.pop()
            for u in self.adj[v]:
                if u in S and u not in seen:
                    seen.add(u)
                    stack.append(u)
        return seen == S

    def n_components(self, nodes: Sequence[int]) -> int:
        S, seen, k = set(nodes), set(), 0
        for v in S:
            if v in seen:
                continue
            k += 1
            stack = [v]
            seen.add(v)
            while stack:
                w = stack.pop()
                for u in self.adj[w]:
                    if u in S and u not in seen:
                        seen.add(u)
                        stack.append(u)
        return k


def make_grid(nrow: int, ncol: int, seed: int | None = 1,
              capacity: int | None = None, area: int | None = None) -> Graph:
    """Row-major ids: id = (r-1)*ncol + c, exactly as grid_edges() in R."""
    n = nrow * ncol
    adj: dict[int, list[int]] = {v: [] for v in range(1, n + 1)}
    for r in range(1, nrow + 1):
        for c in range(1, ncol + 1):
            v = (r - 1) * ncol + c
            if c < ncol:
                adj[v].append(v + 1)
                adj[v + 1].append(v)
            if r < nrow:
                adj[v].append(v + ncol)
                adj[v + ncol].append(v)

    if capacity is not None and area is not None:
        cap = {v: capacity for v in range(1, n + 1)}
        ar = {v: area for v in range(1, n + 1)}
    else:
        rng = random.Random(seed)
        cap = {v: rng.randint(1, 5) for v in range(1, n + 1)}
        ar = {v: rng.randint(3, 10) for v in range(1, n + 1)}
    return Graph(n=n, adj={k: sorted(v) for k, v in adj.items()},
                 capacity=cap, area=ar)


# ============================================================================
# 2. THE SWEEP
# ============================================================================

@dataclass
class DAG:
    root: int
    lo: dict[int, int]
    hi: dict[int, int]
    order: list[int]                 # sweep position -> vertex id
    n: int
    n_nodes: int
    accept: dict[int, tuple[int, int, int]] = field(default_factory=dict)
    # accept: for each node whose child is TRUE via arc x, we don't need this;
    # instead final-layer info is folded into the terminal decision.


def build(g: Graph, k_max: int = 1,
          min_capacity: int = 0, min_area: int = 0, min_density: float = 0.0,
          min_component_area: int = 0, max_capacity: int | None = None,
          order: Sequence[int] | None = None) -> DAG:
    """Build the decision DAG for subsets of `g` satisfying the constraints.

    k_max=1 gives single connected blobs (LCC / secondary candidates).
    k_max>1 allows a district with several discontiguous components.
    max_capacity is an upper bound — this is where the conditioned-on-LCC
    version of min_lcc_fraction lands.
    """
    ordv = list(order) if order is not None else list(range(1, g.n + 1))
    pos = {v: i for i, v in enumerate(ordv, start=1)}          # vertex -> position
    cap = [0] + [g.capacity[v] for v in ordv]                  # 1-indexed
    ar = [0] + [g.area[v] for v in ordv]
    adj_pos = {i: sorted(pos[u] for u in g.adj[ordv[i - 1]])
               for i in range(1, g.n + 1)}

    # frontier[i] = positions <= i that still have an undecided neighbour > i
    frontier: list[tuple[int, ...]] = [()] * (g.n + 2)
    for i in range(0, g.n + 1):
        frontier[i] = tuple(p for p in range(1, i + 1)
                            if any(q > i for q in adj_pos[p]))

    # suffix sums for "can this branch still reach the thresholds?" pruning
    rem_cap = [0] * (g.n + 2)
    rem_area = [0] * (g.n + 2)
    rem_dpos = [0.0] * (g.n + 2)
    for i in range(g.n, 0, -1):
        rem_cap[i] = rem_cap[i + 1] + cap[i]
        rem_area[i] = rem_area[i + 1] + ar[i]
        rem_dpos[i] = rem_dpos[i + 1] + max(0.0, cap[i] - min_density * ar[i])

    track_ca = min_component_area > 0

    lo: dict[int, int] = {}
    hi: dict[int, int] = {}
    next_id = 2

    # state = (labels over frontier, closed, cap, area, per-component area)
    root_state = ((), 0, 0, 0, ())
    root = next_id
    next_id += 1
    layer: dict[tuple, int] = {root_state: root}

    for i in range(1, g.n + 1):
        f_prev = frontier[i - 1]
        f_now = frontier[i]
        new_layer: dict[tuple, int] = {}

        for st, nid in layer.items():
            labs_prev, closed0, cap0, area0, ca0 = st
            lab_of = dict(zip(f_prev, labs_prev))

            for x in (0, 1):
                lab = dict(lab_of)
                closed, c_cap, c_area = closed0, cap0, area0
                comp_area = {j + 1: ca0[j] for j in range(len(ca0))} if track_ca else {}
                dead = False

                if x == 0:
                    lab[i] = 0
                else:
                    # every decided neighbour of i sits on the previous frontier
                    nb = {lab[p] for p in adj_pos[i] if p < i and lab.get(p, 0) > 0}
                    if not nb:
                        used = {v for v in lab.values() if v > 0}
                        gnew = 1
                        while gnew in used:
                            gnew += 1
                        lab[i] = gnew
                        if track_ca:
                            comp_area[gnew] = ar[i]
                    else:
                        keep = min(nb)
                        if track_ca:
                            comp_area[keep] = sum(comp_area.get(x_, 0) for x_ in nb) + ar[i]
                            for gd in nb - {keep}:
                                comp_area.pop(gd, None)
                        for p in list(lab):
                            if lab[p] in nb:
                                lab[p] = keep
                        lab[i] = keep
                    c_cap += cap[i]
                    c_area += ar[i]

                    if max_capacity is not None and c_cap > max_capacity:
                        dead = True

                # --- components with nothing left on the frontier are CLOSED
                if not dead:
                    alive = {lab[p] for p in f_now if lab.get(p, 0) > 0}
                    for gg in {v for v in lab.values() if v > 0} - alive:
                        if track_ca and comp_area.get(gg, 0) < min_component_area:
                            dead = True
                            break
                        closed += 1
                        comp_area.pop(gg, None)
                    if not dead and closed > k_max:
                        dead = True

                # --- reachability pruning
                if not dead:
                    if c_cap + rem_cap[i + 1] < min_capacity:
                        dead = True
                    elif c_area + rem_area[i + 1] < min_area:
                        dead = True
                    elif (c_cap - min_density * c_area) + rem_dpos[i + 1] < -1e-9:
                        dead = True

                if dead:
                    child = FALSE
                elif i == g.n:                                  # sweep over: accept?
                    ok = (1 <= closed <= k_max
                          and c_cap >= min_capacity
                          and c_area >= min_area
                          and (c_cap - min_density * c_area) >= -1e-9
                          and (max_capacity is None or c_cap <= max_capacity))
                    child = TRUE if ok else FALSE
                else:
                    # canonicalise labels by first appearance so that equivalent
                    # states collide in the dict and MERGE. This is the algorithm.
                    remap: dict[int, int] = {}
                    labs_new = []
                    for p in f_now:
                        L = lab.get(p, 0)
                        if L == 0:
                            labs_new.append(0)
                        else:
                            if L not in remap:
                                remap[L] = len(remap) + 1
                            labs_new.append(remap[L])
                    ca_new = tuple(comp_area.get(old, 0)
                                   for old, _ in sorted(remap.items(),
                                                        key=lambda kv: kv[1])) \
                        if track_ca else ()

                    st2 = (tuple(labs_new), closed, c_cap, c_area, ca_new)
                    child = new_layer.get(st2)
                    if child is None:
                        child = next_id
                        next_id += 1
                        new_layer[st2] = child

                if x == 0:
                    lo[nid] = child
                else:
                    hi[nid] = child

        layer = new_layer

    return DAG(root=root, lo=lo, hi=hi, order=ordv, n=g.n, n_nodes=next_id - 2)


# ============================================================================
# 3. USING THE DAG
# ============================================================================

def count(dag: DAG) -> int:
    """Exact number of solutions. Cheap. RUN THIS BEFORE enumerate()."""
    memo = {FALSE: 0, TRUE: 1}

    def rec(nd: int) -> int:
        if nd not in memo:
            memo[nd] = rec(dag.lo[nd]) + rec(dag.hi[nd])
        return memo[nd]

    return rec(dag.root)


def enumerate_all(dag: DAG, limit: int = 5_000_000) -> Iterator[tuple[int, ...]]:
    """Lazily yield every solution as a sorted tuple of vertex ids."""
    total = count(dag)
    if total > limit:
        raise ValueError(
            f"{total:,} solutions exceeds limit={limit:,}. Tighten the "
            f"constraints, or raise `limit` if you really want them all."
        )

    def rec(nd: int, depth: int, chosen: list[int]):
        if nd == FALSE:
            return
        if nd == TRUE:
            yield tuple(sorted(dag.order[d - 1] for d in chosen))
            return
        yield from rec(dag.lo[nd], depth + 1, chosen)              # parcel OUT
        chosen.append(depth)
        yield from rec(dag.hi[nd], depth + 1, chosen)              # parcel IN
        chosen.pop()

    yield from rec(dag.root, 1, [])


def sample_uniform(dag: DAG, rng: random.Random) -> tuple[int, ...]:
    """Exactly-uniform draw. No burn-in, no autocorrelation — an independent
    reference sample to hold the MCMC against."""
    memo = {FALSE: 0, TRUE: 1}

    def cnt(nd: int) -> int:
        if nd not in memo:
            memo[nd] = cnt(dag.lo[nd]) + cnt(dag.hi[nd])
        return memo[nd]

    cnt(dag.root)
    nd, depth, chosen = dag.root, 1, []
    while nd not in (TRUE, FALSE):
        a, b = cnt(dag.lo[nd]), cnt(dag.hi[nd])
        if rng.random() < b / (a + b):
            chosen.append(depth)
            nd = dag.hi[nd]
        else:
            nd = dag.lo[nd]
        depth += 1
    return tuple(sorted(dag.order[d - 1] for d in chosen))


# ============================================================================
# 4. CONDITIONING ON THE LCC  (this is how min_lcc_fraction is handled)
# ============================================================================
#
#   min_lcc_fraction says  cap(L) >= f * (cap(L) + cap(S)).
#   Fix L, and that is just     cap(S) <= cap(L) * (1 - f) / f
#   — a plain upper bound on a running sum, which prunes beautifully.
#   Secondaries must also avoid N[L] (the LCC *and its neighbours*), otherwise
#   they would merge into the LCC rather than being separate components.

def enumerate_secondaries(g: Graph, lcc: Sequence[int], constraints: dict,
                          k_max: int = 3, min_component_area: int = 0,
                          limit: int = 5_000_000) -> list[dict]:
    """All valid secondary sets for a FIXED candidate LCC."""
    lcc = sorted(lcc)
    cap_L = sum(g.capacity[v] for v in lcc)
    area_L = sum(g.area[v] for v in lcc)

    f = constraints["min_lcc_fraction"]
    cap_S_max = int(math.floor(cap_L * (1 - f) / f + 1e-9))

    forbidden = set(lcc)
    for v in lcc:
        forbidden.update(g.adj[v])
    eligible = sorted(set(range(1, g.n + 1)) - forbidden)

    out: list[dict] = []
    # the empty secondary set is always a candidate (k = 0)
    cands: list[tuple[int, ...]] = [()]

    if eligible:
        sub = Graph(
            n=len(eligible),
            adj={i + 1: sorted(eligible.index(u) + 1 for u in g.adj[v] if u in set(eligible))
                 for i, v in enumerate(eligible)},
            capacity={i + 1: g.capacity[v] for i, v in enumerate(eligible)},
            area={i + 1: g.area[v] for i, v in enumerate(eligible)},
        )
        dag = build(sub, k_max=k_max,
                    min_capacity=max(0, constraints["min_capacity"] - cap_L),
                    min_area=max(0, constraints["min_area"] - area_L),
                    min_density=0.0,               # applied jointly with L below
                    min_component_area=min_component_area,
                    max_capacity=cap_S_max)        # <- the LCC-fraction bound
        cands += [tuple(eligible[i - 1] for i in s)
                  for s in enumerate_all(dag, limit=limit)]

    for sec in cands:
        tot_c = cap_L + sum(g.capacity[v] for v in sec)
        tot_a = area_L + sum(g.area[v] for v in sec)
        if tot_c < constraints["min_capacity"] or tot_a < constraints["min_area"]:
            continue
        if tot_c / tot_a < constraints["min_density"]:
            continue
        if cap_L < constraints["min_lcc_fraction"] * tot_c:
            continue
        idx = tuple(sorted(set(lcc) | set(sec)))
        out.append(dict(lcc=tuple(lcc), secondaries=sec, indices=idx,
                        k=g.n_components(sec) if sec else 0,
                        capacity=tot_c, area=tot_a))
    return out


# ============================================================================
# 5. EXACT TARGET DISTRIBUTION  (the point of the whole exercise)
# ============================================================================

def exact_distribution(plans: list[dict], min_capacity: int,
                       lambda_cap: float, lambda_k: float,
                       reference_measure: bool = True) -> list[dict]:
    """Exact pi over the enumerated plans.

    Default log-weight matches the formula documented in dev/simulation/readme.md:

        log w = -lambda_cap * (capacity - min_capacity)
                - lambda_k  * k
                - log C(n_pool, k)          <- the reference-measure correction

    CHECK THIS against mcmc_parcel_mcmc_kernels.R before trusting it. If the
    sampler's MH ratio and this formula disagree, this is the thing that will
    silently make the comparison look broken (or, worse, look fine).
    """
    for p in plans:
        excess = max(0.0, p["capacity"] - min_capacity)
        lw = -lambda_cap * excess - lambda_k * p["k"]
        if reference_measure and p.get("n_pool") is not None and p["k"] > 0:
            lw -= math.log(math.comb(p["n_pool"], p["k"]))
        p["log_w"] = lw

    m = max(p["log_w"] for p in plans)
    z = sum(math.exp(p["log_w"] - m) for p in plans)
    for p in plans:
        p["prob"] = math.exp(p["log_w"] - m) / z
    return plans


def write_csv(path: str, rows: list[dict]) -> None:
    """Dump for the R side to read back and compare against MCMC samples."""
    with open(path, "w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["plan_key", "k", "capacity", "area", "log_w", "prob"])
        for r in rows:
            w.writerow([",".join(map(str, r["indices"])), r.get("k", ""),
                        r["capacity"], r["area"],
                        r.get("log_w", ""), r.get("prob", "")])


# ============================================================================
# 6. SELF-TEST / DEMO
# ============================================================================

def _brute_connected(g: Graph) -> int:
    total = 0
    for bits in product((0, 1), repeat=g.n):
        S = [i + 1 for i, b in enumerate(bits) if b]
        if S and g.induced_connected(S):
            total += 1
    return total


def _demo() -> None:
    print("=== 1. agreement with brute force (4x4) ===")
    g4 = make_grid(4, 4)
    d4 = build(g4, k_max=1)
    subs = list(enumerate_all(d4))
    print(f"  frontier : {count(d4):,} connected subgraphs  ({d4.n_nodes:,} DAG nodes)")
    print(f"  brute    : {_brute_connected(g4):,}")
    print(f"  walked   : {len(subs):,}   all connected: "
          f"{all(g4.induced_connected(s) for s in subs)}   "
          f"all distinct: {len(set(subs)) == len(subs)}")

    print("\n=== 2. counting scales past the brute-force wall ===")
    print(f"  {'grid':<8}{'connected subgraphs':>22}{'DAG nodes':>12}")
    for m in (4, 5, 6, 7):
        g = make_grid(m, m)
        d = build(g, k_max=1)
        print(f"  {f'{m}x{m}':<8}{count(d):>22,}{d.n_nodes:>12,}")
    print("  (2^49 = 5.6e14 — the 7x7 row is unreachable by the 2^n loop)")

    print("\n=== 3. constraints make ENUMERATION feasible ===")
    g6 = make_grid(6, 6)
    for mc in (0, 30, 45):
        d = build(g6, k_max=1, min_capacity=mc, min_area=60, min_density=0.35)
        print(f"  6x6, min_capacity >= {mc:<3}: {count(d):>15,} districts, "
              f"{d.n_nodes:>9,} DAG nodes")

    print("\n=== 4. multi-component districts, 5x5 ===")
    g5 = make_grid(5, 5)
    for k in (1, 2, 3):
        d = build(g5, k_max=k, min_capacity=25, min_area=45,
                  min_density=0.35, min_component_area=12)
        print(f"  k_max = {k}: {count(d):>12,} feasible districts")

    print("\n=== 5. conditioning on an LCC handles min_lcc_fraction ===")
    cons = dict(min_capacity=20, min_area=40, min_density=0.3,
                min_lcc_fraction=0.5)
    lcc = [1, 2, 3, 6, 7, 8]                       # a connected 6-parcel blob
    plans = enumerate_secondaries(g5, lcc, cons, k_max=2, min_component_area=6)
    print(f"  LCC {tuple(lcc)}  cap={sum(g5.capacity[v] for v in lcc)}")
    print(f"  -> {len(plans):,} feasible plans (k = 0..2 secondaries)")
    exact_distribution(plans, min_capacity=cons["min_capacity"],
                       lambda_cap=0.1, lambda_k=0.5, reference_measure=False)
    top = sorted(plans, key=lambda p: -p["prob"])[:3]
    for p in top:
        print(f"     k={p['k']} cap={p['capacity']:<3} area={p['area']:<3} "
              f"pi={p['prob']:.4f}  {p['indices']}")
    print(f"  sum(pi) = {sum(p['prob'] for p in plans):.6f}")

    print("\n=== 6. exactly-uniform draws (no burn-in, no autocorrelation) ===")
    rng = random.Random(42)
    d = build(g4, k_max=1, min_capacity=15, min_area=30, min_density=0.3)
    for _ in range(3):
        S = set(sample_uniform(d, rng))
        rows = [" ".join("#" if (r * 4 + c + 1) in S else "." for c in range(4))
                for r in range(4)]
        cap = sum(g4.capacity[v] for v in S)
        print("   " + "   |   ".join(rows) + f"   cap={cap}")


if __name__ == "__main__":
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[1])
    ap.add_argument("--demo", action="store_true", help="run the self-test")
    ap.add_argument("--rows", type=int, default=5)
    ap.add_argument("--cols", type=int, default=5)
    ap.add_argument("--k-max", type=int, default=1)
    ap.add_argument("--min-capacity", type=int, default=0)
    ap.add_argument("--min-area", type=int, default=0)
    ap.add_argument("--min-density", type=float, default=0.0)
    ap.add_argument("--min-component-area", type=int, default=0)
    ap.add_argument("--count-only", action="store_true")
    ap.add_argument("--out", type=str, default=None, help="write plans to CSV")
    a = ap.parse_args()

    if a.demo:
        _demo()
    else:
        g = make_grid(a.rows, a.cols)
        dag = build(g, k_max=a.k_max, min_capacity=a.min_capacity,
                    min_area=a.min_area, min_density=a.min_density,
                    min_component_area=a.min_component_area)
        n = count(dag)
        print(f"{a.rows}x{a.cols}: {n:,} feasible districts, {dag.n_nodes:,} DAG nodes")
        if not a.count_only and a.out:
            rows = [dict(indices=s, k=g.n_components(s),
                         capacity=sum(g.capacity[v] for v in s),
                         area=sum(g.area[v] for v in s))
                    for s in enumerate_all(dag)]
            write_csv(a.out, rows)
            print(f"wrote {len(rows):,} rows to {a.out}")
