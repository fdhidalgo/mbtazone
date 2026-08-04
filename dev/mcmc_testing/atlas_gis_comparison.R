# atlas_gis_comparison.R
#
# Reads density_denominator_all.csv and compares denom_atlas_implied vs
# denom_district directly. Key question: where district and Excel disagree,
# does Atlas side with the district method (suggesting Excel is wrong) or with
# Excel (suggesting the district polygon/deduction method is the problem)?
#
# Run from the mbtazone package root after density_denominator_comparison.R.

library(data.table)

dt <- data.table::fread("dev/mcmc_testing/density_denominator_all.csv")

# Communities where all three sources are available
cmp <- dt[!is.na(denom_district) & !is.na(denom_atlas_implied),
          .(community,
            denom_district,
            denom_atlas_implied,
            denom_excel,
            atlas_minus_district = round(denom_atlas_implied - denom_district,  3),
            district_minus_excel = round(denom_district      - denom_excel,     3),
            atlas_minus_excel    = round(denom_atlas_implied - denom_excel,     3))]
cmp <- cmp[order(-abs(district_minus_excel))]

cat("=== Atlas vs district vs Excel denominator comparison ===\n")
cat(sprintf("Communities with Atlas + district data: %d\n", nrow(cmp)))
cat(sprintf("Communities with all three (Atlas + district + Excel): %d\n\n",
            nrow(cmp[!is.na(denom_excel)])))

all3 <- cmp[!is.na(denom_excel)]

THRESH <- 2.0
all3[, atlas_sides_with_district := abs(atlas_minus_district) < THRESH]
all3[, atlas_sides_with_excel    := abs(atlas_minus_excel)    < THRESH]

cat(sprintf("(threshold = %.0f ac)\n", THRESH))
cat(sprintf("Atlas agrees with district: %d / %d\n",
            sum(all3$atlas_sides_with_district), nrow(all3)))
cat(sprintf("Atlas agrees with Excel:    %d / %d\n",
            sum(all3$atlas_sides_with_excel), nrow(all3)))
cat(sprintf("Atlas agrees with both:     %d / %d\n",
            sum(all3$atlas_sides_with_district & all3$atlas_sides_with_excel), nrow(all3)))
cat(sprintf("Atlas agrees with neither:  %d / %d\n\n",
            sum(!all3$atlas_sides_with_district & !all3$atlas_sides_with_excel), nrow(all3)))

# Focus on where district and Excel disagree
DISTRICT_EXCEL_THRESH <- 1.0
conflict <- all3[abs(district_minus_excel) > DISTRICT_EXCEL_THRESH]
conflict <- conflict[order(-abs(district_minus_excel))]

cat(sprintf("=== Communities where district and Excel disagree (> %.0f ac): %d ===\n\n",
            DISTRICT_EXCEL_THRESH, nrow(conflict)))

if (nrow(conflict) > 0) {
  conflict[, verdict := data.table::fcase(
    atlas_sides_with_district & !atlas_sides_with_excel,
      "Atlas=district  → Excel may be wrong",
    atlas_sides_with_excel & !atlas_sides_with_district,
      "Atlas=Excel     → district method may be wrong",
    atlas_sides_with_district & atlas_sides_with_excel,
      "Atlas=both      (small gap)",
    default = "Atlas disagrees with all"
  )]
  print(conflict[, .(community, denom_district, denom_atlas_implied, denom_excel,
                     atlas_minus_district, district_minus_excel, verdict)])
}

cat("\n=== All communities sorted by |district − excel| ===\n")
print(all3[order(-abs(district_minus_excel)),
           .(community, denom_district, denom_atlas_implied, denom_excel,
             atlas_minus_district, district_minus_excel, atlas_minus_excel)])
