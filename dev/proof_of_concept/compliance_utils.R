# Compliance Utils in R

library(tidyverse)

community_info_df <<- read_csv("data/community_info.csv")

get_community_info <- function(community_name) {

    row <- community_info_df %>%
        dplyr::filter(Community == community_name)

    cell_map <- list()
    for (i in 1:6) {
        cell_map[[paste0("I", 3 + i)]] <- row[[i + 1]][1]
    }

    return(cell_map)
}

apply_district_funcs <- function(
    df,
    water_included,
    max_units_per_lot,
    min_lot_size,
    base_min_lot_size,
    additional_lot_SF,
    building_height,
    FAR,
    max_lot_coverage,
    min_required_open_space,
    parking_spaces_per_dwelling_unit,
    lot_area_per_dwelling_unit,
    max_dwelling_units_per_acre) {
    
    if (nrow(df) == 0) {
        print("Empty dataframe")
        return(df)
    }

    # Initialize O column if missing
    if (!("O" %in% names(df))) {
        df$O <- NA
    }

    apply_N_func <- function() {
        df$N <- ifelse(
            df$I < min_lot_size,
            0,
            ifelse((df$I - df$L) < 0, 0, df$I - df$L)
        )
        return(df)
    }
    df <- apply_N_func()

    apply_Q_func <- function() {
        df$Q <- ifelse(is.na(df$O), df$N, df$O)
        return(df)
    }
    df <- apply_Q_func()

    # Excel code =IFERROR(L20/I20,0)
    apply_R_func <- function() {
        df$R <- ifelse(df$I == 0 | is.na(df$I), 0, df$L / df$I)
        return(df)
    }
    df <- apply_R_func()

    apply_S_func <- function() {
        s_value <- if (is.na(min_required_open_space) || (min_required_open_space < 1)) {
            max(0.2, min_required_open_space, na.rm = TRUE)
        } else {
            0.2
        }
        df$S <- s_value
        return(df)
    }
    df <- apply_S_func()

    apply_T_func <- function() {
        water_included_vec <- rep(water_included, nrow(df))
        df$T <- ifelse(
            is.na(df$O),
            ifelse(
                water_included_vec == "Y",
                pmax(df$R, df$S) * df$I,
                (df$R + df$S) * df$I
            ),
            ifelse(df$O > 0, df$Q * df$S, 0)
        )
        return(df)
    }
    df <- apply_T_func()

    apply_U_func <- function() {

        parking_factor <- function() {
            case_when(
                is.na(parking_spaces_per_dwelling_unit) ~ 0,
                parking_spaces_per_dwelling_unit == 0 ~ 0,
                parking_spaces_per_dwelling_unit >= 0.01 & parking_spaces_per_dwelling_unit <= 0.5 ~ 0.3,
                parking_spaces_per_dwelling_unit > 0.5 & parking_spaces_per_dwelling_unit <= 1 ~ 0.45,
                parking_spaces_per_dwelling_unit > 1 & parking_spaces_per_dwelling_unit <= 1.25 ~ 0.55,
                parking_spaces_per_dwelling_unit > 1.25 & parking_spaces_per_dwelling_unit <= 1.5 ~ 0.6,
                TRUE ~ 0.65
            )
        }

        p_factor <- parking_factor()

        df$U <- ifelse(
            !is.na(df$Q) & df$Q > 0,
            (df$I - df$T) * p_factor,
            0
        )
        return(df)
    }
    df <- apply_U_func()

    apply_V_func <- function() {
        df$V <- ifelse(
            !is.na(df$Q) & df$Q != 0,
            df$I - df$T - df$U,
            0
        )
        return(df)
    }
    df <- apply_V_func()

    apply_W_func <- function() {
        df$W <- ifelse(df$V > 0, df$V * building_height, 0)
        return(df)
    }
    df <- apply_W_func()

    apply_X_func <- function() {
        w_over_1000 <- df$W / 1000
        df$X <- case_when(
            w_over_1000 > 3 ~ floor(w_over_1000),
            w_over_1000 > 2.5 & w_over_1000 <= 3 ~ 3,
            TRUE ~ 0
        )
        return(df)
    }
    df <- apply_X_func()

    apply_Y_func <- function() {
        df$Y <- if (is.na(max_dwelling_units_per_acre) || max_dwelling_units_per_acre == "") {
            NA
        } else {
            (df$I / 43560) * max_dwelling_units_per_acre
        }
        return(df)
    }
    df <- apply_Y_func()

    apply_Z_func <- function() {
        df$Z <- if (is.na(max_lot_coverage) || max_lot_coverage == "") {
            NA
        } else {
            (df$I * max_lot_coverage * building_height) / 1000
        }
        return(df)
    }
    df <- apply_Z_func()

    apply_AA_func <- function() {
        df$AA <- if (is.na(lot_area_per_dwelling_unit) || lot_area_per_dwelling_unit == "") {
            NA
        } else if (lot_area_per_dwelling_unit > 0){
            df$I / lot_area_per_dwelling_unit
        }
        return(df)
    }
    df <- apply_AA_func()

    apply_AB_func <- function() {
        df$AB <- if (is.null(FAR) || is.na(FAR) || FAR == "") {
            NA
        } else {
            (df$I * FAR) / 1000
        }
        return(df)
    }
    df <- apply_AB_func()

    apply_AC_func <- function() {
        if (is.null(max_units_per_lot) || is.na(max_units_per_lot) || max_units_per_lot == "") {
            df$AC <- floor(df$X)
        } else {
            df$AC <- floor(
                case_when(
                    max_units_per_lot >= 3 & max_units_per_lot < df$X ~ max_units_per_lot,
                    max_units_per_lot < df$X & max_units_per_lot < 3 ~ 0,
                    TRUE ~ df$X
                )
            )
        }
        return(df)
    }
    df <- apply_AC_func()

    apply_AD_func <- function() {
        df$AD <- ifelse(df$I > 0 & df$I < min_lot_size, "Y", "")
        return(df)
    }
    df <- apply_AD_func()

    apply_AE_func <- function() {
        df$AE <- ifelse(
            df$AD == "Y",
            0,
            ifelse(
                is.null(additional_lot_SF) || is.na(additional_lot_SF) || additional_lot_SF == "",
                Inf,
                floor(((df$I - base_min_lot_size) / additional_lot_SF) + 1)
            )
        )
        return(df)
    }
    df <- apply_AE_func()

    apply_AF_func <- function() {
        cols <- c("X", "Y", "Z", "AA", "AB", "AC", "AE")
        min_values <- apply(df[, cols], 1, function(row) {
            row[is.na(row)] <- Inf
            min(row)
        })

        df$AF <- case_when(
            min_values < 2.5 ~ 0,
            min_values >= 2.5 & min_values < 3 ~ 3,
            TRUE ~ round(min_values)
        )
        return(df)
    }
    df <- apply_AF_func()

    # Consistent with Excel - handle division by zero or NA
    apply_AG_func <- function() {
        df$AG <- ifelse(df$I == 0 | is.na(df$I), 0, (43560 / df$I) * df$AF)
        return(df)
    }
    df <- apply_AG_func()

    return(df)
}
