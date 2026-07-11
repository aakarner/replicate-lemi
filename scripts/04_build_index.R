source("R/utils.R")
check_packages("mice")
ensure_directories()

cfg <- read_config()
acs <- readr::read_csv(
  "data/processed/acs_levers.csv",
  col_types = readr::cols(geoid = readr::col_character()),
  show_col_types = FALSE
)
evictions <- readr::read_csv(
  "data/processed/eviction_rates_2024.csv",
  col_types = readr::cols(geoid = readr::col_character()),
  show_col_types = FALSE
)
supplemental <- readr::read_csv(
  "data/processed/supplemental_levers.csv",
  col_types = readr::cols(geoid = readr::col_character()),
  show_col_types = FALSE
)

input <- acs |>
  dplyr::left_join(evictions, by = c("geoid", "renter_households", "renter_households_moe")) |>
  dplyr::left_join(supplemental, by = "geoid")
assert_unique(input, "geoid", "combined lever input")

lever_specs <- tibble::tribble(
  ~lever, ~favorable_when, ~percentile_field, ~theme, ~subtheme,
  "uninsured_pct", "low", "uninsured_pct_pctl", "health_wb", "health_acc",
  "prek_pct", "high", "prek_pct_pctl", "health_wb", "health_acc",
  "le_2020_normalized", "high", "le_2020_normalized_pctl", "health_wb", "health_acc",
  "disability_pct", "low", "disability_pct_pctl", "health_wb", "wb_act",
  "amb65_pct", "low", "amb65_pct_pctl", "health_wb", "wb_act",
  "low_phys_activity", "low", "low_phys_activity_pctl", "health_wb", "wb_act",
  "med_hh_income", "high", "med_hh_income_pctl", "liv_work", "inc_emp",
  "underemp_pct", "low", "underemp_pct_pctl", "liv_work", "inc_emp",
  "poverty_pct", "low", "poverty_pct_pctl", "liv_work", "inc_emp",
  "evict_fil_rate", "low", "evict_fil_rate_pctl", "liv_work", "hh_stab_cost",
  "hh_support_risk_score", "low", "hh_support_risk_score_pctl", "liv_work", "hh_stab_cost",
  "energy_burden", "low", "energy_burden_pctl", "liv_work", "hh_stab_cost",
  "lesh_pct", "low", "lesh_pct_pctl", "acc_bel", "edu_lang_dig",
  "no_internet_pct", "low", "no_internet_pct_pctl", "acc_bel", "edu_lang_dig",
  "lths_pct", "low", "lths_pct_pctl", "acc_bel", "edu_lang_dig",
  "pers_pov", "low", "pers_pov_pctl", "acc_bel", "env_neigh",
  "gq_institutional_pct", "low", "gq_institutional_pct_pctl", "acc_bel", "env_neigh",
  "temperature_diff", "low", "temperature_diff_pctl", "acc_bel", "env_neigh"
)

lever_names <- lever_specs$lever
lever_data <- as.data.frame(input[lever_names])

missingness <- tibble::tibble(
  lever = lever_names,
  missing_n = vapply(lever_data, function(x) sum(is.na(x)), integer(1)),
  missing_pct = 100 * missing_n / nrow(lever_data)
)
write_csv_stable(missingness, "output/diagnostics/lever_missingness_before_imputation.csv")

predictor_matrix <- mice::make.predictorMatrix(lever_data)
diag(predictor_matrix) <- 0
core_predictors <- c(
  "med_hh_income", "poverty_pct", "underemp_pct", "lths_pct",
  "no_internet_pct", "uninsured_pct"
)

# The report says eviction and life expectancy received custom models but does
# not publish the complete matrix. These restrictions encode the description
# and are written to disk so a City-supplied matrix can be compared later.
predictor_matrix["evict_fil_rate", ] <- 0
predictor_matrix["evict_fil_rate", core_predictors] <- 1
predictor_matrix["le_2020_normalized", ] <- 0
predictor_matrix[
  "le_2020_normalized",
  unique(c(core_predictors, "disability_pct", "low_phys_activity"))
] <- 1

write.csv(
  predictor_matrix,
  "output/diagnostics/mice_predictor_matrix.csv",
  row.names = TRUE,
  na = ""
)

method <- rep("pmm", length(lever_names))
names(method) <- lever_names
method[missingness$missing_n == 0] <- ""

message(
  "Running MICE: m=", cfg$imputation$m,
  ", maxit=", cfg$imputation$maxit,
  ", donors=", cfg$imputation$donors, "."
)
imputed <- mice::mice(
  lever_data,
  m = cfg$imputation$m,
  maxit = cfg$imputation$maxit,
  method = method,
  predictorMatrix = predictor_matrix,
  donors = cfg$imputation$donors,
  seed = cfg$imputation$seed,
  printFlag = FALSE
)
saveRDS(imputed, "data/processed/lemi_mice.rds")

score_completed_data <- function(completed) {
  scored <- as.data.frame(completed)
  for (i in seq_len(nrow(lever_specs))) {
    spec <- lever_specs[i, ]
    scored[[spec$percentile_field]] <- percent_rank_favorable(
      scored[[spec$lever]], spec$favorable_when
    )
  }

  for (subtheme_name in unique(lever_specs$subtheme)) {
    fields <- lever_specs$percentile_field[lever_specs$subtheme == subtheme_name]
    scored[[paste0(subtheme_name, "_pctl")]] <- row_percentile_mean(scored, fields)
  }
  for (theme_name in unique(lever_specs$theme)) {
    fields <- lever_specs$percentile_field[lever_specs$theme == theme_name]
    scored[[paste0(theme_name, "_pctl")]] <- row_percentile_mean(scored, fields)
  }
  scored$lemi_raw_mean <- rowMeans(scored[lever_specs$percentile_field])
  scored$lemi_pctl <- dplyr::percent_rank(scored$lemi_raw_mean) * 100
  scored
}

completion_number <- cfg$imputation$completed_dataset
completed <- mice::complete(imputed, action = completion_number)
scored <- score_completed_data(completed)

for (lever in lever_names) {
  scored[[paste0(lever, "_flag")]] <- ifelse(
    is.na(lever_data[[lever]]), "Imputed", "Original"
  )
}

scores <- dplyr::bind_cols(
  input |>
    dplyr::select(geoid, name, acs_year),
  scored
)
assert_unique(scores, "geoid", "corrected LEMI scores")
write_csv_stable(scores, "output/lemi_scores_corrected.csv")

# Show how much the unspecified random completion can affect the final score.
sensitivity <- purrr::map_dfr(seq_len(cfg$imputation$m), function(i) {
  completed_i <- mice::complete(imputed, action = i)
  scored_i <- score_completed_data(completed_i)
  tibble::tibble(
    geoid = input$geoid,
    imputation = i,
    lemi_pctl = scored_i$lemi_pctl
  )
})

sensitivity_summary <- sensitivity |>
  dplyr::group_by(geoid) |>
  dplyr::summarise(
    lemi_pctl_mean = mean(lemi_pctl),
    lemi_pctl_sd = stats::sd(lemi_pctl),
    lemi_pctl_min = min(lemi_pctl),
    lemi_pctl_max = max(lemi_pctl),
    .groups = "drop"
  )
write_csv_stable(sensitivity_summary, "output/diagnostics/imputation_score_sensitivity.csv")

message(
  "Corrected index complete. Score range: ",
  paste(round(range(scores$lemi_pctl), 2), collapse = " to "), "."
)
