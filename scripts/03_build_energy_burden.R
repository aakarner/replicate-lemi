source("R/utils.R")
check_packages("data.table")
ensure_directories()

# PURPOSE ---------------------------------------------------------------------
# Independently reconstruct the DOE LEAD energy-burden lever from the official
# Texas 2022 census-tract archive. The LEAD file contains aggregated PUMS cells
# split by AMI group, tenure/building characteristics, and heating fuel. LEMI's
# definition describes the tract as a whole, so this script retains every cell.
cfg <- read_config()

archive_path <- "data/raw/TX-2022-LEAD-data.zip"
extract_dir <- "data/raw/lead_tx"
csv_path <- file.path(extract_dir, cfg$lead$archive_member)

# The state archive is roughly 444 MB. Download it only when it is not already
# cached; data/raw is ignored by Git and can be deleted/rebuilt safely.
if (!file.exists(archive_path)) {
  message("Downloading the official Texas 2022 DOE LEAD archive...")
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = max(3600, old_timeout))
  utils::download.file(
    cfg$lead$archive_url,
    archive_path,
    mode = "wb",
    method = "libcurl",
    quiet = FALSE
  )
}

if (!file.exists(csv_path)) {
  message("Extracting the AMI census-tract table from the Texas archive...")
  dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(
    archive_path,
    files = cfg$lead$archive_member,
    exdir = extract_dir,
    overwrite = TRUE
  )
}

study_area <- readr::read_csv(
  "data/processed/study_area_geoids.csv",
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
)

# Select only fields required for aggregation. The starred columns are already
# survey-weighted sums. The corresponding "... UNITS" columns count households
# with a nonmissing value for that component and therefore provide the correct
# denominator for each component average.
lead_columns <- c(
  "FIP", "AMI150", "TEN-YBL6", "TEN-BLD", "TEN-HFL", "UNITS",
  "HINCP*UNITS", "ELEP*UNITS", "GASP*UNITS", "FULP*UNITS",
  "HINCP UNITS", "ELEP UNITS", "GASP UNITS", "FULP UNITS"
)

lead <- data.table::fread(
  csv_path,
  select = lead_columns,
  showProgress = interactive()
)
lead[, geoid := as.character(FIP)]
lead <- lead[geoid %in% study_area$geoid]

# Reasonable reconstruction assumptions:
#   1. Include all occupied households and all AMI groups.
#   2. Calculate each cost component using its own nonmissing-household count.
#   3. Sum average electricity, gas, and other-fuel expenditures.
#   4. Divide by average annual household income and express as a percentage.
# This matches DOE's definition and avoids treating missing gas/fuel values as
# zero or assuming every component has an identical valid-household universe.
tract <- lead[, .(
  modeled_households = sum(UNITS, na.rm = TRUE),
  income_weighted_sum = sum(get("HINCP*UNITS"), na.rm = TRUE),
  income_valid_units = sum(get("HINCP UNITS"), na.rm = TRUE),
  electricity_weighted_sum = sum(get("ELEP*UNITS"), na.rm = TRUE),
  electricity_valid_units = sum(get("ELEP UNITS"), na.rm = TRUE),
  gas_weighted_sum = sum(get("GASP*UNITS"), na.rm = TRUE),
  gas_valid_units = sum(get("GASP UNITS"), na.rm = TRUE),
  other_fuel_weighted_sum = sum(get("FULP*UNITS"), na.rm = TRUE),
  other_fuel_valid_units = sum(get("FULP UNITS"), na.rm = TRUE)
), by = geoid]

tract[, average_household_income := income_weighted_sum / income_valid_units]
tract[, average_electricity_cost := electricity_weighted_sum / electricity_valid_units]
tract[, average_gas_cost := gas_weighted_sum / gas_valid_units]
tract[, average_other_fuel_cost := other_fuel_weighted_sum / other_fuel_valid_units]
tract[, average_total_energy_cost :=
  average_electricity_cost + average_gas_cost + average_other_fuel_cost]
tract[, energy_burden_continuous :=
  100 * average_total_energy_cost / average_household_income]

# The public LEMI field is an integer 0-4 category even though DOE's underlying
# burden is continuous. Conventional half-up rounding best reproduces it; cap
# the result at the report's five categories while retaining the continuous
# value for auditing and future methodological changes.
tract[, energy_burden := floor(energy_burden_continuous + 0.5)]
tract[, energy_burden := pmax(
  cfg$lead$category_min,
  pmin(cfg$lead$category_max, energy_burden)
)]

energy_burden <- study_area |>
  dplyr::select(geoid) |>
  dplyr::left_join(as.data.frame(tract), by = "geoid") |>
  dplyr::mutate(
    energy_burden_source = dplyr::if_else(
      is.na(energy_burden_continuous),
      "DOE LEAD 2022 tract absent; requires imputation",
      "Independent DOE/OEDI LEAD 2022 aggregation; all AMI groups and households"
    )
  )

assert_unique(energy_burden, "geoid", "DOE LEAD energy burden")
write_csv_stable(energy_burden, "data/processed/energy_burden_lead.csv")

# Compare with the public raw field without feeding public values into the
# reconstruction. Public missing values are excluded from agreement statistics.
public <- readr::read_csv(
  "data/raw/lemi_public_api.csv",
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
) |>
  dplyr::transmute(
    geoid,
    published_energy_burden = clean_census_numeric(energy_burden)
  )

comparison <- energy_burden |>
  dplyr::select(geoid, energy_burden_continuous, energy_burden) |>
  dplyr::left_join(public, by = "geoid") |>
  dplyr::mutate(
    difference = energy_burden - published_energy_burden,
    exact_match = dplyr::if_else(
      is.na(published_energy_burden) | is.na(energy_burden),
      NA,
      difference == 0
    )
  )
write_csv_stable(comparison, "output/diagnostics/energy_burden_public_validation.csv")

compared <- sum(!is.na(comparison$exact_match))
matched <- sum(comparison$exact_match, na.rm = TRUE)
metadata <- tibble::tibble(
  archive_url = cfg$lead$archive_url,
  archive_member = cfg$lead$archive_member,
  archive_md5 = unname(tools::md5sum(archive_path)),
  study_tracts = nrow(study_area),
  lead_tracts_found = sum(!is.na(energy_burden$energy_burden_continuous)),
  public_values_compared = compared,
  exact_rounded_matches = matched,
  exact_match_pct = 100 * matched / compared
)
write_csv_stable(metadata, "output/diagnostics/energy_burden_reconstruction_metadata.csv")

message(
  "DOE LEAD energy burden complete: ", matched, " of ", compared,
  " nonmissing public raw values reproduced exactly."
)
