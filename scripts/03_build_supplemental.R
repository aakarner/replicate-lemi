source("R/utils.R")
check_packages()
ensure_directories()

# PURPOSE ---------------------------------------------------------------------
# Assemble the non-ACS/non-eviction levers. Exact source extracts or parameters
# are not yet available for every lever, so the published *raw, pre-imputation*
# fields are used as explicit temporary fallbacks. No published final score or
# theme score enters the corrected calculation.
cfg <- read_config()
public <- readr::read_csv(
  "data/raw/lemi_public_api.csv",
  col_types = readr::cols(.default = readr::col_character())
)

numeric_field <- function(field) clean_census_numeric(public[[field]])

# Each value travels with a source label so downstream users can distinguish an
# independently supplied extract from the provisional public-raw fallback.
supplemental <- tibble::tibble(
  geoid = public$geoid,
  low_phys_activity = numeric_field("low_phys_activity"),
  pers_pov = numeric_field("pers_pov"),
  le_2020_normalized = numeric_field("le_2020_normalized"),
  le_data_coverage = numeric_field("le_data_coverage"),
  temperature_diff = numeric_field("temperature_diff"),
  max_lst_f = numeric_field("max_lst_f"),
  energy_burden = numeric_field("energy_burden"),
  gq_institutional_pct = numeric_field("gq_institutional_pct"),
  low_phys_activity_source = "published raw fallback; CDC PLACES 2025",
  pers_pov_source = "published raw fallback; Census persistent-poverty tract list",
  le_2020_normalized_source = "published raw fallback; USALEEP 2010-to-2020 areal crosswalk",
  temperature_diff_source = "published raw fallback; Climate Engine Landsat 8",
  energy_burden_source = "published raw fallback; DOE LEAD 2022",
  gq_institutional_pct_source = "published raw fallback; 2020 Decennial P5"
)

# A manual file overrides only matching GEOIDs and requested fields. This lets
# the pipeline improve incrementally as original City/Every Texan extracts are
# obtained, without changing scoring code or silently mixing row order.
override_source <- function(data, path, value_fields, source_label) {
  if (!file.exists(path)) return(data)
  replacement <- readr::read_csv(path, show_col_types = FALSE)
  assert_unique(replacement, "geoid", basename(path))
  missing_fields <- setdiff(c("geoid", value_fields), names(replacement))
  if (length(missing_fields)) {
    stop(path, " is missing: ", paste(missing_fields, collapse = ", "), call. = FALSE)
  }
  for (field in value_fields) {
    matched <- match(data$geoid, replacement$geoid)
    available <- !is.na(matched)
    data[[field]][available] <- replacement[[field]][matched[available]]
    source_field <- paste0(field, "_source")
    if (source_field %in% names(data)) data[[source_field]][available] <- source_label
  }
  data
}

supplemental <- override_source(
  supplemental, "data/manual/heat_disparity.csv", "temperature_diff",
  "user-supplied independent heat extract"
)
supplemental <- override_source(
  supplemental, "data/manual/energy_burden.csv", "energy_burden",
  "user-supplied independent DOE LEAD extract"
)
supplemental <- override_source(
  supplemental, "data/manual/life_expectancy_2020.csv", "le_2020_normalized",
  "user-supplied independent USALEEP crosswalk"
)

# Enforce one row per tract before this table reaches the MICE/scoring stage.
assert_unique(supplemental, "geoid", "supplemental lever table")
write_csv_stable(supplemental, "data/processed/supplemental_levers.csv")

provenance <- tibble::tribble(
  ~lever, ~current_source, ~independently_reproduced,
  "Low physical activity", "Published raw fallback; CDC PLACES 2025", FALSE,
  "Persistent poverty", "Published raw fallback; Census tract list", FALSE,
  "Life expectancy", "Published raw fallback; USALEEP areal crosswalk", FALSE,
  "Heat disparity", "Published raw fallback unless data/manual/heat_disparity.csv is supplied", FALSE,
  "Energy burden", "Published raw fallback unless data/manual/energy_burden.csv is supplied", FALSE,
  "Institutional group quarters", "Published raw fallback; 2020 Decennial P5", FALSE
)

# This summary is intended to remain prominent until each FALSE value can be
# replaced by an independently reproduced input.
write_csv_stable(provenance, "output/diagnostics/supplemental_provenance.csv")

message("Supplemental lever table complete; provisional fields are explicitly labeled.")
