source("R/utils.R")
check_packages()
ensure_directories()

cfg <- read_config()
census_key <- Sys.getenv("CENSUS_API_KEY")
if (!nzchar(census_key)) {
  stop("Set CENSUS_API_KEY in the environment before pulling ACS data.", call. = FALSE)
}

study_area <- readr::read_csv(
  "data/processed/study_area_geoids.csv",
  col_types = readr::cols(.default = readr::col_character())
)

census_tract_query <- function(dataset, variables, county_fips) {
  endpoint <- paste0(
    "https://api.census.gov/data/", cfg$acs_year, "/", dataset
  )
  response <- download_json(
    endpoint,
    query = list(
      get = paste(c("NAME", variables), collapse = ","),
      `for` = "tract:*",
      `in` = paste("state:", cfg$state_fips, " county:", county_fips, sep = ""),
      key = census_key
    )
  )
  response <- as.data.frame(response, stringsAsFactors = FALSE)
  names(response) <- response[1, ]
  response <- response[-1, , drop = FALSE]
  response$geoid <- paste0(response$state, response$county, response$tract)
  for (variable in variables) response[[variable]] <- clean_census_numeric(response[[variable]])
  response[c("geoid", "NAME", variables)]
}

pull_counties <- function(dataset, variables) {
  chunks <- split(variables, ceiling(seq_along(variables) / 45))
  purrr::map(chunks, function(variable_chunk) {
    purrr::map_dfr(
      unname(unlist(cfg$county_fips)),
      ~ census_tract_query(dataset, variable_chunk, .x)
    ) |>
      dplyr::select(-NAME)
  }) |>
    purrr::reduce(dplyr::full_join, by = "geoid")
}

subject_vars <- c(
  "S1810_C02_001E", "S1810_C02_001M", "S1810_C03_001E", "S1810_C03_001M",
  "S1810_C02_052E", "S1810_C02_052M", "S1810_C03_052E", "S1810_C03_052M",
  "S1401_C01_014E", "S1401_C01_014M", "S1401_C02_014E", "S1401_C02_014M",
  "S2701_C04_001E", "S2701_C04_001M", "S2701_C05_001E", "S2701_C05_001M",
  "S1701_C02_001E", "S1701_C02_001M", "S1701_C03_001E", "S1701_C03_001M",
  "S1602_C03_001E", "S1602_C03_001M", "S1602_C04_001E", "S1602_C04_001M"
)

profile_vars <- c(
  "DP02_0001E", "DP02_0001M",
  "DP02_0011E", "DP02_0011M", "DP02_0011PE", "DP02_0011PM",
  "DP02_0014E", "DP02_0014M", "DP02_0014PE", "DP02_0014PM"
)

lths_est <- sprintf("B15003_%03dE", 2:16)
lths_moe <- sprintf("B15003_%03dM", 2:16)
under_est <- paste0("B23022_", c("016E", "017E", "023E", "024E", "040E", "041E", "047E", "048E"))
under_moe <- sub("E$", "M", under_est)

detailed_vars <- c(
  "B01003_001E", "B01003_001M",
  "B19013_001E", "B19013_001M",
  "B28002_001E", "B28002_001M", "B28002_013E", "B28002_013M",
  "B15003_001E", "B15003_001M", lths_est, lths_moe,
  "B23022_003E", "B23022_003M", "B23022_027E", "B23022_027M", under_est, under_moe,
  "B11007_001E", "B11007_001M", "B11007_003E", "B11007_003M",
  "B25003_003E", "B25003_003M"
)

message("Pulling 2024 ACS subject tables...")
subject <- pull_counties("acs/acs5/subject", subject_vars)
message("Pulling 2024 ACS profile tables...")
profile <- pull_counties("acs/acs5/profile", profile_vars)
message("Pulling 2024 ACS detailed tables...")
detailed <- pull_counties("acs/acs5", detailed_vars)

acs <- study_area |>
  dplyr::left_join(subject, by = "geoid") |>
  dplyr::left_join(profile, by = "geoid") |>
  dplyr::left_join(detailed, by = "geoid")

acs$lths_n <- rowSums(as.data.frame(acs[lths_est]), na.rm = FALSE)
acs$lths_n_moe <- sqrt(rowSums(as.data.frame(acs[lths_moe])^2, na.rm = FALSE))
acs$underemp_n <- rowSums(as.data.frame(acs[under_est]), na.rm = FALSE)
acs$underemp_n_moe <- sqrt(rowSums(as.data.frame(acs[under_moe])^2, na.rm = FALSE))

acs <- acs |>
  dplyr::transmute(
    geoid,
    name,
    acs_year = cfg$acs_year,
    total_population = B01003_001E,
    total_population_moe = B01003_001M,
    total_households = DP02_0001E,
    total_households_moe = DP02_0001M,
    renter_households = B25003_003E,
    renter_households_moe = B25003_003M,
    disability_n = S1810_C02_001E,
    disability_n_moe = S1810_C02_001M,
    disability_pct = S1810_C03_001E,
    disability_pct_moe = S1810_C03_001M,
    amb65_n = S1810_C02_052E,
    amb65_n_moe = S1810_C02_052M,
    amb65_pct = S1810_C03_052E,
    amb65_pct_moe = S1810_C03_052M,
    pop25_n = B15003_001E,
    pop25_moe = B15003_001M,
    lths_n,
    lths_n_moe,
    lths_pct = 100 * lths_n / pop25_n,
    lths_pct_moe = ratio_moe(lths_n, lths_n_moe, pop25_n, pop25_moe),
    med_hh_income = B19013_001E,
    med_hh_income_moe = B19013_001M,
    no_internet_n = B28002_013E,
    no_internet_n_moe = B28002_013M,
    hh_total_internet = B28002_001E,
    hh_total_internet_moe = B28002_001M,
    no_internet_pct = 100 * no_internet_n / hh_total_internet,
    no_internet_pct_moe = ratio_moe(
      no_internet_n, no_internet_n_moe, hh_total_internet, hh_total_internet_moe
    ),
    lesh_n = S1602_C03_001E,
    lesh_n_moe = S1602_C03_001M,
    lesh_pct = S1602_C04_001E,
    lesh_pct_moe = S1602_C04_001M,
    poverty_n = S1701_C02_001E,
    poverty_n_moe = S1701_C02_001M,
    poverty_pct = S1701_C03_001E,
    poverty_pct_moe = S1701_C03_001M,
    prek_n = S1401_C01_014E,
    prek_n_moe = S1401_C01_014M,
    prek_pct = S1401_C02_014E,
    prek_pct_moe = S1401_C02_014M,
    uninsured_n = S2701_C04_001E,
    uninsured_n_moe = S2701_C04_001M,
    uninsured_pct = S2701_C05_001E,
    uninsured_pct_moe = S2701_C05_001M,
    female_hh_kids_n = DP02_0011E,
    female_hh_kids_n_moe = DP02_0011M,
    female_hh_kids_pct = DP02_0011PE,
    female_hh_kids_pct_moe = DP02_0011PM,
    hh_kids_n = DP02_0014E,
    hh_kids_n_moe = DP02_0014M,
    hh_kids_pct = DP02_0014PE,
    hh_kids_pct_moe = DP02_0014PM,
    seniors_alone_n = B11007_003E,
    seniors_alone_n_moe = B11007_003M,
    seniors_alone_pct = 100 * seniors_alone_n / total_households,
    seniors_alone_pct_moe = ratio_moe(
      seniors_alone_n, seniors_alone_n_moe, total_households, total_households_moe
    ),
    total_worked = B23022_003E + B23022_027E,
    total_worked_moe = moe_sum(B23022_003M, B23022_027M),
    underemp_n,
    underemp_n_moe,
    underemp_pct = 100 * underemp_n / total_worked,
    underemp_pct_moe = ratio_moe(
      underemp_n, underemp_n_moe, total_worked, total_worked_moe
    )
  )

acs$hh_support_risk_score <- rowMeans(
  cbind(
    dplyr::percent_rank(acs$female_hh_kids_pct),
    dplyr::percent_rank(acs$hh_kids_pct),
    dplyr::percent_rank(acs$seniors_alone_pct)
  ),
  na.rm = FALSE
) * 100

assert_unique(acs, "geoid", "ACS lever table")
write_csv_stable(acs, "data/processed/acs_levers.csv")

# Compare independently constructed fields with the public raw table. This is
# a validation artifact only; the public values are not fed back into ACS.
public <- readr::read_csv(
  "data/raw/lemi_public_api.csv",
  col_types = readr::cols(.default = readr::col_character())
)
comparison_fields <- intersect(
  names(acs),
  c(
    "total_population", "total_households", "disability_pct", "amb65_pct",
    "lths_pct", "med_hh_income", "no_internet_pct", "lesh_pct", "poverty_pct",
    "prek_pct", "uninsured_pct", "female_hh_kids_pct", "hh_kids_pct",
    "seniors_alone_pct", "underemp_pct", "hh_support_risk_score"
  )
)

validation <- purrr::map_dfr(comparison_fields, function(field) {
  independent <- acs[[field]]
  published <- clean_census_numeric(public[[field]][match(acs$geoid, public$geoid)])
  tibble::tibble(
    field,
    n_compared = sum(is.finite(independent) & is.finite(published)),
    max_abs_difference = max(abs(independent - published), na.rm = TRUE),
    mean_abs_difference = mean(abs(independent - published), na.rm = TRUE)
  )
})
write_csv_stable(validation, "output/diagnostics/acs_public_validation.csv")

message("ACS construction complete for ", nrow(acs), " tracts.")
