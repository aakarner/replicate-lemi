source("R/utils.R")
check_packages()
ensure_directories()

cfg <- read_config()
input_path <- "data/eviction_filings_full_geocoded_hex.csv"
if (!file.exists(input_path)) stop("Missing ", input_path, call. = FALSE)

tracts <- sf::st_read("data/processed/lemi_study_tracts.gpkg", quiet = TRUE) |>
  dplyr::select(geoid)
acs <- readr::read_csv(
  "data/processed/acs_levers.csv",
  col_types = readr::cols(geoid = readr::col_character()),
  show_col_types = FALSE
)

filings <- readr::read_csv(
  input_path,
  col_types = readr::cols(
    .default = readr::col_character(),
    filing_year = readr::col_integer(),
    longitude = readr::col_double(),
    latitude = readr::col_double(),
    geocode_score = readr::col_double(),
    geocoded = readr::col_logical(),
    reliable_geocode = readr::col_logical(),
    duplicate_case_defendant = readr::col_logical()
  )
) |>
  dplyr::filter(filing_year == cfg$eviction_year) |>
  dplyr::mutate(
    case_key = paste(jp_district, case_number, sep = "|"),
    coordinate_key = ifelse(
      is.finite(longitude) & is.finite(latitude),
      sprintf("%.7f|%.7f", longitude, latitude),
      NA_character_
    )
  )

# A filing is a court case, not a defendant row. For cases with multiple
# defendant addresses, use the most frequently occurring coordinate; break
# ties in favor of a reliable, high-scoring geocode. Preserve ambiguity counts
# for the diagnostic output.
case_coordinate_counts <- filings |>
  dplyr::group_by(case_key, coordinate_key, longitude, latitude, .drop = FALSE) |>
  dplyr::summarise(
    defendant_rows_at_coordinate = dplyr::n(),
    reliable_geocode = any(reliable_geocode %in% TRUE),
    geocode_score = max(geocode_score, na.rm = TRUE),
    .groups = "drop"
  )

case_diagnostics <- filings |>
  dplyr::group_by(case_key) |>
  dplyr::summarise(
    court = dplyr::first(court),
    jp_district = dplyr::first(jp_district),
    case_number = dplyr::first(case_number),
    file_date = dplyr::first(file_date),
    defendant_rows = dplyr::n(),
    distinct_coordinates = dplyr::n_distinct(coordinate_key, na.rm = TRUE),
    .groups = "drop"
  )

cases <- case_coordinate_counts |>
  dplyr::arrange(
    case_key,
    dplyr::desc(defendant_rows_at_coordinate),
    dplyr::desc(reliable_geocode),
    dplyr::desc(geocode_score)
  ) |>
  dplyr::group_by(case_key) |>
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  dplyr::left_join(case_diagnostics, by = "case_key")

case_points <- cases |>
  dplyr::filter(is.finite(longitude), is.finite(latitude)) |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

case_points <- sf::st_transform(case_points, sf::st_crs(tracts))
case_points <- sf::st_join(case_points, tracts, join = sf::st_within, left = TRUE)

tract_counts <- case_points |>
  sf::st_drop_geometry() |>
  dplyr::filter(!is.na(geoid)) |>
  dplyr::count(geoid, name = "eviction_filings")

eviction_rates <- acs |>
  dplyr::select(geoid, renter_households, renter_households_moe) |>
  dplyr::left_join(tract_counts, by = "geoid") |>
  dplyr::mutate(
    source_county_fips = substr(geoid, 3, 5),
    eviction_filings = dplyr::if_else(
      source_county_fips == cfg$county_fips$Travis,
      tidyr::replace_na(eviction_filings, 0L),
      NA_integer_
    ),
    evict_fil_rate = 100 * eviction_filings / renter_households,
    eviction_source = dplyr::if_else(
      source_county_fips == cfg$county_fips$Travis,
      "Travis County JP case-level data; unique court cases filed in 2024",
      "Not observed; source file covers Travis County only"
    )
  ) |>
  dplyr::select(
    geoid, eviction_filings, renter_households, renter_households_moe,
    evict_fil_rate, eviction_source
  )

assert_unique(eviction_rates, "geoid", "eviction filing rates")
write_csv_stable(eviction_rates, "data/processed/eviction_rates_2024.csv")

case_output <- case_points |>
  sf::st_drop_geometry() |>
  dplyr::select(
    case_key, court, jp_district, case_number, file_date, longitude, latitude,
    reliable_geocode, geocode_score, defendant_rows, distinct_coordinates, geoid
  )
write_csv_stable(case_output, "output/diagnostics/eviction_case_assignment.csv")

summary <- tibble::tibble(
  metric = c(
    "2024 defendant rows", "2024 unique court cases", "cases with multiple coordinates",
    "case representatives with reliable geocode", "cases assigned to a LEMI tract",
    "cases outside the LEMI tract universe"
  ),
  value = c(
    nrow(filings), nrow(cases), sum(cases$distinct_coordinates > 1),
    sum(cases$reliable_geocode %in% TRUE), sum(!is.na(case_points$geoid)),
    sum(is.na(case_points$geoid))
  )
)
write_csv_stable(summary, "output/diagnostics/eviction_summary.csv")

message(
  "Eviction construction complete: ", nrow(cases), " unique 2024 cases; ",
  sum(!is.na(case_points$geoid)), " assigned to the LEMI tract universe."
)
