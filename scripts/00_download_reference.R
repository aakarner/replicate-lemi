source("R/utils.R")
check_packages()
ensure_directories()

cfg <- read_config()

message("Downloading the public Socrata reference table...")
austin_headers <- list()
if (nzchar(Sys.getenv("AUSTIN_APP_TOKEN"))) {
  austin_headers[["X-App-Token"]] <- Sys.getenv("AUSTIN_APP_TOKEN")
}

public <- download_json(
  "https://data.austintexas.gov/resource/8fgt-7i4v.json",
  query = list(`$limit` = 500),
  headers = austin_headers
)
public <- tibble::as_tibble(public)
stopifnot(nrow(public) == 249L)
assert_unique(public, "geoid", "public LEMI table")
write_csv_stable(public, "data/raw/lemi_public_api.csv")

message("Downloading the public ArcGIS tract layer and intermediate scores...")
arcgis_request <- httr2::request(
  "https://services8.arcgis.com/Kd4FT27vDhoaJjLA/arcgis/rest/services/final_lemi_042126/FeatureServer/0/query"
) |>
  httr2::req_url_query(
    f = "geojson",
    where = "1=1",
    outFields = "*",
    outSR = 4326
  ) |>
  httr2::req_retry(max_tries = 4)

arcgis_response <- httr2::req_perform(arcgis_request)
writeBin(httr2::resp_body_raw(arcgis_response), "data/raw/lemi_public_arcgis.geojson")

tracts <- sf::st_read("data/raw/lemi_public_arcgis.geojson", quiet = TRUE)
stopifnot(nrow(tracts) == 249L)
tracts$geoid <- tracts$GEOID
assert_unique(tracts, "geoid", "public ArcGIS tract layer")

# The hosted service contains hundreds of repeated spatial-join fields whose
# long names collide when written through GDAL. Preserve only the geometry and
# the fields needed for study-area selection and the overall-score diagnosis;
# the complete service response remains in the raw GeoJSON snapshot.
tracts <- tracts |>
  dplyr::select(
    geoid, NAME, Join_Count, TARGET_FID, INDEX_RAW, INDEX_PCTL,
    INDEX_PCTL_1, INDEX_PCTL_1_2, INDEX_PCTL_1_2_3
  )

sf::st_write(
  tracts,
  "data/processed/lemi_study_tracts.gpkg",
  layer = "tracts",
  delete_dsn = TRUE,
  quiet = TRUE
)

study_area <- tracts |>
  sf::st_drop_geometry() |>
  dplyr::transmute(geoid, name = NAME)
write_csv_stable(study_area, "data/processed/study_area_geoids.csv")

message("Reference downloads complete: 249 unique tract GEOIDs.")
