source("R/utils.R")
check_packages()
ensure_directories()

# PURPOSE ---------------------------------------------------------------------
# Reconstruct the documented score from the public lever percentiles, then test
# whether each published ArcGIS score came from the target GEOID or from one of
# its touching neighbors. This diagnostic never feeds the corrected index.
public <- readr::read_csv(
  "data/raw/lemi_public_api.csv",
  col_types = readr::cols(.default = readr::col_character())
)
map_layer <- sf::st_read("data/processed/lemi_study_tracts.gpkg", quiet = TRUE)

published_lever_fields <- c(
  "med_hh_income_pctl", "prek_pct_pctl", "le_2020_normalized_pctl",
  "disability_pct_reversed_pctl", "amb65_pct_reversed_pctl",
  "lths_pct_reversed_pctl", "no_internet_pct_reversed",
  "lesh_pct_reversed_pctl", "poverty_pct_reversed_pctl",
  "uninsured_pct_reversed_pctl", "underemp_pct_reversed_pctl",
  "hh_support_risk_score_reversed", "temperature_diff_reversed",
  "gq_institutional_pct_reversed", "energy_burden_reversed_pctl",
  "evict_fil_rate_reversed_pctl", "low_phys_activity_reversed",
  "pers_pov_reversed_pctl"
)

# Recreate the two documented overall-score steps using the public lever fields:
# (1) mean the 18 favorable percentiles; (2) percentile-rank that mean.
for (field in published_lever_fields) public[[field]] <- clean_census_numeric(public[[field]])
public$published_lemi_pctl <- clean_census_numeric(public$lemi_pctl)
public$intended_raw_mean <- rowMeans(public[published_lever_fields])
public$intended_lemi_pctl <- dplyr::percent_rank(public$intended_raw_mean) * 100

audit <- public |>
  dplyr::transmute(
    geoid,
    intended_raw_mean,
    intended_lemi_pctl,
    published_lemi_pctl,
    score_difference = published_lemi_pctl - intended_lemi_pctl,
    exact_score_match = abs(score_difference) < 1e-7
  )

map_layer <- map_layer |>
  dplyr::left_join(
    audit |> dplyr::select(geoid, intended_raw_mean, intended_lemi_pctl),
    by = "geoid"
  )

# Polygon INTERSECT returns the tract itself plus every tract sharing an edge or
# vertex. If attributes were joined this way, multiple candidates exist even
# though both layers represent the same 249 GEOIDs.
touching <- sf::st_intersects(map_layer)
public_raw <- map_layer$INDEX_RAW

candidate_sources <- lapply(seq_len(nrow(map_layer)), function(i) {
  # Search only within the target's INTERSECT candidate set for a raw mean that
  # exactly reproduces the hosted INDEX_RAW value.
  neighbor_ids <- touching[[i]]
  neighbor_ids[abs(map_layer$intended_raw_mean[neighbor_ids] - public_raw[i]) < 1e-7]
})

source_index <- vapply(seq_len(nrow(map_layer)), function(i) {
  candidates <- candidate_sources[[i]]
  if (!length(candidates)) return(NA_integer_)
  # Prefer a candidate whose correct percentile also matches the published
  # percentile; this resolves the few duplicated raw means deterministically.
  exact_pctl <- candidates[
    abs(map_layer$intended_lemi_pctl[candidates] - map_layer$INDEX_PCTL[i]) < 1e-7
  ]
  if (length(exact_pctl)) exact_pctl[1] else candidates[1]
}, integer(1))

spatial_join_audit <- tibble::tibble(
  # One row per target tract records the candidate source selected by the
  # reconstruction and whether its raw and percentile values reproduce ArcGIS.
  target_geoid = map_layer$geoid,
  touching_polygon_count = lengths(touching),
  arcgis_join_count = map_layer$Join_Count,
  published_index_raw = map_layer$INDEX_RAW,
  target_intended_raw = map_layer$intended_raw_mean,
  selected_source_geoid = ifelse(
    is.na(source_index), NA_character_, map_layer$geoid[source_index]
  ),
  selected_source_intended_raw = ifelse(
    is.na(source_index), NA_real_, map_layer$intended_raw_mean[source_index]
  ),
  reproduced_index_pctl = ifelse(
    is.na(source_index), NA_real_, map_layer$intended_lemi_pctl[source_index]
  ),
  source_is_target = map_layer$geoid == ifelse(
    is.na(source_index), NA_character_, map_layer$geoid[source_index]
  ),
  selected_source_touches_target = !is.na(source_index),
  raw_value_reproduced = abs(
    map_layer$INDEX_RAW - ifelse(
      is.na(source_index), NA_real_, map_layer$intended_raw_mean[source_index]
    )
  ) < 1e-7,
  published_percentile_reproduced = abs(
    map_layer$INDEX_PCTL - reproduced_index_pctl
  ) < 1e-7
)

audit <- audit |>
  dplyr::left_join(
    spatial_join_audit |>
      dplyr::select(
        target_geoid, selected_source_geoid, source_is_target,
        raw_value_reproduced, published_percentile_reproduced
      ),
    by = c("geoid" = "target_geoid")
  )

write_csv_stable(audit, "output/diagnostics/public_score_mismatches.csv")
write_csv_stable(spatial_join_audit, "output/diagnostics/arcgis_spatial_join_audit.csv")

# Aggregate the tract-level evidence into a short report suitable for sending
# to the City along with the detailed CSVs.
n <- nrow(spatial_join_audit)
n_wrong_source <- sum(!spatial_join_audit$source_is_target, na.rm = TRUE)
n_raw_reproduced <- sum(spatial_join_audit$raw_value_reproduced, na.rm = TRUE)
n_pctl_reproduced <- sum(spatial_join_audit$published_percentile_reproduced, na.rm = TRUE)
n_intended_matches <- sum(audit$exact_score_match)
n_join_count_matches <- sum(
  spatial_join_audit$arcgis_join_count == spatial_join_audit$touching_polygon_count,
  na.rm = TRUE
)

report <- c(
  "# Diagnosis of the public LEMI score mismatch",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
  "",
  "## Finding",
  "",
  paste0(
    "The 18 public lever percentile fields reproduce the documented raw mean, but only ",
    n_intended_matches, " of ", n, " published overall scores remain attached to the correct GEOID."
  ),
  "",
  paste0(
    "Every public `INDEX_RAW` value (", n_raw_reproduced, " of ", n,
    ") exactly equals the documented raw mean of either the target tract or a polygon that touches it."
  ),
  "",
  paste0(
    "For ", n_wrong_source, " of ", n,
    " tracts, the selected source is a touching neighbor rather than the target tract. "
  ),
  "",
  paste0(
    "Using that selected neighbor also reproduces ", n_pctl_reproduced, " of ", n,
    " published overall percentile scores. The remaining cases are tied raw means."
  ),
  "",
  "## Likely cause",
  "",
  paste0(
    "For ", n_join_count_matches, " of ", n,
    " tracts, ArcGIS `Join_Count` exactly equals the number of tract polygons that touch or overlap the target."
  ),
  "",
  paste(
    "The hosted feature layer contains repeated `Join_Count` and `TARGET_FID` fields.",
    "The evidence is consistent with an ArcGIS polygon-to-polygon spatial join using",
    "INTERSECT and the default first-match behavior. Census tract polygons intersect",
    "their neighbors along shared boundaries, so the first intersecting feature is",
    "often not the tract itself."
  ),
  "",
  "## Correction",
  "",
  paste(
    "Rebuild the final feature layer by joining every raw, lever, sub-theme, theme,",
    "and overall table to the 2020 tract layer on the 11-digit GEOID. Enforce a",
    "one-to-one key assertion before publishing. Do not use a polygon INTERSECT join",
    "to attach attributes to identical tract geographies."
  )
)
writeLines(report, "output/diagnostics/public_data_diagnosis.md")

message(
  "Public score diagnosis complete: ", n_wrong_source, " of ", n,
  " tracts receive an overall raw score from a touching neighbor."
)
