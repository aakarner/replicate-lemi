options(stringsAsFactors = FALSE)

# Keep optional project-local packages ahead of the user and system libraries.
# R/library is ignored by Git, so collaborators can install packages locally
# without committing platform-specific binaries.
local_library <- file.path(getwd(), "R", "library")
if (dir.exists(local_library)) .libPaths(c(local_library, .libPaths()))

# Packages used by nearly every stage. Scripts can request additional packages
# (for example, mice or leaflet) through check_packages(extra = ...).
required_packages <- c(
  "dplyr", "tidyr", "readr", "purrr", "sf", "httr2", "jsonlite",
  "yaml", "ggplot2", "scales"
)

check_packages <- function(extra = character()) {
  packages <- unique(c(required_packages, extra))
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop(
      "Missing R package(s): ", paste(missing, collapse = ", "),
      ". Install them before running the pipeline.", call. = FALSE
    )
  }
}

# All scripts assume they are launched from the repository root. Keeping this
# helper makes that convention explicit for future path validation or logging.
project_root <- function() {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

read_config <- function(path = "config.yml") {
  check_packages()
  yaml::read_yaml(path)
}

# Generated and downloaded artifacts are intentionally separated from tracked
# source files. These folders are safe to rebuild and are ignored by Git.
ensure_directories <- function() {
  dirs <- c("data/raw", "data/processed", "output", "output/diagnostics")
  invisible(vapply(dirs, dir.create, logical(1), recursive = TRUE, showWarnings = FALSE))
}

# GEOID joins must always be one-to-one. This guard prevents the row-order and
# many-to-many join problems that appear to have affected the public map.
assert_unique <- function(data, key, label = deparse(substitute(data))) {
  duplicated_key <- duplicated(data[[key]]) | is.na(data[[key]])
  if (any(duplicated_key)) {
    stop(label, " does not have a complete unique `", key, "` key.", call. = FALSE)
  }
  invisible(data)
}

# Use a consistent blank representation for missing values across all outputs.
write_csv_stable <- function(data, path) {
  readr::write_csv(data, path, na = "")
  invisible(path)
}

# Shared JSON downloader with bounded retries for transient government API
# failures. Query parameters and headers are supplied as named lists.
download_json <- function(url, query = list(), headers = list()) {
  request <- httr2::request(url)
  if (length(query)) request <- do.call(httr2::req_url_query, c(list(request), query))
  if (length(headers)) request <- do.call(httr2::req_headers, c(list(request), headers))
  response <- httr2::req_retry(request, max_tries = 4) |> httr2::req_perform()
  httr2::resp_body_json(response, simplifyVector = TRUE)
}

# Census APIs encode suppressed or unavailable estimates as large negative
# sentinel values. Convert those sentinels to ordinary R missing values before
# deriving percentages or passing fields into MICE.
clean_census_numeric <- function(x) {
  value <- suppressWarnings(as.numeric(x))
  value[value < -1e6] <- NA_real_
  value
}

# The ACS approximation for the MOE of a sum is the square root of the sum of
# squared component MOEs (assuming the published component covariance is not
# available).
moe_sum <- function(...) {
  values <- cbind(...)
  sqrt(rowSums(values^2, na.rm = FALSE))
}

# Census ratio-MOE calculation. Subtraction is used when the numerator is a
# subset of the denominator; the addition fallback handles cases where rounding
# produces a small negative value inside the square root.
ratio_moe <- function(numerator, numerator_moe, denominator, denominator_moe) {
  ratio <- numerator / denominator
  inside <- numerator_moe^2 - (ratio^2 * denominator_moe^2)
  fallback <- numerator_moe^2 + (ratio^2 * denominator_moe^2)
  100 / denominator * sqrt(ifelse(inside >= 0, inside, fallback))
}

# Convert a raw lever to a 0-1 rank in the common favorable direction. Negating
# burden variables before percent_rank means every resulting lever is oriented
# so that a larger value represents more favorable structural conditions.
percent_rank_favorable <- function(x, favorable_when = c("high", "low")) {
  favorable_when <- match.arg(favorable_when)
  if (favorable_when == "low") x <- -x
  dplyr::percent_rank(x)
}

# The report applies percentile ranking twice: once to individual levers and
# again to the mean used for each sub-theme, theme, and overall score.
row_percentile_mean <- function(data, columns) {
  raw_mean <- rowMeans(as.data.frame(data[columns]), na.rm = FALSE)
  dplyr::percent_rank(raw_mean) * 100
}

# Convenience helper for source-tracking tables retained for future expansion.
with_provenance <- function(data, source_name, independently_reproduced) {
  data$source_name <- source_name
  data$independently_reproduced <- independently_reproduced
  data
}
