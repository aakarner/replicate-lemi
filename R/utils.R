options(stringsAsFactors = FALSE)

local_library <- file.path(getwd(), "R", "library")
if (dir.exists(local_library)) .libPaths(c(local_library, .libPaths()))

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

project_root <- function() {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

read_config <- function(path = "config.yml") {
  check_packages()
  yaml::read_yaml(path)
}

ensure_directories <- function() {
  dirs <- c("data/raw", "data/processed", "output", "output/diagnostics")
  invisible(vapply(dirs, dir.create, logical(1), recursive = TRUE, showWarnings = FALSE))
}

assert_unique <- function(data, key, label = deparse(substitute(data))) {
  duplicated_key <- duplicated(data[[key]]) | is.na(data[[key]])
  if (any(duplicated_key)) {
    stop(label, " does not have a complete unique `", key, "` key.", call. = FALSE)
  }
  invisible(data)
}

write_csv_stable <- function(data, path) {
  readr::write_csv(data, path, na = "")
  invisible(path)
}

download_json <- function(url, query = list(), headers = list()) {
  request <- httr2::request(url)
  if (length(query)) request <- do.call(httr2::req_url_query, c(list(request), query))
  if (length(headers)) request <- do.call(httr2::req_headers, c(list(request), headers))
  response <- httr2::req_retry(request, max_tries = 4) |> httr2::req_perform()
  httr2::resp_body_json(response, simplifyVector = TRUE)
}

clean_census_numeric <- function(x) {
  value <- suppressWarnings(as.numeric(x))
  value[value < -1e6] <- NA_real_
  value
}

moe_sum <- function(...) {
  values <- cbind(...)
  sqrt(rowSums(values^2, na.rm = FALSE))
}

ratio_moe <- function(numerator, numerator_moe, denominator, denominator_moe) {
  ratio <- numerator / denominator
  inside <- numerator_moe^2 - (ratio^2 * denominator_moe^2)
  fallback <- numerator_moe^2 + (ratio^2 * denominator_moe^2)
  100 / denominator * sqrt(ifelse(inside >= 0, inside, fallback))
}

percent_rank_favorable <- function(x, favorable_when = c("high", "low")) {
  favorable_when <- match.arg(favorable_when)
  if (favorable_when == "low") x <- -x
  dplyr::percent_rank(x)
}

row_percentile_mean <- function(data, columns) {
  raw_mean <- rowMeans(as.data.frame(data[columns]), na.rm = FALSE)
  dplyr::percent_rank(raw_mean) * 100
}

with_provenance <- function(data, source_name, independently_reproduced) {
  data$source_name <- source_name
  data$independently_reproduced <- independently_reproduced
  data
}
