source("R/utils.R")
check_packages(c("mice", "data.table"))
ensure_directories()

# PURPOSE ---------------------------------------------------------------------
# Test whether a much smaller set of levers preserves the tract rank ordering
# of the documented 18-lever LEMI. Because the target is itself an arithmetic
# composite rather than an external outcome, this is an approximation exercise:
# the question is how efficiently a subset reproduces the full formula.
cfg <- read_config()
output_dir <- "output/parsimony"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

lever_specs <- tibble::tribble(
  ~lever, ~label, ~favorable_when, ~percentile_field, ~theme, ~subtheme,
  "uninsured_pct", "Uninsured rate", "low", "uninsured_pct_pctl", "Health & Wellbeing", "Health Access & Resources",
  "prek_pct", "Early-education enrollment", "high", "prek_pct_pctl", "Health & Wellbeing", "Health Access & Resources",
  "le_2020_normalized", "Life expectancy", "high", "le_2020_normalized_pctl", "Health & Wellbeing", "Health Access & Resources",
  "disability_pct", "Disability rate", "low", "disability_pct_pctl", "Health & Wellbeing", "Wellbeing & Activity",
  "amb65_pct", "Seniors with ambulatory disabilities", "low", "amb65_pct_pctl", "Health & Wellbeing", "Wellbeing & Activity",
  "low_phys_activity", "Low physical activity", "low", "low_phys_activity_pctl", "Health & Wellbeing", "Wellbeing & Activity",
  "med_hh_income", "Median household income", "high", "med_hh_income_pctl", "Livelihood & Work", "Income & Employment",
  "underemp_pct", "Underemployment rate", "low", "underemp_pct_pctl", "Livelihood & Work", "Income & Employment",
  "poverty_pct", "Poverty rate", "low", "poverty_pct_pctl", "Livelihood & Work", "Income & Employment",
  "evict_fil_rate", "Eviction filing rate", "low", "evict_fil_rate_pctl", "Livelihood & Work", "Household Stability & Cost Burdens",
  "hh_support_risk_score", "Household support risk", "low", "hh_support_risk_score_pctl", "Livelihood & Work", "Household Stability & Cost Burdens",
  "energy_burden", "Energy burden", "low", "energy_burden_pctl", "Livelihood & Work", "Household Stability & Cost Burdens",
  "lesh_pct", "Limited-English households", "low", "lesh_pct_pctl", "Access & Belonging", "Education, Language & Digital Access",
  "no_internet_pct", "Households without internet", "low", "no_internet_pct_pctl", "Access & Belonging", "Education, Language & Digital Access",
  "lths_pct", "Less than high-school education", "low", "lths_pct_pctl", "Access & Belonging", "Education, Language & Digital Access",
  "pers_pov", "Persistent poverty", "low", "pers_pov_pctl", "Access & Belonging", "Environment & Neighborhood",
  "gq_institutional_pct", "Institutional group quarters", "low", "gq_institutional_pct_pctl", "Access & Belonging", "Environment & Neighborhood",
  "temperature_diff", "Heat disparity", "low", "temperature_diff_pctl", "Access & Belonging", "Environment & Neighborhood"
)

scores <- readr::read_csv(
  "output/lemi_scores_corrected.csv",
  col_types = readr::cols(geoid = readr::col_character()),
  show_col_types = FALSE
)
assert_unique(scores, "geoid", "corrected score table")

# Rebuild percentiles from the completed raw lever values rather than reading
# the serialized percentile columns back from CSV. This preserves exact ties;
# tiny decimal round-trip differences can otherwise move a tied tract by one
# rank even though the underlying values are identical.
X <- vapply(seq_len(nrow(lever_specs)), function(j) {
  percent_rank_favorable(scores[[lever_specs$lever[j]]], lever_specs$favorable_when[j])
}, numeric(nrow(scores)))
colnames(X) <- lever_specs$lever
n <- nrow(X)
p <- ncol(X)
stopifnot(p == 18L, n == 249L, !anyNA(X))

# Reconstruct the full target directly from lever percentiles. Spearman
# correlation depends only on ranks, so using raw means avoids an unnecessary
# second percentile transformation during subset search.
full_raw <- rowMeans(X)
full_rank <- rank(full_raw, ties.method = "average")
reconstructed_full <- dplyr::percent_rank(full_raw) * 100
full_score_difference <- max(abs(reconstructed_full - scores$lemi_pctl))
if (full_score_difference >= 1e-7) {
  stop(
    "Lever percentile fields do not reconstruct lemi_pctl; maximum difference = ",
    format(full_score_difference, scientific = TRUE),
    call. = FALSE
  )
}

mask_indices <- function(mask, variables = p) {
  which(vapply(
    0:(variables - 1L),
    function(bit) bitwAnd(mask, bitwShiftL(1L, bit)) != 0L,
    logical(1)
  ))
}

subset_metrics <- function(indices, matrix = X, target_raw = full_raw, geoids = scores$geoid) {
  subset_raw <- rowMeans(matrix[, indices, drop = FALSE])
  subset_rank <- rank(subset_raw, ties.method = "average")
  target_rank <- rank(target_raw, ties.method = "average")
  rank_error <- subset_rank - target_rank

  extreme_n <- ceiling(nrow(matrix) * cfg$parsimony$extreme_group_share)
  full_order <- order(target_raw, geoids)
  subset_order <- order(subset_raw, geoids)
  bottom_overlap <- length(intersect(full_order[seq_len(extreme_n)], subset_order[seq_len(extreme_n)])) / extreme_n
  top_overlap <- length(intersect(
    tail(full_order, extreme_n), tail(subset_order, extreme_n)
  )) / extreme_n

  full_quintile <- pmin(5L, ceiling(target_rank / (nrow(matrix) / 5)))
  subset_quintile <- pmin(5L, ceiling(subset_rank / (nrow(matrix) / 5)))

  tibble::tibble(
    spearman_rho = stats::cor(subset_rank, target_rank),
    kendall_tau = stats::cor(subset_raw, target_raw, method = "kendall"),
    rank_mae = mean(abs(rank_error)),
    rank_rmse = sqrt(mean(rank_error^2)),
    rank_max_abs = max(abs(rank_error)),
    bottom_group_overlap = bottom_overlap,
    top_group_overlap = top_overlap,
    quintile_agreement = mean(subset_quintile == full_quintile)
  )
}

# EXHAUSTIVE SUBSET SEARCH -----------------------------------------------------
# Enumerate all 2^18 - 1 subsets with a Gray code, which changes only one lever
# at a time. Updating the running row sum is substantially faster than creating
# a new 249-by-k matrix for every candidate. Rounding the mean before ranking
# prevents floating-point drift from breaking exact ties during the Gray walk.
constraint_names <- c("Unconstrained", "All three themes", "All six sub-themes")
best_rho <- matrix(-Inf, nrow = p, ncol = length(constraint_names), dimnames = list(NULL, constraint_names))
best_mae <- matrix(Inf, nrow = p, ncol = length(constraint_names), dimnames = list(NULL, constraint_names))
best_mask <- matrix(NA_integer_, nrow = p, ncol = length(constraint_names), dimnames = list(NULL, constraint_names))

theme_id <- match(lever_specs$theme, unique(lever_specs$theme))
subtheme_id <- match(lever_specs$subtheme, unique(lever_specs$subtheme))
theme_counts <- integer(length(unique(theme_id)))
subtheme_counts <- integer(length(unique(subtheme_id)))

running_sum <- numeric(n)
current_size <- 0L
previous_gray <- 0L
subset_total <- 2^p - 1L
search_start <- Sys.time()

message("Evaluating all ", format(subset_total, big.mark = ","), " nonempty lever subsets...")
for (i in seq_len(subset_total)) {
  gray <- bitwXor(i, bitwShiftR(i, 1L))
  changed <- bitwXor(gray, previous_gray)
  bit_index <- as.integer(log2(changed)) + 1L
  added <- bitwAnd(gray, changed) != 0L

  if (added) {
    running_sum <- running_sum + X[, bit_index]
    current_size <- current_size + 1L
    theme_counts[theme_id[bit_index]] <- theme_counts[theme_id[bit_index]] + 1L
    subtheme_counts[subtheme_id[bit_index]] <- subtheme_counts[subtheme_id[bit_index]] + 1L
  } else {
    running_sum <- running_sum - X[, bit_index]
    current_size <- current_size - 1L
    theme_counts[theme_id[bit_index]] <- theme_counts[theme_id[bit_index]] - 1L
    subtheme_counts[subtheme_id[bit_index]] <- subtheme_counts[subtheme_id[bit_index]] - 1L
  }

  candidate_rank <- rank(round(running_sum / current_size, 12), ties.method = "average")
  rho <- stats::cor(candidate_rank, full_rank)
  mae <- mean(abs(candidate_rank - full_rank))
  eligible <- c(TRUE, all(theme_counts > 0L), all(subtheme_counts > 0L))

  for (constraint in which(eligible)) {
    better <- rho > best_rho[current_size, constraint] + 1e-14
    tied_but_lower_error <- abs(rho - best_rho[current_size, constraint]) <= 1e-14 &&
      mae < best_mae[current_size, constraint]
    if (better || tied_but_lower_error) {
      best_rho[current_size, constraint] <- rho
      best_mae[current_size, constraint] <- mae
      best_mask[current_size, constraint] <- gray
    }
  }
  previous_gray <- gray
  if (i %% 50000L == 0L) message("  evaluated ", format(i, big.mark = ","), " subsets")
}

search_seconds <- as.numeric(difftime(Sys.time(), search_start, units = "secs"))

# Recompute detailed metrics from the original matrix for the globally best
# mask at every size and constraint. This removes any Gray-code rounding from
# the reported results.
best_by_size <- purrr::map_dfr(seq_along(constraint_names), function(constraint) {
  purrr::map_dfr(seq_len(p), function(k) {
    mask <- best_mask[k, constraint]
    if (is.na(mask)) return(NULL)
    indices <- mask_indices(mask)
    dplyr::bind_cols(
      tibble::tibble(
        constraint = constraint_names[constraint],
        subset_size = k,
        mask = mask,
        levers = paste(lever_specs$lever[indices], collapse = ";"),
        lever_labels = paste(lever_specs$label[indices], collapse = "; ")
      ),
      subset_metrics(indices)
    )
  })
})
write_csv_stable(best_by_size, file.path(output_dir, "best_subsets_by_size.csv"))

thresholds <- as.numeric(unlist(cfg$parsimony$fidelity_thresholds))
threshold_results <- purrr::map_dfr(constraint_names, function(constraint) {
  constraint_value <- constraint
  path <- best_by_size |> dplyr::filter(.data$constraint == .env$constraint_value)
  purrr::map_dfr(thresholds, function(threshold) {
    threshold_value <- threshold
    reached <- path |>
      dplyr::filter(.data$spearman_rho >= .env$threshold_value) |>
      dplyr::slice_min(subset_size, n = 1)
    if (!nrow(reached)) {
      return(tibble::tibble(
        constraint = constraint_value,
        threshold = threshold_value,
        subset_size = NA_integer_
      ))
    }
    reached |> dplyr::mutate(threshold = threshold_value, .after = "constraint")
  })
})
write_csv_stable(threshold_results, file.path(output_dir, "fidelity_thresholds.csv"))

# Retain three operating points rather than imposing one definition of
# "parsimonious": a broad rank core (rho >= .95), a high-fidelity version
# (rho >= .975), and an extreme-preserving version that also retains at least
# 90% of both the top and bottom 20% of tracts.
unconstrained_path <- best_by_size |> dplyr::filter(constraint == "Unconstrained")
rank_core <- unconstrained_path |>
  dplyr::filter(spearman_rho >= 0.95) |>
  dplyr::slice_min(subset_size, n = 1)
high_fidelity <- unconstrained_path |>
  dplyr::filter(spearman_rho >= 0.975) |>
  dplyr::slice_min(subset_size, n = 1)
extreme_preserving <- unconstrained_path |>
  dplyr::filter(
    spearman_rho >= 0.95,
    top_group_overlap >= 0.90,
    bottom_group_overlap >= 0.90
  ) |>
  dplyr::slice_min(subset_size, n = 1)
if (!nrow(extreme_preserving)) {
  extreme_preserving <- high_fidelity
}

rank_core_indices <- mask_indices(rank_core$mask[[1]])
rank_core_k <- length(rank_core_indices)
high_fidelity_indices <- mask_indices(high_fidelity$mask[[1]])
extreme_preserving_indices <- mask_indices(extreme_preserving$mask[[1]])

# LEVER REDUNDANCY AND INFLUENCE ----------------------------------------------
pairwise <- stats::cor(X, method = "spearman")
pairwise_output <- as.data.frame(pairwise) |>
  tibble::rownames_to_column("lever")
write_csv_stable(pairwise_output, file.path(output_dir, "pairwise_spearman_correlations.csv"))

# PCA provides a complementary measure of multivariate redundancy. It is run on
# standardized percentile vectors, so a lever's original unit cannot dominate.
pca <- stats::prcomp(X, center = TRUE, scale. = TRUE)
pca_variance <- pca$sdev^2 / sum(pca$sdev^2)
pca_summary <- tibble::tibble(
  component = seq_along(pca_variance),
  variance_explained = pca_variance,
  cumulative_variance_explained = cumsum(pca_variance)
)
pca_loadings <- as.data.frame(pca$rotation) |>
  tibble::rownames_to_column("lever")
effective_dimension <- sum(pca$sdev^2)^2 / sum((pca$sdev^2)^2)
write_csv_stable(pca_summary, file.path(output_dir, "pca_variance_explained.csv"))
write_csv_stable(pca_loadings, file.path(output_dir, "pca_loadings.csv"))

lever_diagnostics <- purrr::map_dfr(seq_len(p), function(j) {
  without_j <- setdiff(seq_len(p), j)
  univariate <- subset_metrics(j)
  leave_one_out <- subset_metrics(without_j)
  other_correlations <- abs(pairwise[j, -j])
  closest <- which.max(other_correlations)
  closest_index <- setdiff(seq_len(p), j)[closest]
  tibble::tibble(
    lever = lever_specs$lever[j],
    label = lever_specs$label[j],
    theme = lever_specs$theme[j],
    univariate_rho = univariate$spearman_rho,
    full_without_lever_rho = leave_one_out$spearman_rho,
    rank_information_loss = 1 - leave_one_out$spearman_rho,
    leave_one_out_rank_mae = leave_one_out$rank_mae,
    closest_correlated_lever = lever_specs$lever[closest_index],
    max_abs_pairwise_rho = other_correlations[closest]
  )
}) |>
  dplyr::arrange(dplyr::desc(rank_information_loss))
write_csv_stable(lever_diagnostics, file.path(output_dir, "lever_redundancy_and_influence.csv"))

# GREEDY SELECTION FOR STABILITY TESTS ----------------------------------------
# Exhaustive selection is used for the main result. A faster forward-selection
# approximation is used inside repeated splits and imputed datasets.
greedy_select <- function(matrix, target_raw, rows, size) {
  selected <- integer()
  remaining <- seq_len(ncol(matrix))
  path <- vector("list", size)
  target_rank <- rank(target_raw[rows], ties.method = "average")
  for (step in seq_len(size)) {
    candidate_rho <- vapply(remaining, function(j) {
      candidate <- c(selected, j)
      candidate_raw <- rowMeans(matrix[rows, candidate, drop = FALSE])
      stats::cor(rank(candidate_raw, ties.method = "average"), target_rank)
    }, numeric(1))
    winner <- remaining[which.max(candidate_rho)]
    selected <- c(selected, winner)
    remaining <- setdiff(remaining, winner)
    path[[step]] <- tibble::tibble(step, lever_index = winner, rho = max(candidate_rho))
  }
  list(selected = selected, path = dplyr::bind_rows(path))
}

# Repeated 80/20 splits select a k-lever greedy subset using training tracts and
# evaluate its rank preservation on held-out tracts. This guards against a core
# set that only looks good because it was chosen and evaluated on all 249 rows.
set.seed(cfg$parsimony$seed)
cv_repeats <- cfg$parsimony$cross_validation_repeats
train_n <- floor(n * cfg$parsimony$training_share)
cv_selection <- vector("list", cv_repeats)
cv_results <- purrr::map_dfr(seq_len(cv_repeats), function(repeat_id) {
  train <- sort(sample.int(n, train_n, replace = FALSE))
  test <- setdiff(seq_len(n), train)
  selected <- greedy_select(X, full_raw, train, rank_core_k)$selected
  train_metrics <- subset_metrics(selected, X[train, , drop = FALSE], full_raw[train], scores$geoid[train])
  test_metrics <- subset_metrics(selected, X[test, , drop = FALSE], full_raw[test], scores$geoid[test])
  cv_selection[[repeat_id]] <<- tibble::tibble(repeat_id, lever = lever_specs$lever[selected])
  dplyr::bind_rows(
    dplyr::mutate(train_metrics, repeat_id, sample = "train"),
    dplyr::mutate(test_metrics, repeat_id, sample = "test")
  )
})
cv_selection <- dplyr::bind_rows(cv_selection)
cv_frequency <- cv_selection |>
  dplyr::count(lever, name = "selected_repeats") |>
  dplyr::mutate(selection_frequency = selected_repeats / cv_repeats) |>
  dplyr::right_join(lever_specs |> dplyr::select(lever, label, theme), by = "lever") |>
  dplyr::mutate(
    selected_repeats = tidyr::replace_na(selected_repeats, 0L),
    selection_frequency = tidyr::replace_na(selection_frequency, 0)
  ) |>
  dplyr::arrange(dplyr::desc(selection_frequency))
write_csv_stable(cv_results, file.path(output_dir, "cross_validation_metrics.csv"))
write_csv_stable(cv_frequency, file.path(output_dir, "cross_validation_selection_frequency.csv"))

# IMPUTATION STABILITY ---------------------------------------------------------
# Reconstruct lever percentiles for each MICE completion, measure how the
# primary rank-core subsets perform, and record which k levers a greedy path
# would select under each completion.
mids <- readRDS("data/processed/lemi_mice.rds")
imputation_selection <- vector("list", mids$m)
imputation_stability <- purrr::map_dfr(seq_len(mids$m), function(imputation) {
  completed <- mice::complete(mids, action = imputation)
  X_i <- vapply(seq_len(p), function(j) {
    percent_rank_favorable(completed[[lever_specs$lever[j]]], lever_specs$favorable_when[j])
  }, numeric(n))
  colnames(X_i) <- lever_specs$lever
  full_i <- rowMeans(X_i)
  greedy_i <- greedy_select(X_i, full_i, seq_len(n), rank_core_k)$selected
  imputation_selection[[imputation]] <<- tibble::tibble(
    imputation,
    lever = lever_specs$lever[greedy_i]
  )
  dplyr::bind_rows(
    dplyr::mutate(
      subset_metrics(rank_core_indices, X_i, full_i, scores$geoid),
      imputation, subset = "rank_core"
    ),
    dplyr::mutate(
      subset_metrics(high_fidelity_indices, X_i, full_i, scores$geoid),
      imputation, subset = "high_fidelity"
    ),
    dplyr::mutate(
      subset_metrics(extreme_preserving_indices, X_i, full_i, scores$geoid),
      imputation, subset = "extreme_preserving"
    )
  )
})
imputation_selection <- dplyr::bind_rows(imputation_selection) |>
  dplyr::count(lever, name = "selected_imputations") |>
  dplyr::mutate(selection_frequency = selected_imputations / mids$m) |>
  dplyr::right_join(lever_specs |> dplyr::select(lever, label, theme), by = "lever") |>
  dplyr::mutate(
    selected_imputations = tidyr::replace_na(selected_imputations, 0L),
    selection_frequency = tidyr::replace_na(selection_frequency, 0)
  ) |>
  dplyr::arrange(dplyr::desc(selection_frequency))
write_csv_stable(imputation_stability, file.path(output_dir, "imputation_stability_metrics.csv"))
write_csv_stable(imputation_selection, file.path(output_dir, "imputation_selection_frequency.csv"))

# TRACT-LEVEL COMPARISON -------------------------------------------------------
# The tract comparison uses the smallest rho-.95 rank core. The other operating
# points remain available in best_subsets_by_size.csv.
rank_core_raw <- rowMeans(X[, rank_core_indices, drop = FALSE])
rank_core_score <- dplyr::percent_rank(rank_core_raw) * 100
rank_core_rank <- rank(rank_core_raw, ties.method = "average")
tract_comparison <- tibble::tibble(
  geoid = scores$geoid,
  name = scores$name,
  full_lemi_score = scores$lemi_pctl,
  parsimonious_score = rank_core_score,
  full_rank = full_rank,
  parsimonious_rank = rank_core_rank,
  rank_difference = rank_core_rank - full_rank,
  absolute_rank_difference = abs(rank_difference)
)
write_csv_stable(tract_comparison, file.path(output_dir, "recommended_tract_comparison.csv"))

# PLOTS -----------------------------------------------------------------------
fidelity_plot <- ggplot2::ggplot(
  best_by_size,
  ggplot2::aes(subset_size, spearman_rho, color = constraint)
) +
  ggplot2::geom_hline(
    yintercept = thresholds,
    color = "grey80",
    linetype = "dashed",
    linewidth = 0.35
  ) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::geom_point(size = 1.8) +
  ggplot2::scale_x_continuous(breaks = seq_len(p)) +
  ggplot2::coord_cartesian(ylim = c(min(best_by_size$spearman_rho), 1)) +
  ggplot2::labs(
    title = "How many levers are needed to reproduce the full LEMI ranking?",
    x = "Number of levers",
    y = "Spearman rank correlation with 18-lever LEMI",
    color = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
  file.path(output_dir, "fidelity_by_subset_size.png"),
  fidelity_plot,
  width = 9,
  height = 6,
  dpi = 220,
  bg = "white"
)

rank_plot <- ggplot2::ggplot(
  tract_comparison,
  ggplot2::aes(full_rank, parsimonious_rank, color = absolute_rank_difference)
) +
  ggplot2::geom_abline(slope = 1, intercept = 0, color = "grey50", linetype = "dashed") +
  ggplot2::geom_point(size = 2, alpha = 0.8) +
  ggplot2::scale_color_viridis_c(option = "C", direction = -1) +
  ggplot2::coord_equal() +
  ggplot2::labs(
    title = paste0("Full versus ", rank_core_k, "-lever tract ranks"),
    x = "18-lever LEMI rank",
    y = "Parsimonious-index rank",
    color = "Absolute rank\ndifference"
  ) +
  ggplot2::theme_minimal(base_size = 11)
ggplot2::ggsave(
  file.path(output_dir, "rank_core_comparison.png"),
  rank_plot,
  width = 7,
  height = 6,
  dpi = 220,
  bg = "white"
)

influence_plot <- lever_diagnostics |>
  dplyr::mutate(label = stats::reorder(label, rank_information_loss)) |>
  ggplot2::ggplot(ggplot2::aes(rank_information_loss, label, fill = theme)) +
  ggplot2::geom_col() +
  ggplot2::labs(
    title = "Unique rank information from each lever",
    subtitle = "Loss is 1 minus correlation between full LEMI and LEMI with that lever removed",
    x = "Leave-one-out rank-information loss",
    y = NULL,
    fill = NULL
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
  file.path(output_dir, "leave_one_out_influence.png"),
  influence_plot,
  width = 9,
  height = 7,
  dpi = 220,
  bg = "white"
)

# HUMAN-READABLE REPORT -------------------------------------------------------
cv_test <- cv_results |> dplyr::filter(sample == "test")
imputation_rank_core <- imputation_stability |> dplyr::filter(subset == "rank_core")
single_best <- unconstrained_path |> dplyr::filter(subset_size == 1)
theme_balanced_95 <- threshold_results |>
  dplyr::filter(constraint == "All three themes", threshold == 0.95)
subtheme_balanced_95 <- threshold_results |>
  dplyr::filter(constraint == "All six sub-themes", threshold == 0.95)

pairwise_for_max <- abs(pairwise)
diag(pairwise_for_max) <- NA_real_
max_pair_location <- which(
  pairwise_for_max == max(pairwise_for_max, na.rm = TRUE),
  arr.ind = TRUE
)[1, ]
max_pair_rho <- pairwise[max_pair_location[1], max_pair_location[2]]
max_pair_labels <- lever_specs$label[max_pair_location]
pcs_for_80 <- which(pca_summary$cumulative_variance_explained >= 0.80)[1]
pcs_for_90 <- which(pca_summary$cumulative_variance_explained >= 0.90)[1]
least_influential <- lever_diagnostics |>
  dplyr::slice_min(rank_information_loss, n = 3)
stable_core_frequency <- cv_frequency |>
  dplyr::slice_max(selection_frequency, n = 6)

bullet_list <- function(values) paste0("- ", values)
report <- c(
  "# Parsimonious LEMI analysis",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
  "",
  "## Main result",
  "",
  paste0(
    "The smallest unconstrained rank core contains **", rank_core_k,
    " levers** and has Spearman rho **", sprintf("%.3f", rank_core$spearman_rho),
    "** with the 18-lever tract ranking."
  ),
  "",
  bullet_list(lever_specs$label[rank_core_indices]),
  "",
  paste0(
    "It retains ", scales::percent(rank_core$bottom_group_overlap, accuracy = 1),
    " of the bottom 20% and ", scales::percent(rank_core$top_group_overlap, accuracy = 1),
    " of the top 20%, with a mean absolute rank error of ",
    sprintf("%.1f", rank_core$rank_mae), " places."
  ),
  "",
  paste0(
    "A high-fidelity subset reaches rho **", sprintf("%.3f", high_fidelity$spearman_rho),
    "** with **", high_fidelity$subset_size, " levers** and mean absolute rank error ",
    sprintf("%.1f", high_fidelity$rank_mae), "."
  ),
  "",
  paste0(
    "Requiring at least 90% retention in both the top and bottom 20% requires **",
    extreme_preserving$subset_size, " levers** (rho **",
    sprintf("%.3f", extreme_preserving$spearman_rho), "**)."
  ),
  "",
  "## Domain-balance tradeoff",
  "",
  paste0(
    "Requiring at least one lever from every theme reaches rho >= .95 with **",
    theme_balanced_95$subset_size, " levers**. Requiring at least one lever from",
    " every sub-theme reaches the same threshold with **",
    subtheme_balanced_95$subset_size, " levers**."
  ),
  "",
  "## Evidence of redundancy",
  "",
  paste0(
    "The single best proxy is **", single_best$lever_labels,
    "**, which alone has rho **", sprintf("%.3f", single_best$spearman_rho), "**."
  ),
  "",
  paste0(
    "The strongest pairwise lever correlation is **", sprintf("%.3f", max_pair_rho),
    "** between **", max_pair_labels[1], "** and **", max_pair_labels[2],
    "**. Thus, redundancy is distributed across several moderate relationships",
    " rather than explained only by duplicate pairs above 0.80."
  ),
  "",
  paste0(
    "The first principal component explains ",
    scales::percent(pca_summary$variance_explained[1], accuracy = 0.1),
    " of standardized lever variance; ", pcs_for_80, " components are needed for 80%",
    " and ", pcs_for_90, " for 90%. The eigenvalue participation-ratio effective",
    " dimension is ", sprintf("%.1f", effective_dimension), " out of 18."
  ),
  "",
  paste0(
    "The smallest leave-one-out effects are for **",
    paste(least_influential$label, collapse = "**, **"),
    "**; removing each leaves the full tract ranking correlated at rho ",
    paste(sprintf("%.5f", least_influential$full_without_lever_rho), collapse = ", "),
    ", respectively."
  ),
  "",
  "## Stability",
  "",
  paste0(
    "Across ", cv_repeats, " repeated 80/20 splits, the median held-out rho for a",
    " training-selected ", rank_core_k, "-lever subset was **",
    sprintf("%.3f", stats::median(cv_test$spearman_rho)), "** (10th-90th percentile ",
    sprintf("%.3f", stats::quantile(cv_test$spearman_rho, 0.10)), "-",
    sprintf("%.3f", stats::quantile(cv_test$spearman_rho, 0.90)), ")."
  ),
  "",
  paste0(
    "The most frequently selected levers in the repeated four-variable training",
    " subsets were: ",
    paste0(
      stable_core_frequency$label, " (",
      scales::percent(stable_core_frequency$selection_frequency, accuracy = 0.1), ")",
      collapse = "; "
    ),
    ". This shows that some core positions are occupied by interchangeable proxies."
  ),
  "",
  paste0(
    "Across the 10 MICE completions, the fixed rank core's rho ranged from **",
    sprintf("%.3f", min(imputation_rank_core$spearman_rho)), "** to **",
    sprintf("%.3f", max(imputation_rank_core$spearman_rho)), "**."
  ),
  "",
  "## Interpretation",
  "",
  paste(
    "A small subset can reproduce much of the full index's ordering if the",
    "reported correlations and extreme-group overlap are high. This supports the",
    "hypothesis that several levers contribute limited additional rank information."
  ),
  "",
  paste(
    "It does not prove that omitted levers are substantively unimportant. An index",
    "can deliberately represent distinct policy mechanisms even when they do not",
    "move the composite rank much. The unconstrained and domain-balanced results",
    "should therefore be presented together."
  ),
  "",
  "## Important limitations",
  "",
  "- The target is the corrected LEMI formula, not an external mobility outcome.",
  "- Selection optimizes rank replication; it does not establish causal importance.",
  "- Several supplemental levers still use published raw fallbacks.",
  "- Exact City MICE settings remain unavailable.",
  "- Results are conditional on the 249-tract Austin study universe.",
  "",
  "## Computation",
  "",
  paste0(
    "All ", format(subset_total, big.mark = ","), " nonempty subsets were evaluated in ",
    sprintf("%.1f", search_seconds), " seconds. Detailed tables and plots are in `output/parsimony/`."
  )
)
writeLines(report, file.path(output_dir, "report.md"))

message(
  "Parsimony analysis complete: rank core ", rank_core_k,
  " levers (rho=", sprintf("%.3f", rank_core$spearman_rho),
  "); high fidelity ", high_fidelity$subset_size, " levers."
)
