# Parsimonious-index analysis methodology

## Question

How many of the 18 LEMI levers are needed to preserve the tract rank ordering
of the complete documented index?

This is a test of redundancy within the composite, not a test of which
neighborhood conditions cause economic mobility. The full 18-lever corrected
LEMI is the target being approximated.

## Exhaustive search

For every nonempty subset of the 18 levers (262,143 subsets), the analysis:

1. averages the selected favorable-direction lever percentiles for each tract;
2. ranks the 249 tracts by that subset mean; and
3. compares the subset ranking with the full 18-lever ranking.

The globally best subset at each size is selected by Spearman rank correlation,
with mean absolute rank error as a tie-breaker. Detailed metrics are then
recomputed without search-time numerical approximations.

Three searches are reported:

- unconstrained;
- at least one lever from each of the three themes; and
- at least one lever from each of the six sub-themes.

The constrained paths distinguish empirical rank replication from conceptual
coverage.

## Evaluation metrics

The analysis reports:

- Spearman rank correlation;
- Kendall rank correlation;
- mean, root-mean-square, and maximum absolute tract-rank error;
- overlap in the full index's top and bottom 20%; and
- agreement in tract quintile.

The analysis reports three operating points instead of treating one threshold
as universally correct:

- the smallest unconstrained rank core with Spearman rho of at least 0.95;
- a high-fidelity solution with rho of at least 0.975; and
- the smallest solution with rho of at least 0.95 and at least 90% overlap in
  both the top and bottom 20%.

## Redundancy and unique rank information

Pairwise Spearman correlations identify closely related levers. Each lever is
also removed from the full index one at a time. The correlation between that
17-lever result and the full result measures how much unique rank information
is lost when the lever is omitted.

Because the full index is an equal-weighted arithmetic construction, this
leave-one-out statistic should be interpreted as redundancy within the formula,
not substantive importance.

A principal-components diagnostic is also calculated on standardized lever
percentiles. Variance explained and the eigenvalue participation-ratio effective
dimension summarize distributed multivariate redundancy that may not appear as
any single pairwise correlation above a conventional threshold.

## Stability checks

### Repeated train/test splits

For 200 repeated 80/20 splits, a forward-selection approximation chooses the
same number of levers using only the training tracts. Rank preservation is then
evaluated on held-out tracts. Lever selection frequencies show whether the core
set is stable or whether many interchangeable subsets perform similarly.

### Multiple imputations

The recommended and high-fidelity subsets are re-evaluated in each of the ten
MICE completed datasets. A forward path is also rerun in each completion to
measure whether imputation choices change the selected variables.

## Outputs

`scripts/07_analyze_parsimony.R` writes results to `output/parsimony/`, including:

- best subsets by size and conceptual constraint;
- fidelity thresholds;
- pairwise correlations and leave-one-out influence;
- cross-validation and imputation stability;
- tract-level full-versus-parsimonious ranks;
- diagnostic figures; and
- a generated findings report.

## Current findings

These results use the current corrected 249-tract dataset and are conditional on
the source and imputation limitations listed below.

### Broad rank core: four levers

The globally best four-lever subset is:

- disability rate;
- low physical activity;
- median household income; and
- less than high-school education.

Its Spearman correlation with the 18-lever tract ranking is 0.953. Mean absolute
rank error is 17.0 places. It retains 80% of the full index's bottom quintile and
82% of its top quintile. Because these four levers cover all three themes, adding
a three-theme constraint does not change this result.

Across 200 repeated 80/20 splits, a four-lever set selected on training tracts
has median held-out rho 0.937 (10th-90th percentile 0.911-0.954). The fixed core
has rho 0.951-0.957 across the ten MICE completions.

### Conceptually balanced core: six levers

Requiring at least one lever from all six sub-themes produces a six-lever subset
with rho 0.954:

- uninsured rate;
- disability rate;
- poverty rate;
- energy burden;
- less than high-school education; and
- heat disparity.

This has nearly the same overall rank fidelity as the unconstrained four-lever
core while preserving the full conceptual framework.

### Higher-fidelity operating points

A nine-lever subset reaches rho 0.978 and mean absolute rank error 11.9:

- uninsured rate;
- life expectancy;
- disability rate;
- low physical activity;
- median household income;
- poverty rate;
- household support risk;
- limited-English households; and
- less than high-school education.

Twelve levers are needed under the stricter requirement that at least 90% of
both the top and bottom quintiles remain in their respective extreme groups.
That subset has rho 0.988 and mean absolute rank error 8.9.

### What appears redundant

Low physical activity alone correlates 0.899 with the complete tract ranking,
showing that one socioeconomic/health gradient accounts for much of the index.
The first principal component explains 34.1% of standardized lever variance,
and the participation-ratio effective dimension is 6.7 rather than 18.

No single pair exceeds an absolute Spearman correlation of 0.787. The redundancy
therefore comes from several overlapping moderate correlations rather than only
obvious duplicate pairs.

Persistent poverty and institutional group quarters add almost no marginal rank
information in this Austin universe: removing them individually leaves the full
ranking correlated at 0.99996 and 0.99983, respectively. Removing energy burden
leaves rho 0.99835. These are the strongest empirical candidates for limited
incremental value in the composite ranking.

There is an important distinction between a good proxy and unique marginal
information. Low physical activity is the best single proxy for the full index,
but other socioeconomic variables can largely replace it. Conversely, household
support risk and underemployment are poor standalone proxies yet create the two
largest leave-one-out changes because their information is less correlated with
the rest of the index.

### Interpretation

The results support the hypothesis that the 18-variable index is more elaborate
than necessary if its sole purpose is to reproduce a general tract opportunity
ranking. Four variables recover the broad ordering, and six retain every
sub-theme with similar fidelity. More variables still add value when precise
rank positions or stable identification of top and bottom tracts matter.

## Limitations

- The analysis reproduces an index, not observed future mobility outcomes.
- A redundant lever may still represent a distinct actionable policy mechanism.
- Subset selection is conditional on Austin's 249-tract distribution.
- Some supplemental raw inputs and exact imputation settings remain provisional.
- Percentile indices are relative; results can change with a different study
  universe or update year.
