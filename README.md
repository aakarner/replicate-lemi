# Austin Levers of Economic Mobility Index replication

This repository rebuilds the Austin LEMI using the formula documented in the
April 2026 technical report. It also audits the scores in the public Socrata
table and ArcGIS map.

## Current status

- The 2024 ACS levers are pulled independently from the Census API.
- 2024 eviction filing rates are rebuilt from unique court cases in
  `data/eviction_filings_full_geocoded_hex.csv`.
- The documented percentile, sub-theme, theme, and overall calculations are
  implemented with joins keyed on 11-digit census tract GEOID.
- Heat and energy burden use clearly marked published raw values until the
  original extracts/API parameters are available.
- The study-area GEOID list and polygons temporarily use the April 2026 public
  ArcGIS layer until the historical Austin jurisdiction-boundary snapshot is
  available.

## Requirements

R 4.3 or newer and the packages listed in `R/utils.R` are required. The scoring
step also requires `mice`. A Census API key must be stored in the environment as
`CENSUS_API_KEY`. The Austin app token is optional and may be stored as
`AUSTIN_APP_TOKEN`; it is not committed to this repository.

## Run

```r
source("run_all.R")
```

Intermediate downloads are written to `data/raw`, processed inputs to
`data/processed`, and final tables, maps, and diagnostics to `output`.

## Interpretation

`output/lemi_scores_corrected.csv` is the primary result. It follows the report:
orient each lever so higher is more favorable, percentile-rank each lever,
average the 18 lever percentiles, and percentile-rank that mean. Theme and
sub-theme scores are calculated for interpretation and do not weight the
overall index.

The public-data audit is intentionally separate. It documents the apparent
polygon spatial-join error without propagating that error into the corrected
index.
