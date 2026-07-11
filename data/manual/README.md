# Optional source extracts

Place source files that cannot be retrieved automatically here. The pipeline
recognizes the following files when present:

- `heat_disparity.csv`: `geoid` plus `temperature_diff` and optionally
  `max_lst_f`.
- `energy_burden.csv`: `geoid` plus `energy_burden`.
- `life_expectancy_2020.csv`: `geoid` plus `le_2020_normalized` and optionally
  `le_data_coverage`.

An optional file must contain exactly one row per 11-digit 2020 census tract
GEOID. Its values replace the provisional published-raw fallback and are marked
as independently supplied in the provenance output.
