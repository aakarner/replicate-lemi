# DOE LEAD energy-burden reconstruction

## Why this note exists

The April 2026 LEMI technical report defines energy burden as total annual
housing energy costs divided by total annual household income and cites the DOE
Low-Income Energy Affordability Data (LEAD) Tool 2022 Update. It does not state:

- which of the AMI, FPL, SMI, or LLSI tract tables was used;
- whether the calculation includes every household or only an income subset;
- how the many tenure, building, and fuel rows were aggregated;
- how missing expenditure components were handled; or
- how a continuous percentage became the published integer values 0-4.

This document records the assumptions used by the replication. They are an
inference from the official DOE files, the report's definition, and validation
against the public pre-imputation LEMI field. They should not be represented as
City of Austin or Every Texan methodology unless confirmed by those authors.

## Official source

- Dataset: *Low-Income Energy Affordability Data - LEAD Tool - 2022 Update*
- Publisher: U.S. Department of Energy, Office of Energy Efficiency and
  Renewable Energy, distributed through OEDI/OpenEI
- DOI: <https://doi.org/10.25984/2504170>
- State archive: `TX-2022-LEAD-data.zip`
- Selected archive member: `TX AMI Census Tracts 2022.csv`

The release describes the underlying data as primarily 2018-2022 ACS five-year
PUMS estimates calibrated to 2022 EIA electricity and natural-gas utility data.
The pipeline records the downloaded archive's MD5 checksum in
`output/diagnostics/energy_burden_reconstruction_metadata.csv`.

## Why the AMI table was selected

The state archive repeats the same modeled household information in four income
classification families: Area Median Income (AMI), Federal Poverty Level (FPL),
State Median Income (SMI), and Lower Living Standard Income (LLSI). The choice
affects how rows are grouped, not the underlying tract total when every income
group is retained.

The AMI table was selected because AMI is a common housing-policy convention
and because the resulting all-household tract aggregation closely reproduces
the public LEMI values. No AMI threshold is applied. The following groups are
all included:

- 0-30%;
- 30-60%;
- 60-80%;
- 80-100%;
- 100-150%; and
- 150%+.

## Unit of observation and filters

Each source row represents a modeled tract cell split by:

- AMI group;
- tenure and building vintage (`TEN-YBL6`);
- tenure and building type (`TEN-BLD`); and
- tenure and primary heating fuel (`TEN-HFL`).

The reconstruction filters only on the 249 LEMI census-tract GEOIDs. It does
not filter on income group, owner/renter tenure, building vintage/type, or fuel.
This implements a total-population neighborhood condition rather than a
low-income-household-only measure.

## Aggregation

LEAD supplies survey-weighted sums and separate valid-household counts for
income and each expenditure component. For tract \(t\), calculate:

\[
\bar{I}_t = \frac{\sum_c (HINCP \times UNITS)_{tc}}
                  {\sum_c HINCP\ UNITS_{tc}}
\]

\[
\bar{E}^{elec}_t = \frac{\sum_c (ELEP \times UNITS)_{tc}}
                         {\sum_c ELEP\ UNITS_{tc}}
\]

with equivalent calculations for gas (`GASP`) and other fuel (`FULP`). The
continuous tract burden is:

\[
Burden_t = 100 \times
\frac{\bar{E}^{elec}_t + \bar{E}^{gas}_t + \bar{E}^{fuel}_t}
     {\bar{I}_t}
\]

Here, \(c\) indexes all source cells in the tract.

Using each component's own valid-unit denominator is important. It avoids
treating an unreported gas or other-fuel expenditure as zero and improves the
match to the public LEMI field compared with simply dividing total weighted
energy expenditures by total weighted income.

## Conversion to the LEMI field

The official LEMI report says the mapped energy-burden values form a small set
of discrete percentage categories from 0 through 4, while the underlying DOE
result is continuous. The replication therefore:

1. retains `energy_burden_continuous` for transparency;
2. rounds to the nearest integer using conventional half-up rounding,
   `floor(x + 0.5)`; and
3. caps the display/scoring input to the documented range 0-4.

The cap currently affects a tract whose public raw value is missing, so it does
not inflate the agreement statistic.

## Coverage and validation

The official Texas archive contains 248 of the 249 LEMI tracts. GEOID
`48453002319` is absent and remains missing for MICE rather than being treated
as zero.

The public LEMI raw field is also missing for GEOIDs `48453000608` and
`48453001606`, even though the current DOE archive contains both. Those two
independent values are retained and no longer require imputation in the
replication.

Among tracts for which both the independent reconstruction and public raw field
are present, the rounded calculation reproduces 244 of 246 values. The two
disagreements are:

- `48453002422`; and
- `48491020513`.

Both independent continuous values are slightly above 1.5% and round to 2,
whereas the public field is 1. The isolated nature of these discrepancies is
more consistent with a source-version, manual rounding, or tract-processing
difference than with a different statewide aggregation rule.

Detailed tract comparisons are written to
`output/diagnostics/energy_burden_public_validation.csv`.

## Reproducible workflow

The implementation is in `scripts/03_build_energy_burden.R` and runs before
`scripts/03_build_supplemental.R`:

1. Download and cache the official Texas archive if needed.
2. Extract only the AMI census-tract member.
3. Read only the columns needed for aggregation.
4. Filter to the LEMI GEOID universe.
5. Aggregate weighted sums and valid-unit denominators.
6. Calculate continuous and integer burden values.
7. Validate against the public raw field without using it as an input.
8. Write the independent tract table to
   `data/processed/energy_burden_lead.csv`.
9. Let the supplemental stage replace the former public-raw fallback by GEOID.

## Remaining uncertainty

This reconstruction is highly consistent with the public values but cannot
establish the City's exact workflow. Confirmation would require the original
LEAD extract or code, its download date/version, and the rule used to convert
continuous burden into the five published categories.
