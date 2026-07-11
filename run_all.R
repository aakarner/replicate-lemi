pipeline_scripts <- c(
  "scripts/00_download_reference.R",
  "scripts/01_build_acs.R",
  "scripts/02_build_evictions.R",
  "scripts/03_build_supplemental.R",
  "scripts/04_build_index.R",
  "scripts/05_diagnose_public_scores.R",
  "scripts/06_make_map.R"
)

for (script in pipeline_scripts) {
  message("\n==> ", script)
  source(script, local = new.env(parent = globalenv()))
}

message("\nLEMI replication pipeline complete.")
