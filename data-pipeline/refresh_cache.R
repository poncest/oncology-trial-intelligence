# =============================================================================
# refresh_cache.R
# Pulls overview data from ClinicalTrials.gov API v2, builds cached .rds,
# and writes cache_meta.json. Run by GitHub Actions daily.
#
# NOT deployed to shinyapps.io.
# =============================================================================

library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(stringr)

# TODO: Phase 1 coverage probe (run manually first)
# See: scripts/01_coverage_probe.R

cat("[refresh_cache] Starting cache refresh —", format(Sys.time()), "
")

# TODO: implement API pull + .rds write
# See: OCTID_Technical_Spec.docx Section 3

cat("[refresh_cache] Done.
")

