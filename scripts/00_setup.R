# =============================================================================
# 00_setup.R
# One-time setup script for the Oncology Competitive Trial Intelligence Dashboard
# Run this script ONCE during initial project setup
#
# Author: Steven Ponce
# Date:   February 2026
#
# ⚠️  LOCAL DEVELOPMENT ONLY — never deploy this file to shinyapps.io
# =============================================================================

cat("
╔══════════════════════════════════════════════════════════════════════════════╗
║       Oncology Competitive Trial Intelligence Dashboard (OCTID)              ║
║                     Environment Setup                                        ║
╚══════════════════════════════════════════════════════════════════════════════╝
\n")

# --- Step 1: CRAN mirror -------------------------------------------------------
cat("🔧 Step 1: Setting CRAN mirror...\n")
options(repos = c(CRAN = "https://cloud.r-project.org"))
cat("   ✅ Mirror set to cloud.r-project.org\n\n")


# --- Step 2: Bootstrap pak -----------------------------------------------------
cat("📦 Step 2: Ensuring pak is available...\n")
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak", quiet = TRUE)
  cat("   ✅ pak installed\n\n")
} else {
  cat("   ✅ pak already available\n\n")
}


# --- Step 3: Define package stack ----------------------------------------------
cat("📋 Step 3: Defining package stack...\n\n")

packages <- c(
  
  # --- Core Shiny ---------------------------------------------------------------
  "shiny",
  "shiny.semantic",       # Appsilon enterprise UI framework (Semantic UI)
  
  # --- UI & Layout --------------------------------------------------------------
  "waiter",               # Loading screens and spinners
  "shinyjs",              # JavaScript helpers (show/hide, toggle)
  "htmltools",            # HTML construction utilities
  
  # --- Data Wrangling -----------------------------------------------------------
  "dplyr",
  "tidyr",
  "purrr",
  "stringr",
  "lubridate",
  "forcats",
  
  # --- API & HTTP ---------------------------------------------------------------
  "httr2",                # Modern HTTP client — ClinicalTrials.gov API v2
  "jsonlite",             # JSON parsing for API responses
  
  # --- Visualization ------------------------------------------------------------
  "ggplot2",
  "ggiraph",              # Interactive SVG charts (preferred over plotly)
  "scales",               # Number and axis formatting
  "patchwork",            # Multi-panel chart layouts
  
  # --- Tables -------------------------------------------------------------------
  "reactable",            # Modern interactive tables
  "reactablefmtr",        # reactable formatting helpers
  
  # --- Maps ---------------------------------------------------------------------
  "leaflet",              # Interactive maps for site networks
  "leaflet.extras",       # Marker clustering and additional leaflet tools
  
  # --- Utilities ----------------------------------------------------------------
  "here",                 # Project-relative paths
  "fs",                   # File system operations
  "glue",                 # String interpolation
  "janitor",              # Data cleaning helpers
  
  # --- Deployment ---------------------------------------------------------------
  "rsconnect"             # shinyapps.io deployment
)

cat(sprintf("   Total packages to install: %d\n\n", length(packages)))


# --- Step 4: Install via pak ---------------------------------------------------
cat("🚀 Step 4: Installing packages with pak...\n")
cat("   (This may take a few minutes on first run)\n\n")

tryCatch({
  pak::pkg_install(packages, ask = FALSE)
  cat("\n   ✅ All packages installed successfully\n\n")
}, error = function(e) {
  cat("\n   ❌ Error during installation:\n")
  cat("  ", conditionMessage(e), "\n\n")
  cat("   Review errors above and retry individual packages if needed.\n\n")
})


# --- Step 5: Create folder structure -------------------------------------------
cat("📁 Step 5: Creating project folder structure...\n\n")

dirs <- c(
  # Development scripts (NOT deployed)
  "scripts",
  
  # Data pipeline (NOT deployed — run by GitHub Actions)
  "data-pipeline",
  
  # GitHub Actions workflow
  ".github/workflows",
  
  # Raw and processed data (local only)
  "data/raw",
  "data/processed",
  
  # App directory (deployed to shinyapps.io)
  "app/modules",
  "app/data/processed",    # Bundled fallback cache — committed to repo
  "app/www",
  
  # Docs and portfolio assets (NOT deployed)
  "docs",
  "screenshots"
)

for (d in dirs) {
  fs::dir_create(d)
  cat(sprintf("   ✅ %s/\n", d))
}
cat("\n")


# --- Step 6: Create placeholder files ------------------------------------------
cat("📄 Step 6: Creating placeholder files...\n\n")

# Helper: write a file only if it doesn't already exist
write_placeholder <- function(path, content) {
  if (!file.exists(path)) {
    writeLines(content, path)
    cat(sprintf("   ✅ %s\n", path))
  } else {
    cat(sprintf("   ⏭️  %s (already exists — skipped)\n", path))
  }
}

# .gitkeep files for empty dirs that need to be tracked
gitkeep_dirs <- c(
  "data/raw",
  "data/processed",
  "app/data/processed",
  "screenshots",
  "docs"
)
for (d in gitkeep_dirs) {
  write_placeholder(file.path(d, ".gitkeep"), "")
}

cat("\n")

# ── App entry point ─────────────────────────────────────────────────────────
write_placeholder("app/app.R", '# =============================================================================
# app.R — OCTID Entry Point
# Oncology Competitive Trial Intelligence Dashboard
# Author: Steven Ponce | February 2026
# =============================================================================
# ⚠️  Source order matters. Modules and ui.R sourced AFTER global.R.
#     R/ folder is intentionally EMPTY — avoids auto-sourcing before global.R.
# =============================================================================

source("global.R")

# Modules (explicit source — preserves load order)
source("modules/mod_executive_brief.R")
source("modules/mod_pipeline_endpoints.R")
source("modules/mod_sponsor_activity.R")
source("modules/mod_site_networks.R")
source("modules/mod_methods.R")

# UI & Server (sourced after global.R + modules)
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
')

# ── global.R ─────────────────────────────────────────────────────────────────
write_placeholder("app/global.R", '# =============================================================================
# global.R — Packages, cache loading, constants, color palette
# =============================================================================

# --- Packages (explicit — no library(tidyverse)) ----------------------------
library(shiny)
library(shiny.semantic)
library(waiter)
library(shinyjs)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(forcats)
library(httr2)
library(jsonlite)
library(ggplot2)
library(ggiraph)
library(scales)
library(patchwork)
library(reactable)
library(reactablefmtr)
library(leaflet)
library(leaflet.extras)
library(here)
library(glue)
library(janitor)

# --- Color palette -----------------------------------------------------------
octid_colors <- list(
  primary       = "#0077B6",   # Oncology blue
  primary_dark  = "#023E8A",   # Header / sidebar background
  accent        = "#48CAE4",   # Section underlines, highlights
  rare_disease  = "#7B2D8B",   # Rare Disease contrast series
  success       = "#107C10",   # Positive indicators
  warning       = "#CA5010",   # Scope boundary callouts
  text_dark     = "#323130",
  text_gray     = "#605E5C",
  bg_light_blue = "#E8F4FD",
  bg_light_purple = "#F3E8F9",
  bg_light_gray = "#F5F5F5"
)

# --- App metadata ------------------------------------------------------------
app_meta <- list(
  title    = "OCTID",
  subtitle = "Oncology Competitive Trial Intelligence Dashboard",
  version  = "0.1.0-dev",
  author   = "Steven Ponce"
)

# --- Cache loading (fail-open) -----------------------------------------------
# TODO: implement load_cache() with GitHub Release fallback
# See: OCTID_Technical_Spec.docx Section 3
load_cache <- function(app_dir = ".") {
  fallback <- file.path(app_dir, "data", "processed", "cache_trials_overview.rds")
  if (file.exists(fallback)) {
    readRDS(fallback)
  } else {
    message("[Cache] No fallback cache found — app will load empty state")
    list()
  }
}

app_cache <- load_cache()
')

# ── ui.R ─────────────────────────────────────────────────────────────────────
write_placeholder("app/ui.R", '# =============================================================================
# ui.R — Top-level UI shell
# =============================================================================

ui <- semanticPage(
  title = "OCTID | Oncology Competitive Trial Intelligence Dashboard",

  # TODO: wire up module UIs
  h1("OCTID — placeholder")
)
')

# ── server.R ─────────────────────────────────────────────────────────────────
write_placeholder("app/server.R", '# =============================================================================
# server.R — Top-level server
# =============================================================================

server <- function(input, output, session) {

  # TODO: call module servers
  # executive_brief_server("executive_brief", app_cache)
  # pipeline_endpoints_server("pipeline_endpoints", app_cache)
  # sponsor_activity_server("sponsor_activity", app_cache)
  # site_networks_server("site_networks", app_cache)
  # methods_server("methods")

}
')

# ── Module placeholders ───────────────────────────────────────────────────────
modules <- list(
  list(
    path = "app/modules/mod_executive_brief.R",
    id   = "executive_brief",
    label = "Executive Brief"
  ),
  list(
    path = "app/modules/mod_pipeline_endpoints.R",
    id   = "pipeline_endpoints",
    label = "Pipeline & Endpoints"
  ),
  list(
    path = "app/modules/mod_sponsor_activity.R",
    id   = "sponsor_activity",
    label = "Sponsor Activity"
  ),
  list(
    path = "app/modules/mod_site_networks.R",
    id   = "site_networks",
    label = "Site & Investigator Networks"
  ),
  list(
    path = "app/modules/mod_methods.R",
    id   = "methods",
    label = "Methods & Governance"
  )
)

for (m in modules) {
  ui_fn     <- paste0(m$id, "_ui")
  server_fn <- paste0(m$id, "_server")
  write_placeholder(m$path, glue::glue(
    '# =============================================================================
# {m$label} Module
# =============================================================================

{ui_fn} <- function(id) {{
  ns <- NS(id)
  tagList(
    # TODO: build {m$label} UI
    h2("{m$label} — placeholder")
  )
}}

{server_fn} <- function(id, cache = NULL) {{
  moduleServer(id, function(input, output, session) {{
    ns <- session$ns

    # TODO: implement {m$label} logic

    # ⚠️  Apply outputOptions to ALL outputs (CSS tab fix — see lessons learned)
    # outputOptions(output, "my_output", suspendWhenHidden = FALSE)

  }})
}}
'))
}

# ── Data pipeline script ──────────────────────────────────────────────────────
write_placeholder("data-pipeline/refresh_cache.R", '# =============================================================================
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

cat("[refresh_cache] Starting cache refresh —", format(Sys.time()), "\n")

# TODO: implement API pull + .rds write
# See: OCTID_Technical_Spec.docx Section 3

cat("[refresh_cache] Done.\n")
')

# ── Phase 1 coverage probe ────────────────────────────────────────────────────
write_placeholder("scripts/01_coverage_probe.R", '# =============================================================================
# 01_coverage_probe.R
# Phase 1 gating test: what % of PHASE3 oncology trials have primary endpoint
# text populated in ClinicalTrials.gov API v2?
#
# Decision thresholds (from OCTID spec):
#   >= 70% → endpoint panel is a centerpiece
#   40–69% → secondary / "best effort" with coverage badge
#   < 40%  → fallback to structured fields only
# =============================================================================

library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(stringr)

# TODO: paste coverage probe code from design session
# See conversation: "Phase 1 coverage probe" section
')

# ── GitHub Actions workflow ───────────────────────────────────────────────────
write_placeholder(".github/workflows/refresh_cache.yml", '# =============================================================================
# refresh_cache.yml — Daily cache refresh via GitHub Actions
# =============================================================================
name: Refresh ClinicalTrials Cache

on:
  schedule:
    - cron: "0 2 * * *"   # 02:00 UTC daily
  workflow_dispatch:        # Manual trigger for testing

jobs:
  refresh:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: r-lib/actions/setup-r@v2
      - name: Install dependencies
        run: Rscript -e "install.packages(c(\'httr2\', \'dplyr\', \'jsonlite\', \'purrr\', \'stringr\'))"
      - name: Build cache
        run: Rscript data-pipeline/refresh_cache.R
      - name: Upload to Release
        uses: softprops/action-gh-release@v2
        with:
          tag_name: cache-latest
          files: |
            app/data/processed/cache_trials_overview.rds
            app/data/processed/cache_meta.json
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
')

# ── DESCRIPTION ───────────────────────────────────────────────────────────────
write_placeholder("app/DESCRIPTION", 'Type: shiny
Title: Oncology Competitive Trial Intelligence Dashboard
Imports: shiny, shiny.semantic, waiter, shinyjs, htmltools, dplyr, tidyr, purrr, stringr, lubridate, forcats, httr2, jsonlite, ggplot2, ggiraph, scales, patchwork, reactable, reactablefmtr, leaflet, leaflet.extras, here, glue, janitor
')

# ── .rscignore ────────────────────────────────────────────────────────────────
write_placeholder("app/.rscignore", '# Deployment exclusions — never deploy these to shinyapps.io
renv/library/
renv/staging/
renv/python/
.Rprofile
.Rhistory
.RData
rsconnect/
*.qmd
*.Rmd
LESSONS_LEARNED.md
HANDOFF_DOCUMENT.md
')

# ── .gitignore ────────────────────────────────────────────────────────────────
write_placeholder(".gitignore", '.Rhistory
.RData
.Rproj.user/
renv/library/
renv/staging/
*.rds.bak
data/raw/
*.DS_Store
rsconnect/
')

# ── run_app.R (local dev launcher) ───────────────────────────────────────────
write_placeholder("run_app.R", '# =============================================================================
# run_app.R — Local development launcher
# Run this from the PROJECT ROOT, not from app/
# =============================================================================
setwd(here::here("app"))
source("app.R")
')

# ── README ────────────────────────────────────────────────────────────────────
write_placeholder("README.md", '# OCTID: Oncology Competitive Trial Intelligence Dashboard

> R Shiny dashboard consuming the ClinicalTrials.gov API v2 to analyze the active
> oncology competitive landscape — hypothesis-driven, governance-aware, and built
> for multi-persona decision support.

## Status

🚧 In development

## Overview

OCTID provides situational awareness of the oncology clinical trial landscape using
public ClinicalTrials.gov data. It is a benchmarking tool, not a forecasting engine.

**What it answers:**
- Where is trial activity concentrating by phase and modality?
- How do primary endpoints differ across sponsors?
- Who is accelerating pipeline activity?
- Where is enrollment concentrating geographically?

**What it does NOT do:**
- Predict trial success or failure
- Rank or score assets for investment
- Make treatment recommendations
- Estimate NPV or financial returns

## Tech Stack

R · Shiny · shiny.semantic · ggiraph · reactable · leaflet · httr2

## Data Source

[ClinicalTrials.gov](https://clinicaltrials.gov) API v2 — NIH/NLM registry of 500,000+ clinical studies.
Overview data cached daily via GitHub Actions. Trial detail fetched live on demand.

## Portfolio Context

Part of a pharmaceutical commercial analytics portfolio. Complements:
- [Launch Curve Forecaster](https://github.com/poncest/launch-curve-forecaster)
- [Clinical Trial Forecaster](https://github.com/poncest/clinical-trial-forecaster)
- [Pharma R&D Pipeline Simulator](https://github.com/poncest/pharma-rd-pipeline)

## Disclaimer

Portfolio project using publicly available data. Not for commercial use.

---

*Steven Ponce · 2026*
')

cat("\n")

# --- Step 7: Summary -----------------------------------------------------------
cat("
╔══════════════════════════════════════════════════════════════════════════════╗
║                         ✅ Setup Complete                                    ║
╚══════════════════════════════════════════════════════════════════════════════╝

📁 Folder structure created
📄 Placeholder files written (existing files were NOT overwritten)

Next steps:
  1. Run scripts/01_coverage_probe.R  ← Phase 1 gating test (run NOW)
  2. Review app/DESCRIPTION           ← Verify all packages listed
  3. Commit initial structure to Git

Key files:
  run_app.R               ← Local launcher (run from project root)
  app/app.R               ← Shiny entry point
  app/global.R            ← Packages + cache loading
  data-pipeline/          ← GitHub Actions cache refresh (NOT deployed)
  scripts/                ← Development scripts (NOT deployed)

⚠️  Reminder from lessons learned:
  • app/R/ folder is intentionally EMPTY (auto-sources before global.R)
  • Never put install.packages() inside app/
  • Always use rsconnect::deployApp(forceUpdate = TRUE)
  • outputOptions(suspendWhenHidden = FALSE) required for all module outputs

")