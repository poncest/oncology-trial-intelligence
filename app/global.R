# =============================================================================
# global.R
# Packages, cache loading, color palette, app constants
#
# Load order (CRITICAL — from lessons learned):
#   1. R/ folder auto-sources alphabetically  ← R/ is intentionally EMPTY
#   2. global.R runs                          ← this file
#   3. app.R executes (sources modules + ui/server explicitly)
#
# Author: Steven Ponce | February 2026
# =============================================================================

# --- Packages (explicit — no library(tidyverse)) -----------------------------
library(shiny)
library(shiny.semantic)
library(waiter)
library(shinyjs)
library(htmltools)
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
  primary          = "#0077B6",   # Oncology blue — main UI elements
  primary_dark     = "#023E8A",   # Header / sidebar background
  primary_light    = "#90E0EF",   # Light accent
  accent           = "#48CAE4",   # Section underlines, highlights
  rare_disease     = "#7B2D8B",   # Rare Disease contrast series
  rare_light       = "#CE93D8",   # Rare Disease light
  success          = "#107C10",   # Positive indicators
  warning          = "#CA5010",   # Scope boundary callouts
  danger           = "#D13438",   # Error states
  text_dark        = "#323130",   # Body text
  text_gray        = "#605E5C",   # Secondary text
  text_light       = "#A19F9D",   # Tertiary / disabled
  bg_light_blue    = "#E8F4FD",   # Oncology callout background
  bg_light_purple  = "#F3E8F9",   # Rare Disease callout background
  bg_light_gray    = "#F5F5F5",   # Card backgrounds
  bg_white         = "#FFFFFF",
  border           = "#EDEBE9"    # Subtle borders
)

# --- App metadata ------------------------------------------------------------
app_meta <- list(
  title        = "OCTID",
  subtitle     = "Oncology Competitive Trial Intelligence Dashboard",
  version      = "0.1.0-dev",
  author       = "Steven Ponce",
  data_source  = "ClinicalTrials.gov API v2 (NIH/NLM)",
  cache_scope  = "RECRUITING + ACTIVE_NOT_RECRUITING | PHASE2_PHASE3 + PHASE3"
)

# --- Sponsor class labels (human-readable) -----------------------------------
sponsor_class_labels <- c(
  "INDUSTRY"  = "Industry",
  "OTHER"     = "Academic / Other",
  "NETWORK"   = "Research Network",
  "NIH"       = "NIH",
  "OTHER_GOV" = "Other Government",
  "UNKNOWN"   = "Unknown"
)

# --- Cache loading (fail-open) -----------------------------------------------
#
# Startup logic:
#   1. Try to download fresh cache from GitHub Release (if newer than local)
#   2. Fall back to bundled .rds in app/data/processed/ if download fails
#   3. If neither exists, return empty list and show "no data" state
#
# The app NEVER blocks on a failed download — it always loads.

load_cache <- function(app_dir = ".") {
  
  local_rds  <- file.path(app_dir, "data", "processed", "cache_overview.rds")
  local_meta <- file.path(app_dir, "data", "processed", "cache_meta.json")
  
  # GitHub Release URLs (update USER/REPO after first GH Actions run)
  remote_meta_url <- "https://github.com/poncest/oncology-trial-intelligence/releases/latest/download/cache_meta.json"
  remote_rds_url  <- "https://github.com/poncest/oncology-trial-intelligence/releases/latest/download/cache_overview.rds"
  
  # --- Attempt remote version check + download -------------------------------
  fresh_data <- tryCatch({
    
    remote_meta <- jsonlite::fromJSON(remote_meta_url)
    remote_date <- as.Date(remote_meta$last_updated)
    
    local_date <- if (file.exists(local_meta)) {
      as.Date(jsonlite::fromJSON(local_meta)$last_updated)
    } else {
      as.Date("2000-01-01")   # force download if no local meta
    }
    
    if (remote_date > local_date) {
      message(glue("[Cache] Remote cache is newer ({remote_date}). Downloading..."))
      tmp <- tempfile(fileext = ".rds")
      download.file(remote_rds_url, tmp, quiet = TRUE, mode = "wb")
      data <- readRDS(tmp)
      message(glue("[Cache] Remote cache loaded successfully ({remote_date})."))
      data
    } else {
      message(glue("[Cache] Local cache is current ({local_date}). Using bundled fallback."))
      NULL   # signal: use local
    }
    
  }, error = function(e) {
    message(glue("[Cache] Remote unavailable ({conditionMessage(e)}). Falling back to local."))
    NULL   # signal: use local
  })
  
  # --- Use remote data if downloaded, otherwise use local --------------------
  if (!is.null(fresh_data)) return(fresh_data)
  
  if (file.exists(local_rds)) {
    data <- readRDS(local_rds)
    message(glue("[Cache] Bundled fallback loaded: {nrow(data)} trials."))
    return(data)
  }
  
  # --- Nothing available — return empty state --------------------------------
  warning("[Cache] No cache available. App will show empty state.")
  tibble(
    nct_id = character(), brief_title = character(), overall_status = character(),
    start_date = character(), primary_completion = character(), phases = character(),
    study_type = character(), enrollment = integer(), lead_sponsor = character(),
    sponsor_class = character(), conditions = character(), intervention_types = character(),
    intervention_names = character(), primary_endpoint = character(),
    primary_timeframe = character(), countries = character(),
    n_sites = integer(), ta = character(), cached_at = character()
  )
}

# --- Load cache at startup ---------------------------------------------------
message("[OCTID] Loading cache...")
app_cache <- load_cache()
message(glue("[OCTID] Cache ready: {nrow(app_cache)} trials ({format(Sys.time(), '%H:%M:%S')})"))

# --- Cache metadata (for UI freshness banner) --------------------------------
load_cache_meta <- function(app_dir = ".") {
  local_meta <- file.path(app_dir, "data", "processed", "cache_meta.json")
  if (file.exists(local_meta)) {
    tryCatch(
      jsonlite::fromJSON(local_meta),
      error = function(e) list(last_updated = "unknown", total_trial_count = 0)
    )
  } else {
    list(last_updated = "unknown", total_trial_count = 0)
  }
}

cache_meta <- load_cache_meta()

# --- Shared formatting helpers -----------------------------------------------

fmt_count <- function(x) {
  scales::comma(x, accuracy = 1)
}

fmt_pct <- function(x, digits = 1) {
  paste0(round(x * 100, digits), "%")
}

fmt_date <- function(x) {
  if (is.na(x) || x == "unknown") return("N/A")
  format(as.Date(x), "%B %d, %Y")
}

# Normalize phase strings for display
fmt_phase <- function(x) {
  x |>
    str_replace_all("PHASE", "Phase ") |>
    str_replace_all("_", "/") |>
    str_replace_all("\\|", " | ")
}

# Truncate long strings for table display
fmt_truncate <- function(x, width = 60) {
  ifelse(nchar(x) > width, paste0(substr(x, 1, width), "…"), x)
}