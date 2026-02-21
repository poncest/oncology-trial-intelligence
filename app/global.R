# =============================================================================
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

