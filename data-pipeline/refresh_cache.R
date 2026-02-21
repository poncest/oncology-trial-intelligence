# =============================================================================
# refresh_cache.R
# Pulls overview data from ClinicalTrials.gov API v2 for both Oncology and
# Rare Disease, builds a single cached .rds, and writes cache_meta.json.
#
# Run manually for first build, then by GitHub Actions daily at 02:00 UTC.
# NOT deployed to shinyapps.io.
#
# Cache scope (locked in design session, February 2026):
#   Statuses : RECRUITING, ACTIVE_NOT_RECRUITING
#   Phases   : PHASE2_PHASE3, PHASE3
#   Structure: single cache_overview.rds + cache_meta.json
#
# Author: Steven Ponce | February 2026
# =============================================================================

library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)

cat("
╔══════════════════════════════════════════════════════════════════════════════╗
║              OCTID — Cache Refresh                                           ║
║              ClinicalTrials.gov API v2                                       ║
╚══════════════════════════════════════════════════════════════════════════════╝
\n")

start_time <- Sys.time()

# =============================================================================
# CONFIGURATION
# =============================================================================

CONFIG <- list(
  
  # Output paths (relative to project root — adjust if running from app/)
  out_rds  = "app/data/processed/cache_overview.rds",
  out_meta = "app/data/processed/cache_meta.json",
  
  # API settings
  page_size = 1000,
  max_pages = 20,          # Safety ceiling (~20,000 trials max per TA)
  timeout   = 30,
  retry     = 3,
  
  # Scope (locked)
  statuses  = c("RECRUITING", "ACTIVE_NOT_RECRUITING"),
  phases    = c("PHASE2_PHASE3", "PHASE3"),
  
  # Therapeutic areas
  ta = list(
    oncology = list(
      label = "Oncology",
      query = "cancer OR oncology OR tumor OR carcinoma OR lymphoma OR leukemia OR myeloma OR sarcoma OR glioma OR melanoma"
    ),
    rare_disease = list(
      label = "Rare Disease",
      query = "rare disease OR orphan disease OR ultra-rare"
    )
  )
)

# =============================================================================
# HELPER: Fetch one page from the API
# =============================================================================

fetch_page <- function(query_term, page_token = NULL) {
  
  params <- list(
    "query.term"          = query_term,
    "filter.overallStatus" = paste(CONFIG$statuses, collapse = ","),
    "pageSize"            = CONFIG$page_size,
    "fields"              = paste(
      # Identification
      "protocolSection.identificationModule.nctId",
      "protocolSection.identificationModule.briefTitle",
      "protocolSection.identificationModule.organization.fullName",
      # Status
      "protocolSection.statusModule.overallStatus",
      "protocolSection.statusModule.startDateStruct.date",
      "protocolSection.statusModule.primaryCompletionDateStruct.date",
      # Design
      "protocolSection.designModule.phases",
      "protocolSection.designModule.studyType",
      "protocolSection.designModule.enrollmentInfo.count",
      # Conditions & Interventions
      "protocolSection.conditionsModule.conditions",
      "protocolSection.conditionsModule.keywords",
      "protocolSection.armsInterventionsModule.interventions.type",
      "protocolSection.armsInterventionsModule.interventions.name",
      # Outcomes (centerpiece — 95.8% coverage confirmed in Phase 1 probe)
      "protocolSection.outcomesModule.primaryOutcomes.measure",
      "protocolSection.outcomesModule.primaryOutcomes.timeFrame",
      # Sponsor
      "protocolSection.sponsorCollaboratorsModule.leadSponsor.name",
      "protocolSection.sponsorCollaboratorsModule.leadSponsor.class",
      # Locations
      "protocolSection.contactsLocationsModule.locations.country",
      "protocolSection.contactsLocationsModule.locations.facility",
      sep = ","
    )
  )
  
  if (!is.null(page_token)) params[["pageToken"]] <- page_token
  
  resp <- request("https://clinicaltrials.gov/api/v2/studies") |>
    req_url_query(!!!params) |>
    req_timeout(CONFIG$timeout) |>
    req_retry(max_tries = CONFIG$retry, backoff = ~ 2) |>
    req_perform()
  
  resp_body_json(resp, simplifyVector = FALSE)
}

# =============================================================================
# HELPER: Flatten one study into a single-row tibble
# =============================================================================

flatten_study <- function(s) {
  
  ps  <- s$protocolSection
  id  <- ps$identificationModule
  st  <- ps$statusModule
  des <- ps$designModule
  con <- ps$conditionsModule
  arm <- ps$armsInterventionsModule
  out <- ps$outcomesModule
  sp  <- ps$sponsorCollaboratorsModule
  loc <- ps$contactsLocationsModule
  
  # Primary endpoints (95.8% populated — confirmed Phase 1 probe)
  primary_measures <- tryCatch({
    measures <- map_chr(out$primaryOutcomes %||% list(), ~ .x$measure %||% "")
    measures <- str_trim(measures)
    paste(measures[nzchar(measures)], collapse = " | ")
  }, error = function(e) NA_character_)
  
  primary_timeframes <- tryCatch({
    tfs <- map_chr(out$primaryOutcomes %||% list(), ~ .x$timeFrame %||% "")
    tfs <- str_trim(tfs)
    paste(tfs[nzchar(tfs)], collapse = " | ")
  }, error = function(e) NA_character_)
  
  # Interventions — type and name
  intervention_types <- tryCatch({
    types <- map_chr(arm$interventions %||% list(), ~ .x$type %||% "")
    paste(unique(types[nzchar(types)]), collapse = " | ")
  }, error = function(e) NA_character_)
  
  intervention_names <- tryCatch({
    names_vec <- map_chr(arm$interventions %||% list(), ~ .x$name %||% "")
    paste(names_vec[nzchar(names_vec)][1:min(3, sum(nzchar(names_vec)))], collapse = " | ")
  }, error = function(e) NA_character_)
  
  # Conditions
  conditions <- tryCatch({
    paste(unlist(con$conditions %||% list()), collapse = " | ")
  }, error = function(e) NA_character_)
  
  # Phases
  phases <- tryCatch({
    paste(unlist(des$phases %||% list()), collapse = " | ")
  }, error = function(e) NA_character_)
  
  # Countries (unique, collapsed)
  countries <- tryCatch({
    ctry <- map_chr(loc$locations %||% list(), ~ .x$country %||% "")
    paste(sort(unique(ctry[nzchar(ctry)])), collapse = " | ")
  }, error = function(e) NA_character_)
  
  # Site count
  n_sites <- tryCatch({
    length(loc$locations %||% list())
  }, error = function(e) NA_integer_)
  
  tibble(
    nct_id               = id$nctId               %||% NA_character_,
    brief_title          = id$briefTitle           %||% NA_character_,
    overall_status       = st$overallStatus        %||% NA_character_,
    start_date           = st$startDateStruct$date %||% NA_character_,
    primary_completion   = st$primaryCompletionDateStruct$date %||% NA_character_,
    phases               = phases,
    study_type           = des$studyType           %||% NA_character_,
    enrollment           = des$enrollmentInfo$count %||% NA_integer_,
    lead_sponsor         = sp$leadSponsor$name     %||% NA_character_,
    sponsor_class        = sp$leadSponsor$class    %||% NA_character_,
    conditions           = conditions,
    intervention_types   = intervention_types,
    intervention_names   = intervention_names,
    primary_endpoint     = primary_measures,
    primary_timeframe    = primary_timeframes,
    countries            = countries,
    n_sites              = n_sites
  )
}

# =============================================================================
# MAIN: Pull all pages for one TA
# =============================================================================

pull_ta <- function(ta_label, query_term) {
  
  cat(glue::glue("\n▶  Pulling {ta_label}...\n"))
  
  tok  <- NULL
  all  <- list()
  
  for (i in seq_len(CONFIG$max_pages)) {
    
    cat(glue::glue("   Page {i}... "))
    
    body <- tryCatch(
      fetch_page(query_term, page_token = tok),
      error = function(e) {
        cat(glue::glue("ERROR: {conditionMessage(e)}\n"))
        return(NULL)
      }
    )
    
    if (is.null(body)) break
    
    studies <- body$studies %||% list()
    if (length(studies) == 0) {
      cat("empty — stopping.\n")
      break
    }
    
    cat(glue::glue("{length(studies)} studies.\n"))
    
    page_df <- map_dfr(studies, ~ tryCatch(
      flatten_study(.x),
      error = function(e) {
        message("  [flatten] skipped one study: ", conditionMessage(e))
        NULL
      }
    ))
    
    all[[i]] <- page_df
    
    tok <- body$nextPageToken %||% NULL
    if (is.null(tok)) {
      cat(glue::glue("   No more pages for {ta_label}.\n"))
      break
    }
  }
  
  if (length(all) == 0) {
    warning(glue::glue("[{ta_label}] No data retrieved."))
    return(tibble())
  }
  
  result <- bind_rows(all) |>
    distinct(nct_id, .keep_all = TRUE) |>       # deduplicate
    filter(
      str_detect(
        phases,
        paste(CONFIG$phases, collapse = "|")
      )
    ) |>
    mutate(
      ta              = ta_label,
      enrollment      = suppressWarnings(as.integer(enrollment)),
      cached_at       = as.character(Sys.Date())
    )
  
  cat(glue::glue(
    "   ✅ {ta_label}: {nrow(result)} trials after phase filter\n"
  ))
  
  result
}

# =============================================================================
# BUILD CACHE
# =============================================================================

cat("📡 Fetching data from ClinicalTrials.gov API v2...\n")
cat(glue::glue(
  "   Statuses : {paste(CONFIG$statuses, collapse = ', ')}\n",
  "   Phases   : {paste(CONFIG$phases, collapse = ', ')}\n\n"
))

oncology     <- pull_ta(CONFIG$ta$oncology$label,     CONFIG$ta$oncology$query)
rare_disease <- pull_ta(CONFIG$ta$rare_disease$label, CONFIG$ta$rare_disease$query)

cache <- bind_rows(oncology, rare_disease)

cat(glue::glue("\n📊 Cache summary:\n"))
cat(glue::glue("   Oncology trials     : {nrow(oncology)}\n"))
cat(glue::glue("   Rare Disease trials : {nrow(rare_disease)}\n"))
cat(glue::glue("   Total               : {nrow(cache)}\n\n"))

# =============================================================================
# WRITE OUTPUTS
# =============================================================================

cat("💾 Writing cache files...\n")

# Ensure output directory exists
fs::dir_create(dirname(CONFIG$out_rds))

# .rds
saveRDS(cache, CONFIG$out_rds)
cat(glue::glue("   ✅ {CONFIG$out_rds}\n"))

# cache_meta.json — read by app on startup for version-check
meta <- list(
  last_updated          = as.character(Sys.Date()),
  api_version           = "v2",
  oncology_trial_count  = nrow(oncology),
  rare_disease_count    = nrow(rare_disease),
  total_trial_count     = nrow(cache),
  statuses              = CONFIG$statuses,
  phases                = CONFIG$phases,
  endpoint_coverage_pct = 95.8,   # Confirmed Phase 1 probe — Feb 2026
  notes = paste(
    "Endpoint text available for 95.8% of Phase 3 oncology trials (766 trials sampled).",
    "Free-text field — basic normalization applied (tolower + trimws + grepl).",
    "~4% missing endpoint text; adjacent-TA trials possible due to broad keyword query.",
    "COMPLETED trials excluded from default cache; available via live drill-down."
  )
)

write(jsonlite::toJSON(meta, auto_unbox = TRUE, pretty = TRUE), CONFIG$out_meta)
cat(glue::glue("   ✅ {CONFIG$out_meta}\n"))

# =============================================================================
# SUMMARY
# =============================================================================

elapsed <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 1)

cat(glue::glue("
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  Cache refresh complete in {elapsed}s
  Oncology     : {nrow(oncology)} trials
  Rare Disease : {nrow(rare_disease)} trials
  Total        : {nrow(cache)} trials
  Output       : {CONFIG$out_rds}
  Meta         : {CONFIG$out_meta}
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
\n"))

# Quick column check for downstream module development
cat("📋 Cache column names (for module reference):\n")
cat(paste(" ", names(cache), collapse = "\n"), "\n\n")

cat("📋 TA distribution:\n")
print(count(cache, ta))

cat("\n📋 Phase distribution:\n")
print(count(cache, phases) |> arrange(desc(n)))

cat("\n📋 Status distribution:\n")
print(count(cache, overall_status) |> arrange(desc(n)))

cat("\n📋 Sponsor class distribution:\n")
print(count(cache, sponsor_class) |> arrange(desc(n)))

cat("\n📋 Sample endpoint text (first 5 rows):\n")
cache |>
  filter(!is.na(primary_endpoint), nzchar(primary_endpoint)) |>
  slice_head(n = 5) |>
  select(nct_id, lead_sponsor, primary_endpoint) |>
  print(width = 120)