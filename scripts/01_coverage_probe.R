# =============================================================================
# 01_coverage_probe.R
# Phase 1 gating test: what % of PHASE3 oncology trials have primary endpoint
# text populated in ClinicalTrials.gov API v2?
#
# Decision thresholds (from OCTID spec):
#   >= 70% → endpoint panel is a centerpiece
#   40–69% → secondary / "best effort" with coverage badge
#   < 40%  → fallback to structured fields only
#
# Author: Steven Ponce | February 2026
# =============================================================================

library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(stringr)

cat("
╔══════════════════════════════════════════════════════════════════════════════╗
║              OCTID — Phase 1 Coverage Probe                                  ║
║              ClinicalTrials.gov API v2 — Endpoint Field Coverage             ║
╚══════════════════════════════════════════════════════════════════════════════╝
\n")

# -----------------------------------------------------------------------------
# Helper: fetch one page from ClinicalTrials.gov API v2
# -----------------------------------------------------------------------------
ctgov_page <- function(page_token = NULL, page_size = 1000) {
  
  base <- "https://clinicaltrials.gov/api/v2/studies"
  
  params <- list(
    "query.term" = "cancer OR oncology OR tumor OR carcinoma OR lymphoma OR leukemia",
    "pageSize"   = page_size,
    "fields"     = paste(
      "protocolSection.identificationModule.nctId",
      "protocolSection.designModule.phases",
      "protocolSection.outcomesModule.primaryOutcomes.measure",
      sep = ","
    )
  )
  
  if (!is.null(page_token)) params[["pageToken"]] <- page_token
  
  resp <- request(base) |>
    req_url_query(!!!params) |>
    req_timeout(30) |>
    req_retry(max_tries = 3, backoff = ~ 2) |>
    req_perform()
  
  resp_body_json(resp, simplifyVector = FALSE)
}

# -----------------------------------------------------------------------------
# Main: page through results, filter to PHASE3, compute coverage
# -----------------------------------------------------------------------------
endpoint_coverage_probe <- function(max_pages = 8) {
  
  cat(glue::glue("Fetching up to {max_pages} pages (1,000 trials/page)...\n\n"))
  
  tok  <- NULL
  all  <- list()
  
  for (i in seq_len(max_pages)) {
    
    cat(glue::glue("  Page {i}... "))
    
    tryCatch({
      b <- ctgov_page(page_token = tok, page_size = 1000)
    }, error = function(e) {
      cat(glue::glue("ERROR: {conditionMessage(e)}\n"))
      break
    })
    
    studies <- b$studies
    if (is.null(studies) || length(studies) == 0) {
      cat("no studies returned — stopping.\n")
      break
    }
    
    cat(glue::glue("{length(studies)} studies retrieved.\n"))
    
    # Flatten minimal fields per study
    df <- tibble(
      nct_id = map_chr(studies, ~ {
        .x$protocolSection$identificationModule$nctId %||% NA_character_
      }),
      phases = map(studies, ~ {
        .x$protocolSection$designModule$phases %||% list()
      }),
      primary_measures = map(studies, ~ {
        po <- .x$protocolSection$outcomesModule$primaryOutcomes
        if (is.null(po) || length(po) == 0) return(character(0))
        measures <- map_chr(po, ~ .x$measure %||% "")
        measures <- str_trim(measures)
        measures[nzchar(measures)]
      })
    )
    
    all[[i]] <- df
    
    tok <- b$nextPageToken %||% NULL
    if (is.null(tok)) {
      cat("  No more pages.\n")
      break
    }
  }
  
  if (length(all) == 0) {
    cat("\n❌ No data retrieved. Check API connectivity.\n")
    return(invisible(NULL))
  }
  
  dat <- bind_rows(all) |>
    mutate(
      is_phase3 = map_lgl(phases, ~ {
        any(unlist(.x) %in% c("PHASE3", "PHASE2_PHASE3", "PHASE3_PHASE4"))
      }),
      has_primary_endpoint = map_lgl(primary_measures, ~ length(.x) > 0),
      n_primary_endpoints  = map_int(primary_measures, length)
    ) |>
    filter(is_phase3)
  
  cat(glue::glue("\n  Total studies fetched (all phases): {nrow(bind_rows(all))}\n"))
  cat(glue::glue("  Phase 3 oncology studies: {nrow(dat)}\n\n"))
  
  if (nrow(dat) == 0) {
    cat("❌ No Phase 3 trials found. Check query terms.\n")
    return(invisible(NULL))
  }
  
  pct <- mean(dat$has_primary_endpoint) * 100
  
  dist <- dat |>
    count(n_primary_endpoints, name = "n_trials") |>
    mutate(pct = round(n_trials / sum(n_trials) * 100, 1)) |>
    arrange(n_primary_endpoints)
  
  # --- Decision gate -----------------------------------------------------------
  decision <- dplyr::case_when(
    pct >= 70 ~ "CENTERPIECE — endpoint panel is primary. Proceed as planned.",
    pct >= 40 ~ "SECONDARY   — keep panel, add coverage badge + 'best effort' framing.",
    TRUE      ~ "FALLBACK    — drop endpoint panel; default to structured fields only."
  )
  
  cat("
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  COVERAGE RESULTS
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
")
  
  cat(glue::glue("  Phase 3 oncology trials analysed : {nrow(dat)}\n"))
  cat(glue::glue("  % with ≥1 primary endpoint text  : {round(pct, 1)}%\n\n"))
  
  cat("  Distribution (# primary endpoints per trial):\n")
  print(dist, n = Inf)
  
  cat(glue::glue("\n  ▶  DECISION: {decision}\n"))
  cat("
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
\n")
  
  # Sample of endpoint text for manual review
  cat("  Sample endpoint text (first 10 populated trials):\n\n")
  sample_rows <- dat |>
    filter(has_primary_endpoint) |>
    slice_head(n = 10) |>
    mutate(endpoint_preview = map_chr(primary_measures, ~ paste(.x[1:min(2, length(.x))], collapse = " | ")))
  
  for (k in seq_len(nrow(sample_rows))) {
    cat(glue::glue("  [{sample_rows$nct_id[k]}] {sample_rows$endpoint_preview[k]}\n"))
  }
  
  invisible(list(
    n_phase3              = nrow(dat),
    pct_with_primary      = pct,
    dist_n_primary        = dist,
    decision              = decision,
    data                  = dat
  ))
}

# -----------------------------------------------------------------------------
# Run the probe
# -----------------------------------------------------------------------------
res <- endpoint_coverage_probe(max_pages = 8)

# Paste res$pct_with_primary and res$dist_n_primary back into the chat
# to get the UI banner wording and final endpoint panel decision.