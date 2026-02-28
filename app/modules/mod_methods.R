# =============================================================================
# OCTID — mod_methods.R
# Methods & Governance Tab
# Static content only — no server logic, no reactive outputs
# Sections:
#   1. Purpose & Scope
#   2. Data Source
#   3. Cache Scope
#   4. Known Limitations (7 locked facts)
#   5. Endpoint Normalization
#   6. What This App Does NOT Do
#   7. Tech Stack
#   8. Contact / GitHub
# =============================================================================

methods_ui <- function(id) {
  ns <- NS(id)
  
  # Helper: section card
  methods_card <- function(..., border_color = octid_colors$primary) {
    div(
      class = "ui segment",
      style = paste0(
        "padding: 1.8em 2em; margin-bottom: 1.5em; ",
        "border-left: 4px solid ", border_color, ";"
      ),
      ...
    )
  }
  
  # Helper: section heading
  methods_heading <- function(icon_name, title, color = octid_colors$primary_dark) {
    div(
      style = paste0(
        "display: flex; align-items: center; gap: 0.6em; ",
        "margin-bottom: 1em; padding-bottom: 0.6em; ",
        "border-bottom: 1px solid ", octid_colors$border, ";"
      ),
      icon(icon_name, style = paste0("color: ", color, "; font-size: 1.1em;")),
      h3(style = paste0("color: ", color, "; margin: 0; font-size: 1.1em;"), title)
    )
  }
  
  # Helper: limitation badge
  limitation_item <- function(number, title, body) {
    div(
      style = paste0(
        "display: flex; gap: 1em; padding: 1em 0; ",
        "border-bottom: 1px solid ", octid_colors$border, ";"
      ),
      div(
        style = paste0(
          "flex-shrink: 0; width: 28px; height: 28px; border-radius: 50%; ",
          "background: ", octid_colors$primary, "; color: white; ",
          "display: flex; align-items: center; justify-content: center; ",
          "font-size: 0.82em; font-weight: 700;"
        ),
        number
      ),
      div(
        div(style = paste0("font-weight: 700; color: ", octid_colors$primary_dark,
                           "; margin-bottom: 0.3em; font-size: 0.95em;"), title),
        div(style = "color: #323130; font-size: 0.9em; line-height: 1.6;", body)
      )
    )
  }
  
  # Helper: "does not do" item
  not_do_item <- function(text) {
    div(
      style = "display: flex; align-items: flex-start; gap: 0.75em; padding: 0.5em 0;",
      icon("times circle",
           style = paste0("color: ", octid_colors$danger, "; margin-top: 0.15em; flex-shrink: 0;")),
      div(style = "color: #323130; font-size: 0.93em; line-height: 1.5;", text)
    )
  }
  
  # Helper: tech badge
  tech_badge <- function(label, detail, color = octid_colors$primary) {
    div(
      style = paste0(
        "padding: 0.8em 1.2em; border-radius: 6px; ",
        "background: ", octid_colors$bg_light_blue, "; ",
        "border: 1px solid ", octid_colors$border, ";"
      ),
      div(style = paste0("font-weight: 700; color: ", color, "; font-size: 0.95em;"), label),
      div(style = "color: #605E5C; font-size: 0.82em; margin-top: 0.2em;", detail)
    )
  }
  
  tagList(
    
    # ── Page header ────────────────────────────────────────────────────────────
    div(
      style = paste0(
        "padding: 1.5em 2em; margin-bottom: 1.5em; border-radius: 6px; ",
        "background: ", octid_colors$primary_dark, "; color: white;"
      ),
      div(
        style = "display: flex; justify-content: space-between; align-items: flex-start;",
        div(
          h2(style = "color: white; margin: 0 0 0.4em 0; font-size: 1.4em;",
             "Methods & Governance"),
          div(
            style = "color: rgba(255,255,255,0.8); font-size: 0.93em; line-height: 1.6; max-width: 700px;",
            "This tab documents the analytical scope, data provenance, known limitations, ",
            "and explicit boundaries of the OCTID dashboard. Transparent methodology ",
            "is a core design principle — not an afterthought."
          )
        ),
        uiOutput(ns("header_cache_badge"))
      )
    ),
    
    # ── Two-column layout: left (main) + right (sidebar) ──────────────────────
    div(
      class = "ui two column stackable grid",
      
      # ── LEFT COLUMN ───────────────────────────────────────────────────────
      div(
        class = "eleven wide column",
        
        # 1. Purpose & Scope
        methods_card(
          border_color = octid_colors$primary,
          methods_heading("bullseye", "Purpose & Scope"),
          div(
            style = "color: #323130; font-size: 0.93em; line-height: 1.7;",
            p(
              "OCTID answers one question: ",
              tags$em("what is the competitive trial landscape in oncology and rare disease right now?"),
              " It is designed for CI analysts, Medical Affairs, and Commercial Strategy teams ",
              "who need rapid situational awareness before a business development conversation, ",
              "portfolio review, or launch strategy meeting."
            ),
            p(
              "The dashboard surfaces structural patterns — sponsor concentration, endpoint frequency, ",
              "geographic footprint — from a curated snapshot of active Phase 2/3 and Phase 3 trials. ",
              "It is intended as a starting point for deeper investigation, not a definitive source."
            )
          )
        ),
        
        # 2. Data Source
        methods_card(
          border_color = octid_colors$accent,
          methods_heading("database", "Data Source", color = octid_colors$primary_dark),
          div(
            class = "ui two column stackable grid",
            div(
              class = "column",
              div(style = paste0("font-weight: 700; color: ", octid_colors$primary_dark,
                                 "; margin-bottom: 0.5em; font-size: 0.9em;"),
                  "PRIMARY SOURCE"),
              div(style = "color: #323130; font-size: 0.88em; line-height: 1.7;",
                  div(tags$b("ClinicalTrials.gov API v2")),
                  div("National Library of Medicine (NLM)"),
                  div("National Institutes of Health (NIH)"),
                  div(style = "margin-top: 0.5em;",
                      tags$a(href = "https://clinicaltrials.gov/api/v2/studies",
                             target = "_blank",
                             style = paste0("color: ", octid_colors$primary, ";"),
                             "clinicaltrials.gov/api/v2/studies"))
              )
            ),
            div(
              class = "column",
              div(style = paste0("font-weight: 700; color: ", octid_colors$primary_dark,
                                 "; margin-bottom: 0.5em; font-size: 0.9em;"),
                  "QUERY STRATEGY"),
              div(style = "color: #323130; font-size: 0.88em; line-height: 1.7;",
                  div("Keyword: cancer OR oncology OR tumor OR neoplasm OR carcinoma OR lymphoma OR leukemia OR sarcoma OR melanoma OR myeloma"),
                  div(style = "margin-top: 0.4em;", "Statuses: RECRUITING + ACTIVE_NOT_RECRUITING"),
                  div("Phases: PHASE3 + PHASE2|PHASE3"),
                  div(style = "margin-top: 0.4em; font-style: italic; color: #605E5C;",
                      "Rare Disease subset identified via NLM rare disease condition tags")
              )
            )
          )
        ),
        
        # 4. Known Limitations
        methods_card(
          border_color = octid_colors$warning,
          methods_heading("exclamation triangle", "Known Limitations",
                          color = octid_colors$warning),
          div(
            style = paste0(
              "padding: 0.6em 1em; margin-bottom: 1em; border-radius: 4px; ",
              "background: #FFF8F0; color: #605E5C; font-size: 0.88em;"
            ),
            "These limitations are disclosed as locked analytical facts. ",
            "They reflect the boundaries of what the data can and cannot support — ",
            "not errors in implementation."
          ),
          
          limitation_item(
            "1", "PHASE2 | PHASE3 Combined Classification",
            "The ClinicalTrials.gov API returns both phase values as a combined string
            for basket and umbrella trials that span both phases. 278 trials in the cache
            carry this classification. This is an API behavior, not a parsing error,
            and these trials are retained and labeled accordingly throughout the dashboard."
          ),
          
          limitation_item(
            "2", "Endpoint Text Coverage",
            "95.8% of Phase 3 oncology trials have at least one primary endpoint text
            field populated. Approximately 4% of trials have no endpoint text available
            in the API response. These trials appear in trial counts but are excluded
            from endpoint frequency analysis."
          ),
          
          limitation_item(
            "3", "Adjacent Therapeutic Area Contamination",
            "The broad keyword query captures approximately 1–2% of trials from adjacent
            therapeutic areas (e.g., psoriasis, menstrual disorders observed in Phase 1
            probe sampling). These trials are not filtered out. Their presence is disclosed
            here and should be considered when interpreting indication-level counts."
          ),
          
          limitation_item(
            "4", "Free-Text Endpoint Normalization",
            "Endpoint terms are raw free-text from the API. The endpoint frequency
            function applies regex pattern matching (e.g., 'progression.free survival|\\bpfs\\b').
            This is approximate, not canonical. Variant capitalization and phrasing —
            'PFS', 'Progression Free Survival', 'progression-free survival' — may split
            counts across categories."
          ),
          
          limitation_item(
            "5", "Rare Disease Sample Size",
            "The Rare Disease subset contains 176 trials. This is sufficient for
            structural contrast with the 1,934-trial Oncology cohort, but not for
            granular sub-analysis by indication or sponsor. The dashboard does not
            present Rare Disease findings as statistically comparable to Oncology
            at the sub-group level."
          ),
          
          limitation_item(
            "6", "Cache Freshness & Registry Lag",
            "All overview data reflects a point-in-time snapshot — not a real-time feed.
            Individual trial drill-down modals fetch live data from the ClinicalTrials.gov
            API on demand. Trial status, enrollment figures, and sponsor information may
            have changed since the cache was captured. Registry updates may also lag
            sponsor operational reality; status and enrollment fields are self-reported
            by study teams and are not independently verified."
          ),
          
          div(
            style = paste0(
              "display: flex; gap: 1em; padding: 1em 0;"
            ),
            div(
              style = paste0(
                "flex-shrink: 0; width: 28px; height: 28px; border-radius: 50%; ",
                "background: ", octid_colors$primary, "; color: white; ",
                "display: flex; align-items: center; justify-content: center; ",
                "font-size: 0.82em; font-weight: 700;"
              ),
              "7"
            ),
            div(
              div(style = paste0("font-weight: 700; color: ", octid_colors$primary_dark,
                                 "; margin-bottom: 0.3em; font-size: 0.95em;"),
                  "Scope Boundaries"),
              div(style = "color: #323130; font-size: 0.9em; line-height: 1.6;",
                  "The cache includes RECRUITING and ACTIVE_NOT_RECRUITING trials only. ",
                  "COMPLETED trials are excluded by design from the overview cache — they ",
                  "are accessible via live API drill-down for individual NCT IDs. ",
                  "PHASE1 and PHASE4 trials are excluded by design across all views.")
            )
          )
        ),
        
        # 5. Endpoint Normalization
        methods_card(
          border_color = octid_colors$primary,
          methods_heading("code", "Endpoint Normalization"),
          div(
            style = "color: #323130; font-size: 0.9em; line-height: 1.7;",
            p(
              "The Pipeline & Endpoints tab displays endpoint frequency derived from the ",
              tags$code("primary_endpoint"), " field returned by the ClinicalTrials.gov API. ",
              "This field contains free-text descriptions entered by study coordinators — ",
              "there is no controlled vocabulary enforced at submission."
            ),
            p(
              "Endpoint terms are normalized using regex pattern matching. For example, ",
              "progression-free survival is captured by the pattern ",
              tags$code("progression.free survival|\\bpfs\\b"), ", which matches common ",
              "variants. Terms that do not match any defined pattern are categorized as ",
              tags$em("Other / Unclassified"), "."
            ),
            div(
              style = paste0(
                "padding: 0.8em 1em; border-radius: 4px; margin-top: 0.5em; ",
                "background: #F5F5F5; font-size: 0.88em; color: #605E5C;"
              ),
              tags$b("Implication: "),
              "Endpoint counts are approximate. Two trials measuring the same endpoint ",
              "using different phrasing may be counted separately. Users should treat ",
              "endpoint frequency as a directional signal, not a precise audit."
            )
          )
        )
      ),
      
      # ── RIGHT COLUMN (sidebar) ─────────────────────────────────────────────
      div(
        class = "five wide column",
        
        # 3. Cache Scope (sidebar card)
        div(
          class = "ui segment",
          style = paste0(
            "padding: 1.5em; margin-bottom: 1.5em; ",
            "border-top: 3px solid ", octid_colors$primary, ";"
          ),
          div(style = paste0("font-weight: 700; color: ", octid_colors$primary_dark,
                             "; margin-bottom: 1em; font-size: 0.95em;"),
              icon("server"), " Cache Scope"),
          
          # Stats — dynamic from cache_meta
          uiOutput(ns("cache_scope_stats")),
          
          # Scope details
          div(
            style = "font-size: 0.85em; color: #323130; line-height: 1.8;",
            div(icon("check circle", style = paste0("color: ", octid_colors$success, ";")),
                " RECRUITING"),
            div(icon("check circle", style = paste0("color: ", octid_colors$success, ";")),
                " ACTIVE_NOT_RECRUITING"),
            div(icon("check circle", style = paste0("color: ", octid_colors$success, ";")),
                " PHASE3"),
            div(icon("check circle", style = paste0("color: ", octid_colors$success, ";")),
                " PHASE2 | PHASE3"),
            div(style = "margin-top: 0.5em;",
                icon("times circle", style = paste0("color: ", octid_colors$danger, ";")),
                " COMPLETED (excluded)"),
            div(icon("times circle", style = paste0("color: ", octid_colors$danger, ";")),
                " PHASE1 (excluded)"),
            div(icon("times circle", style = paste0("color: ", octid_colors$danger, ";")),
                " PHASE4 (excluded)")
          )
        ),
        
        # 6. What This App Does NOT Do
        div(
          class = "ui segment",
          style = paste0(
            "padding: 1.5em; margin-bottom: 1.5em; ",
            "border-top: 3px solid ", octid_colors$danger, ";"
          ),
          div(style = paste0("font-weight: 700; color: ", octid_colors$primary_dark,
                             "; margin-bottom: 1em; font-size: 0.95em;"),
              icon("ban", style = paste0("color: ", octid_colors$danger, ";")),
              " What This App Does NOT Do"),
          
          not_do_item("Predict whether a specific trial will succeed or fail"),
          not_do_item("Recommend investment, licensing, or portfolio decisions"),
          not_do_item("Model competitive response timing or market dynamics"),
          not_do_item("Estimate NPV, revenue potential, or financial returns"),
          not_do_item("Replace clinical, regulatory, or business development judgment"),
          not_do_item("Provide real-time trial status — data reflects the cache snapshot"),
          not_do_item("Perform causal analysis — all findings are associative and descriptive")
        ),
        
        # 7. Tech Stack
        div(
          class = "ui segment",
          style = paste0(
            "padding: 1.5em; margin-bottom: 1.5em; ",
            "border-top: 3px solid ", octid_colors$accent, ";"
          ),
          div(style = paste0("font-weight: 700; color: ", octid_colors$primary_dark,
                             "; margin-bottom: 1em; font-size: 0.95em;"),
              icon("cogs"), " Tech Stack"),
          
          div(
            style = "display: flex; flex-direction: column; gap: 0.6em;",
            tech_badge("R + Shiny", "Core application framework"),
            tech_badge("shiny.semantic", "Appsilon UI framework (Semantic UI)"),
            tech_badge("ggiraph", "Interactive ggplot2 visualizations"),
            tech_badge("reactable", "Interactive data tables"),
            tech_badge("leaflet", "Geographic bubble map"),
            tech_badge("httr2", "ClinicalTrials.gov API v2 calls"),
            tech_badge("dplyr + tidyr", "Data wrangling pipeline")
          )
        ),
        
        # 8. Contact / GitHub
        div(
          class = "ui segment",
          style = paste0("padding: 1.5em; border-top: 3px solid ", octid_colors$primary, ";"),
          div(style = paste0("font-weight: 700; color: ", octid_colors$primary_dark,
                             "; margin-bottom: 1em; font-size: 0.95em;"),
              icon("user"), " About"),
          
          div(style = "font-size: 0.88em; color: #323130; line-height: 1.8;",
              div(tags$b("Author:"), " Steven Ponce"),
              div(tags$b("Role:"), " Pharmaceutical Commercial Analytics"),
              div(style = "margin-top: 0.8em;",
                  tags$a(
                    href   = "https://github.com/poncest/oncology-trial-intelligence",
                    target = "_blank",
                    style  = paste0("color: ", octid_colors$primary, "; font-weight: 600;"),
                    icon("github"), " View on GitHub"
                  )
              ),
              div(
                style = paste0(
                  "margin-top: 1em; padding: 0.8em; border-radius: 4px; ",
                  "background: #F5F5F5; font-size: 0.85em; color: #605E5C; ",
                  "font-style: italic; line-height: 1.5;"
                ),
                "This dashboard is a portfolio project demonstrating competitive intelligence ",
                "analytics for pharmaceutical strategy. Data is sourced from the public ",
                "ClinicalTrials.gov registry."
              )
          )
        )
      ) # end right column
    ) # end grid
  )
}


# =============================================================================
# SERVER — static module, no reactive logic needed
# =============================================================================

methods_server <- function(id, cache) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ── Dynamic header badge ──────────────────────────────────────────────────
    output$header_cache_badge <- renderUI({
      div(
        style = paste0(
          "flex-shrink: 0; padding: 0.5em 1em; border-radius: 4px; ",
          "background: rgba(255,255,255,0.12); font-size: 0.82em; ",
          "color: rgba(255,255,255,0.9); text-align: center;"
        ),
        div(style = "font-weight: 700;", "Cache Date"),
        div(cache_meta$last_updated),
        div(style = "margin-top: 0.3em; font-weight: 700;", "Trials"),
        div(scales::comma(cache_meta$total_trial_count), " total")
      )
    })
    
    # ── Dynamic sidebar stat tiles ────────────────────────────────────────────
    output$cache_scope_stats <- renderUI({
      div(
        style = "display: grid; grid-template-columns: 1fr 1fr; gap: 0.8em; margin-bottom: 1em;",
        
        div(style = paste0("text-align: center; padding: 0.8em; border-radius: 4px; background: ",
                           octid_colors$bg_light_blue, ";"),
            div(style = paste0("font-size: 1.6em; font-weight: 700; color: ", octid_colors$primary, ";"),
                scales::comma(cache_meta$total_trial_count)),
            div(style = "font-size: 0.78em; color: #605E5C;", "Total trials")),
        
        div(style = paste0("text-align: center; padding: 0.8em; border-radius: 4px; background: ",
                           octid_colors$bg_light_blue, ";"),
            div(style = paste0("font-size: 1.6em; font-weight: 700; color: ", octid_colors$primary, ";"),
                scales::comma(cache_meta$oncology_trial_count)),
            div(style = "font-size: 0.78em; color: #605E5C;", "Oncology")),
        
        div(style = paste0("text-align: center; padding: 0.8em; border-radius: 4px; background: ",
                           octid_colors$bg_light_purple, ";"),
            div(style = paste0("font-size: 1.6em; font-weight: 700; color: ", octid_colors$rare_disease, ";"),
                scales::comma(cache_meta$rare_disease_count)),
            div(style = "font-size: 0.78em; color: #605E5C;", "Rare Disease")),
        
        div(style = paste0("text-align: center; padding: 0.8em; border-radius: 4px; background: ",
                           octid_colors$bg_light_blue, ";"),
            div(style = paste0("font-size: 1.6em; font-weight: 700; color: ", octid_colors$accent, ";"),
                paste0(cache_meta$endpoint_coverage_pct, "%")),
            div(style = "font-size: 0.78em; color: #605E5C;", "Endpoint coverage"))
      )
    })
    
    # ── outputOptions — CSS tab fix ───────────────────────────────────────────
    outputOptions(output, "header_cache_badge", suspendWhenHidden = FALSE)
    outputOptions(output, "cache_scope_stats",  suspendWhenHidden = FALSE)
    
  })
}