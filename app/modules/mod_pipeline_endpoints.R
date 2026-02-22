# =============================================================================
# mod_pipeline_endpoints.R
# Pipeline & Endpoints — analytical core
#
# Three panels:
#   1. Filter sidebar  — TA, Phase, Status, Sponsor Class, free-text
#   2. Trial table     — reactable with row-click drill-down (live API)
#   3. Endpoint panel  — frequency bar chart from filtered cache subset
#
# Design rules (from lessons learned):
#   - NO glue() inside renderUI/renderReactable — use paste0() for styles
#   - outputOptions(suspendWhenHidden = FALSE) on ALL outputs
#   - tidyr:: namespace for separate_rows
#   - Inline formatting — no helper function scoping risk
#
# Author: Steven Ponce | February 2026
# =============================================================================

pipeline_endpoints_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    # --- Row count banner -----------------------------------------------------
    uiOutput(ns("filter_banner")),
    
    div(style = "padding: 1em 0 0 0;",
        
        # --- Filter bar (horizontal) -------------------------------------------
        div(class = "ui segment",
            style = "padding: 1em 1.2em; margin-bottom: 1em;",
            
            div(class = "ui stackable four column grid",
                
                # TA filter
                div(class = "column",
                    div(class = "ui tiny header", style = "margin-bottom: 0.4em;", "Therapeutic Area"),
                    selectInput(
                      ns("filter_ta"),
                      label    = NULL,
                      choices  = c("All" = "All", "Oncology" = "Oncology", "Rare Disease" = "Rare Disease"),
                      selected = "All",
                      width    = "100%"
                    )
                ),
                
                # Phase filter
                div(class = "column",
                    div(class = "ui tiny header", style = "margin-bottom: 0.4em;", "Phase"),
                    selectInput(
                      ns("filter_phase"),
                      label    = NULL,
                      choices  = c("All" = "All", "Phase 3" = "PHASE3", "Phase 2/3" = "PHASE2_PHASE3"),
                      selected = "All",
                      width    = "100%"
                    )
                ),
                
                # Status filter
                div(class = "column",
                    div(class = "ui tiny header", style = "margin-bottom: 0.4em;", "Status"),
                    selectInput(
                      ns("filter_status"),
                      label    = NULL,
                      choices  = c(
                        "All"                    = "All",
                        "Recruiting"             = "RECRUITING",
                        "Active (not recruiting)"= "ACTIVE_NOT_RECRUITING"
                      ),
                      selected = "All",
                      width    = "100%"
                    )
                ),
                
                # Sponsor class filter
                div(class = "column",
                    div(class = "ui tiny header", style = "margin-bottom: 0.4em;", "Sponsor Class"),
                    selectInput(
                      ns("filter_sponsor_class"),
                      label    = NULL,
                      choices  = c(
                        "All"              = "All",
                        "Industry"         = "INDUSTRY",
                        "Academic / Other" = "OTHER",
                        "Research Network" = "NETWORK",
                        "NIH"              = "NIH",
                        "Other Government" = "OTHER_GOV"
                      ),
                      selected = "All",
                      width    = "100%"
                    )
                )
            ),
            
            # Free-text search (full width, below filters)
            div(style = "margin-top: 0.6em;",
                div(class = "ui tiny header", style = "margin-bottom: 0.4em;",
                    icon("search"), "Search trials"
                ),
                textInput(
                  ns("filter_search"),
                  label       = NULL,
                  placeholder = "Search by title, sponsor, condition, or NCT ID...",
                  width       = "100%"
                )
            )
        ),
        
        # --- Main content: table + endpoint panel ------------------------------
        div(class = "ui stackable two column grid",
            
            # Left: Trial table (wider)
            div(class = "ten wide column",
                div(class = "ui segment", style = "padding: 1em;",
                    div(
                      style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.8em;",
                      h4(class = "ui header",
                         style = paste0("color: ", octid_colors$primary_dark, "; margin: 0;"),
                         icon("table"), "Active Trials"
                      ),
                      uiOutput(ns("download_btn"))
                    ),
                    p(style = paste0("color: ", octid_colors$text_gray, "; font-size: 0.82em; margin: 0 0 0.8em 0;"),
                      icon("mouse pointer"), " Click any row to view trial details"
                    ),
                    reactable::reactableOutput(ns("trial_table"))
                )
            ),
            
            # Right: Endpoint frequency panel
            div(class = "six wide column",
                div(class = "ui segment", style = "padding: 1em;",
                    h4(class = "ui header",
                       style = paste0("color: ", octid_colors$primary_dark, "; margin-bottom: 0.4em;"),
                       icon("chart bar"), "Primary Endpoint Frequency"
                    ),
                    p(style = paste0("color: ", octid_colors$text_gray, "; font-size: 0.82em; margin: 0 0 0.8em 0;"),
                      "Top endpoint terms from filtered trials. Free-text normalization applied."
                    ),
                    ggiraph::girafeOutput(ns("chart_endpoints"), height = "420px"),
                    uiOutput(ns("endpoint_coverage_note"))
                )
            )
        )
    ),
    
    # --- Drill-down modal -----------------------------------------------------
    div(id = ns("trial_modal"),
        class = "ui modal",
        div(class = "header",
            style = paste0("color: ", octid_colors$primary_dark, ";"),
            icon("flask"), uiOutput(ns("modal_title"), inline = TRUE)
        ),
        div(class = "scrolling content",
            uiOutput(ns("modal_body"))
        ),
        div(class = "actions",
            tags$a(
              id    = ns("modal_ct_link"),
              href  = "#",
              target = "_blank",
              class = "ui primary button",
              icon("external alternate"), "View on ClinicalTrials.gov"
            ),
            div(class = "ui cancel button", "Close")
        )
    )
  )
}


pipeline_endpoints_server <- function(id, cache) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # -------------------------------------------------------------------------
    # REACTIVE: filtered dataset
    # -------------------------------------------------------------------------
    filtered_data <- reactive({
      
      d <- cache
      
      # TA filter
      ta_val <- if (is.null(input$filter_ta)) "All" else input$filter_ta
      if (ta_val != "All") {
        d <- d |> filter(ta == ta_val)
      }
      
      # Phase filter
      phase_val <- if (is.null(input$filter_phase)) "All" else input$filter_phase
      if (phase_val != "All") {
        d <- d |> filter(str_detect(phases, phase_val))
      }
      
      # Status filter
      status_val <- if (is.null(input$filter_status)) "All" else input$filter_status
      if (status_val != "All") {
        d <- d |> filter(overall_status == status_val)
      }
      
      # Sponsor class filter
      sc_val <- if (is.null(input$filter_sponsor_class)) "All" else input$filter_sponsor_class
      if (sc_val != "All") {
        d <- d |> filter(sponsor_class == sc_val)
      }
      
      # Free-text search
      search_val <- if (is.null(input$filter_search)) "" else str_trim(input$filter_search)
      if (nzchar(search_val)) {
        pattern <- fixed(search_val, ignore_case = TRUE)
        d <- d |> filter(
          str_detect(brief_title,  pattern) |
            str_detect(lead_sponsor, pattern) |
            str_detect(conditions,   pattern) |
            str_detect(nct_id,       pattern)
        )
      }
      
      d
    })
    
    # -------------------------------------------------------------------------
    # Filter banner
    # -------------------------------------------------------------------------
    output$filter_banner <- renderUI({
      n_filtered <- nrow(filtered_data())
      n_total    <- nrow(cache)
      div(
        class = "ui info message",
        style = "padding: 0.6em 1em; margin-bottom: 0;",
        icon("filter"),
        tags$strong(fmt_count(n_filtered)),
        paste0(" of ", fmt_count(n_total), " trials shown"),
        if (n_filtered < n_total) {
          tags$span(
            style = paste0("color: ", octid_colors$warning, "; margin-left: 0.8em; font-size: 0.9em;"),
            paste0("(", fmt_count(n_total - n_filtered), " filtered out)")
          )
        }
      )
    })
    
    # -------------------------------------------------------------------------
    # Download button
    # -------------------------------------------------------------------------
    output$download_btn <- renderUI({
      downloadLink(
        ns("download_csv"),
        label = tagList(icon("download"), " Export CSV"),
        class = "ui mini basic button"
      )
    })
    
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("octid_trials_", Sys.Date(), ".csv")
      },
      content = function(file) {
        filtered_data() |>
          select(nct_id, brief_title, overall_status, phases, lead_sponsor,
                 sponsor_class, enrollment, conditions, primary_endpoint,
                 start_date, primary_completion, countries, n_sites, ta) |>
          write.csv(file, row.names = FALSE)
      }
    )
    
    # -------------------------------------------------------------------------
    # Trial table
    # -------------------------------------------------------------------------
    output$trial_table <- reactable::renderReactable({
      
      d <- filtered_data()
      
      if (nrow(d) == 0) {
        return(reactable::reactable(
          data.frame(Message = "No trials match the current filters."),
          columns = list(Message = reactable::colDef(name = ""))
        ))
      }
      
      # Prepare display columns
      display <- d |>
        mutate(
          Status = case_when(
            overall_status == "RECRUITING"             ~ "Recruiting",
            overall_status == "ACTIVE_NOT_RECRUITING"  ~ "Active",
            TRUE ~ overall_status
          ),
          Phase = case_when(
            str_detect(phases, "PHASE2_PHASE3") ~ "Ph 2/3",
            str_detect(phases, "PHASE3")        ~ "Ph 3",
            TRUE ~ phases
          ),
          Sponsor = fmt_truncate(lead_sponsor, 35),
          Title   = fmt_truncate(brief_title,  70),
          Class   = dplyr::recode(sponsor_class, !!!sponsor_class_labels),
          Enroll  = ifelse(is.na(enrollment), "—", fmt_count(enrollment))
        ) |>
        select(
          NCT_ID   = nct_id,
          Title,
          Phase,
          Status,
          Sponsor,
          Class,
          Enroll,
          TA       = ta
        )
      
      reactable::reactable(
        display,
        onClick   = "select",
        selection = "single",
        defaultColDef = reactable::colDef(
          headerStyle = list(
            background = octid_colors$primary_dark,
            color      = "white",
            fontWeight = "600",
            fontSize   = "12px"
          ),
          style = list(fontSize = "12px")
        ),
        columns = list(
          NCT_ID  = reactable::colDef(width = 110, style = list(
            fontFamily = "monospace",
            color      = octid_colors$primary,
            fontWeight = "600"
          )),
          Title   = reactable::colDef(minWidth = 220),
          Phase   = reactable::colDef(width = 65,  align = "center"),
          Status  = reactable::colDef(width = 80,  align = "center",
                                      style = function(value) {
                                        color <- if (value == "Recruiting") octid_colors$success else octid_colors$warning
                                        list(color = color, fontWeight = "600", fontSize = "11px")
                                      }
          ),
          Sponsor = reactable::colDef(minWidth = 150),
          Class   = reactable::colDef(width = 120),
          Enroll  = reactable::colDef(width = 70,  align = "right"),
          TA      = reactable::colDef(width = 90,  align = "center",
                                      style = function(value) {
                                        color <- if (value == "Oncology") octid_colors$primary else octid_colors$rare_disease
                                        list(color = color, fontWeight = "600", fontSize = "11px")
                                      }
          )
        ),
        striped       = TRUE,
        highlight     = TRUE,
        compact       = TRUE,
        borderless    = TRUE,
        showPageSizeOptions = TRUE,
        pageSizeOptions     = c(15, 25, 50),
        defaultPageSize     = 15,
        theme = reactable::reactableTheme(
          stripedColor   = octid_colors$bg_light_gray,
          highlightColor = octid_colors$bg_light_blue,
          cellPadding    = "5px 10px"
        )
      )
    })
    
    # -------------------------------------------------------------------------
    # Row click → drill-down modal
    # -------------------------------------------------------------------------
    selected_row <- reactive({
      reactable::getReactableState("trial_table", "selected")
    })
    
    observeEvent(selected_row(), {
      row_idx <- selected_row()
      req(!is.null(row_idx))
      
      d   <- filtered_data()
      req(nrow(d) >= row_idx)
      
      nct_id <- d$nct_id[row_idx]
      req(!is.na(nct_id), nzchar(nct_id))
      
      # Show modal with loading state immediately
      output$modal_title <- renderUI({ nct_id })
      output$modal_body  <- renderUI({
        div(class = "ui active centered inline loader",
            style = "margin: 2em auto;"
        )
      })
      
      shinyjs::runjs(paste0("$('#", ns("trial_modal"), "').modal('show');"))
      
      # Update CT.gov link
      ct_url <- paste0("https://clinicaltrials.gov/study/", nct_id)
      shinyjs::runjs(paste0(
        "document.getElementById('", ns("modal_ct_link"), "').href = '", ct_url, "';"
      ))
      
      # Fetch live detail from API
      detail <- tryCatch({
        fetch_trial_detail(nct_id)
      }, error = function(e) {
        list(error = conditionMessage(e))
      })
      
      # Render modal body
      output$modal_title <- renderUI({
        if (!is.null(detail$error)) {
          nct_id
        } else {
          detail$brief_title %||% nct_id
        }
      })
      
      output$modal_body <- renderUI({
        if (!is.null(detail$error)) {
          div(class = "ui error message",
              icon("exclamation circle"),
              paste0("Could not fetch trial details: ", detail$error),
              p(style = "margin-top: 0.5em;",
                "You can view this trial directly on ",
                tags$a(href = ct_url, target = "_blank", "ClinicalTrials.gov"), "."
              )
          )
        } else {
          build_modal_body(detail, octid_colors)
        }
      })
      
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    # -------------------------------------------------------------------------
    # Endpoint frequency chart
    # -------------------------------------------------------------------------
    output$chart_endpoints <- ggiraph::renderGirafe({
      
      d <- filtered_data()
      
      # Parse endpoint text from filtered trials
      endpoints_raw <- d |>
        filter(!is.na(primary_endpoint), nzchar(primary_endpoint)) |>
        pull(primary_endpoint)
      
      if (length(endpoints_raw) == 0) {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "No endpoint data\nin filtered trials",
                   color = octid_colors$text_gray, size = 4, hjust = 0.5) +
          theme_void()
        return(ggiraph::girafe(ggobj = p, width_svg = 4, height_svg = 4))
      }
      
      # Normalize and count endpoint terms
      endpoint_df <- build_endpoint_freq(endpoints_raw, top_n = 15) |>
        dplyr::mutate(
          tooltip = paste0(term, "\n", fmt_count(n), " trials (",
                           round(pct * 100, 1), "%)")
        )
      
      p <- ggplot(endpoint_df, aes(
        x       = reorder(term, n),
        y       = n,
        tooltip = tooltip,
        data_id = term
      )) +
        ggiraph::geom_col_interactive(
          width = 0.7, fill = octid_colors$primary, show.legend = FALSE
        ) +
        scale_y_continuous(labels = scales::comma) +
        coord_flip() +
        labs(x = NULL, y = "Trials") +
        theme_minimal(base_size = 11) +
        theme(
          panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank(),
          axis.text.y        = element_text(
            color = octid_colors$text_dark, size = 9
          ),
          axis.text.x        = element_text(color = octid_colors$text_gray),
          plot.background    = element_rect(fill = "transparent", color = NA),
          panel.background   = element_rect(fill = "transparent", color = NA)
        )
      
      ggiraph::girafe(
        ggobj    = p,
        width_svg = 4, height_svg = 5,
        options  = list(
          ggiraph::opts_hover(css = "opacity: 0.8; cursor: pointer;"),
          ggiraph::opts_tooltip(
            css = paste0(
              "background: ", octid_colors$primary_dark, "; ",
              "color: white; padding: 6px 10px; border-radius: 4px; font-size: 11px;"
            )
          ),
          ggiraph::opts_toolbar(saveaspng = FALSE)
        )
      )
    })
    
    # Endpoint coverage note
    output$endpoint_coverage_note <- renderUI({
      d  <- filtered_data()
      n_total    <- nrow(d)
      n_with_ep  <- sum(!is.na(d$primary_endpoint) & nzchar(d$primary_endpoint))
      pct_cov    <- if (n_total > 0) n_with_ep / n_total else 0
      
      div(
        style = paste0(
          "margin-top: 0.5em; padding: 0.5em 0.7em; ",
          "background: ", octid_colors$bg_light_gray, "; ",
          "border-radius: 4px; font-size: 0.8em; ",
          "color: ", octid_colors$text_gray, ";"
        ),
        icon("info circle"),
        paste0(
          fmt_pct(pct_cov), " of filtered trials (",
          fmt_count(n_with_ep), " of ", fmt_count(n_total),
          ") have endpoint text. Free-text — normalization approximate."
        )
      )
    })
    
    # -------------------------------------------------------------------------
    # outputOptions — required for shiny.semantic CSS tabs
    # -------------------------------------------------------------------------
    # modal_title and modal_body are NOT listed here — they are rendered
    # inside observeEvent (user-triggered), not CSS-tab-dependent outputs.
    # outputOptions() only applies to outputs declared at server startup.
    outputOptions(output, "filter_banner",          suspendWhenHidden = FALSE)
    outputOptions(output, "download_btn",           suspendWhenHidden = FALSE)
    outputOptions(output, "trial_table",            suspendWhenHidden = FALSE)
    outputOptions(output, "chart_endpoints",        suspendWhenHidden = FALSE)
    outputOptions(output, "endpoint_coverage_note", suspendWhenHidden = FALSE)
    
  })
}

# =============================================================================
# HELPERS (defined outside module — sourced after global.R)
# =============================================================================

# --- Endpoint frequency builder ----------------------------------------------
build_endpoint_freq <- function(endpoint_strings, top_n = 15) {
  
  # Canonical endpoint terms — ordered by specificity (longer match first)
  canonical_terms <- c(
    "Overall Survival"                   = "overall survival|\\bos\\b",
    "Progression-Free Survival"          = "progression.free survival|\\bpfs\\b",
    "Disease-Free Survival"              = "disease.free survival|\\bdfs\\b",
    "Event-Free Survival"                = "event.free survival|\\befs\\b",
    "Objective Response Rate"            = "objective response rate|\\borr\\b",
    "Complete Response"                  = "complete response|\\bcr\\b|complete remission",
    "Pathologic Complete Response"       = "pathologic.complete response|\\bpcr\\b|pathological complete",
    "Disease Control Rate"               = "disease control rate|\\bdcr\\b",
    "Duration of Response"               = "duration of response|\\bdor\\b",
    "Time to Response"                   = "time to response|\\bttr\\b",
    "Time to Progression"                = "time to progression|\\bttp\\b",
    "Minimal Residual Disease"           = "minimal residual disease|\\bmrd\\b",
    "Overall Response Rate"              = "overall response rate",
    "Clinical Benefit Rate"              = "clinical benefit rate|\\bcbr\\b",
    "Relapse-Free Survival"              = "relapse.free survival|\\brfs\\b",
    "Metastasis-Free Survival"           = "metastasis.free survival|\\bmfs\\b",
    "Safety / Adverse Events"            = "adverse event|safety|tolerability|toxicity|dose.limiting",
    "Quality of Life"                    = "quality of life|\\bqol\\b|\\bhrqol\\b|patient.reported",
    "Biomarker / Pharmacodynamic"        = "biomarker|pharmacodynamic|\\bpd\\b marker",
    "Pharmacokinetics"                   = "pharmacokinetic|\\bpk\\b|area under"
  )
  
  # Lowercase all endpoint strings for matching
  ep_lower <- tolower(endpoint_strings)
  
  # Count matches per canonical term
  counts <- purrr::map_int(canonical_terms, function(pattern) {
    sum(stringr::str_detect(ep_lower, pattern), na.rm = TRUE)
  })
  
  result <- tibble(
    term = names(canonical_terms),
    n    = counts
  ) |>
    filter(n > 0) |>
    mutate(pct = n / length(endpoint_strings)) |>
    arrange(desc(n)) |>
    slice_head(n = top_n)
  
  result
}

# --- Live trial detail fetcher -----------------------------------------------
fetch_trial_detail <- function(nct_id) {
  
  fields <- paste(
    "protocolSection.identificationModule",
    "protocolSection.statusModule",
    "protocolSection.descriptionModule",
    "protocolSection.designModule",
    "protocolSection.conditionsModule",
    "protocolSection.armsInterventionsModule",
    "protocolSection.outcomesModule",
    "protocolSection.eligibilityModule",
    "protocolSection.sponsorCollaboratorsModule",
    "protocolSection.contactsLocationsModule",
    sep = ","
  )
  
  resp <- httr2::request(
    paste0("https://clinicaltrials.gov/api/v2/studies/", nct_id)
  ) |>
    httr2::req_url_query(fields = fields) |>
    httr2::req_timeout(15) |>
    httr2::req_retry(max_tries = 2) |>
    httr2::req_perform()
  
  body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  ps   <- body$protocolSection
  
  # Extract key fields
  list(
    nct_id         = nct_id,
    brief_title    = ps$identificationModule$briefTitle,
    official_title = ps$identificationModule$officialTitle,
    status         = ps$statusModule$overallStatus,
    phase          = paste(unlist(ps$designModule$phases %||% list()), collapse = ", "),
    study_type     = ps$designModule$studyType,
    enrollment     = ps$designModule$enrollmentInfo$count,
    brief_summary  = ps$descriptionModule$briefSummary,
    conditions     = paste(unlist(ps$conditionsModule$conditions %||% list()), collapse = "; "),
    interventions  = purrr::map(
      ps$armsInterventionsModule$interventions %||% list(),
      ~ list(type = .x$type, name = .x$name)
    ),
    primary_outcomes = purrr::map(
      ps$outcomesModule$primaryOutcomes %||% list(),
      ~ list(measure = .x$measure, timeframe = .x$timeFrame)
    ),
    secondary_outcomes = purrr::map(
      ps$outcomesModule$secondaryOutcomes %||% list(),
      ~ list(measure = .x$measure, timeframe = .x$timeFrame)
    ),
    lead_sponsor   = ps$sponsorCollaboratorsModule$leadSponsor$name,
    sponsor_class  = ps$sponsorCollaboratorsModule$leadSponsor$class,
    collaborators  = paste(
      purrr::map_chr(
        ps$sponsorCollaboratorsModule$collaborators %||% list(),
        ~ .x$name %||% ""
      ),
      collapse = "; "
    ),
    eligibility_criteria = ps$eligibilityModule$eligibilityCriteria,
    min_age        = ps$eligibilityModule$minimumAge,
    max_age        = ps$eligibilityModule$maximumAge,
    sex            = ps$eligibilityModule$sex,
    n_locations    = length(ps$contactsLocationsModule$locations %||% list()),
    countries      = paste(
      sort(unique(purrr::map_chr(
        ps$contactsLocationsModule$locations %||% list(),
        ~ .x$country %||% ""
      ))),
      collapse = ", "
    ),
    start_date     = ps$statusModule$startDateStruct$date,
    completion_date = ps$statusModule$primaryCompletionDateStruct$date
  )
}

# --- Modal body builder -------------------------------------------------------
build_modal_body <- function(d, colors) {
  
  section_header <- function(label) {
    div(style = paste0(
      "font-weight: 700; color: ", colors$primary_dark, "; ",
      "font-size: 0.9em; text-transform: uppercase; ",
      "letter-spacing: 0.05em; margin: 1.2em 0 0.4em 0; ",
      "border-bottom: 1px solid ", colors$border, "; padding-bottom: 0.2em;"
    ), label)
  }
  
  detail_row <- function(label, value) {
    if (is.null(value) || (is.character(value) && !nzchar(trimws(value)))) return(NULL)
    div(style = "display: flex; margin-bottom: 0.35em; font-size: 0.88em;",
        div(style = paste0(
          "color: ", colors$text_gray, "; width: 140px; flex-shrink: 0; font-weight: 500;"
        ), label),
        div(style = paste0("color: ", colors$text_dark, "; flex: 1;"), value)
    )
  }
  
  # Status badge
  status_color <- if (identical(d$status, "RECRUITING")) colors$success else colors$warning
  status_label <- if (identical(d$status, "RECRUITING")) "Recruiting" else "Active (not recruiting)"
  
  # Interventions list
  intervention_tags <- if (length(d$interventions) > 0) {
    tagList(purrr::map(d$interventions, function(i) {
      tags$span(
        style = paste0(
          "display: inline-block; margin: 2px; padding: 2px 8px; ",
          "background: ", colors$bg_light_blue, "; ",
          "border-radius: 10px; font-size: 0.82em; color: ", colors$primary_dark, ";"
        ),
        paste0(i$type %||% "", ": ", i$name %||% "")
      )
    }))
  } else { p("None listed.") }
  
  # Primary outcomes list
  primary_outcome_items <- if (length(d$primary_outcomes) > 0) {
    tags$ol(style = "margin: 0; padding-left: 1.2em;",
            purrr::map(d$primary_outcomes, function(o) {
              tags$li(style = "font-size: 0.87em; margin-bottom: 0.3em; color: #323130;",
                      tags$strong(o$measure %||% ""),
                      if (!is.null(o$timeframe) && nzchar(o$timeframe %||% "")) {
                        tags$span(
                          style = paste0("color: ", colors$text_gray, "; margin-left: 0.5em;"),
                          paste0("(", o$timeframe, ")")
                        )
                      }
              )
            })
    )
  } else { p(style = "font-size: 0.87em; color: #605E5C;", "Not specified.") }
  
  tagList(
    
    # Overview strip
    div(style = paste0(
      "display: flex; gap: 1em; flex-wrap: wrap; ",
      "padding: 0.6em; background: ", colors$bg_light_gray, "; ",
      "border-radius: 4px; margin-bottom: 0.5em;"
    ),
    div(style = paste0(
      "padding: 3px 10px; border-radius: 10px; font-size: 0.82em; font-weight: 600; ",
      "background: ", status_color, "; color: white;"
    ), status_label),
    div(style = paste0(
      "padding: 3px 10px; border-radius: 10px; font-size: 0.82em; font-weight: 600; ",
      "background: ", colors$primary, "; color: white;"
    ), d$phase %||% "Phase N/A"),
    if (!is.null(d$enrollment) && !is.na(d$enrollment)) {
      div(style = paste0(
        "font-size: 0.82em; color: ", colors$text_gray, "; padding: 3px 0;"
      ), paste0("Enrollment: ", fmt_count(as.integer(d$enrollment))))
    },
    if (!is.null(d$n_locations) && d$n_locations > 0) {
      div(style = paste0(
        "font-size: 0.82em; color: ", colors$text_gray, "; padding: 3px 0;"
      ), paste0(fmt_count(d$n_locations), " sites · ", d$countries %||% ""))
    }
    ),
    
    # Sponsor
    section_header("Sponsor"),
    detail_row("Lead Sponsor",   d$lead_sponsor),
    detail_row("Class",          dplyr::recode(
      d$sponsor_class %||% "", !!!sponsor_class_labels, .default = d$sponsor_class %||% ""
    )),
    if (nzchar(d$collaborators %||% "")) detail_row("Collaborators", d$collaborators),
    
    # Conditions
    section_header("Conditions"),
    p(style = "font-size: 0.87em; color: #323130; margin: 0;", d$conditions %||% "Not specified"),
    
    # Interventions
    section_header("Interventions"),
    intervention_tags,
    
    # Primary outcomes
    section_header("Primary Endpoints"),
    primary_outcome_items,
    
    # Design
    section_header("Study Design"),
    detail_row("Type",       d$study_type),
    detail_row("Start",      d$start_date),
    detail_row("Completion", d$completion_date),
    detail_row("Sex",        d$sex),
    detail_row("Age range",  paste(
      d$min_age %||% "N/A", "–", d$max_age %||% "N/A"
    )),
    
    # Brief summary (truncated)
    if (!is.null(d$brief_summary) && nzchar(d$brief_summary %||% "")) {
      tagList(
        section_header("Brief Summary"),
        p(style = "font-size: 0.85em; color: #323130; line-height: 1.6; max-height: 120px; overflow-y: auto;",
          substr(d$brief_summary, 1, 600),
          if (nchar(d$brief_summary) > 600) "…"
        )
      )
    }
  )
}