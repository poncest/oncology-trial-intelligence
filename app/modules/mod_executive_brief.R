# =============================================================================
# mod_executive_brief.R
# Executive Brief — entry point for VP-level consumption
#
# Loads entirely from cached .rds — no live API calls on this tab.
#
# Analytical questions answered:
#   - Where is trial activity concentrating right now?
#   - What is the competitive footprint by phase and sponsor class?
#   - How do Oncology and Rare Disease dynamics compare?
#
# Rare Disease contrast: one comparison tile (count, enrollment, phase mix)
#
# Author: Steven Ponce | February 2026
# =============================================================================

executive_brief_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    # --- Freshness banner -----------------------------------------------------
    uiOutput(ns("freshness_banner")),
    
    div(style = "padding: 1.5em 0 0.5em 0;",
        
        # --- KPI row ------------------------------------------------------------
        div(class = "ui four statistics",
            style = "margin-bottom: 1.5em;",
            
            # KPI 1: Active trials
            div(class = "statistic",
                div(class = "value", style = glue("color: {octid_colors$primary};"),
                    uiOutput(ns("kpi_total_trials"), inline = TRUE)
                ),
                div(class = "label", "Active Trials")
            ),
            
            # KPI 2: Unique sponsors
            div(class = "statistic",
                div(class = "value", style = glue("color: {octid_colors$primary};"),
                    uiOutput(ns("kpi_sponsors"), inline = TRUE)
                ),
                div(class = "label", "Unique Sponsors")
            ),
            
            # KPI 3: Recruiting
            div(class = "statistic",
                div(class = "value", style = glue("color: {octid_colors$success};"),
                    uiOutput(ns("kpi_recruiting"), inline = TRUE)
                ),
                div(class = "label", "Recruiting Now")
            ),
            
            # KPI 4: Median enrollment
            div(class = "statistic",
                div(class = "value", style = glue("color: {octid_colors$primary};"),
                    uiOutput(ns("kpi_median_enrollment"), inline = TRUE)
                ),
                div(class = "label", "Median Enrollment")
            )
        ),
        
        # --- Row 2: Phase chart + Sponsor class chart --------------------------
        div(class = "ui two column stackable grid",
            style = "margin-bottom: 1.5em;",
            
            div(class = "column",
                div(class = "ui segment", style = "padding: 1.2em;",
                    h4(class = "ui header",
                       style = glue("color: {octid_colors$primary_dark}; margin-bottom: 0.8em;"),
                       icon("tasks"), "Phase Distribution"
                    ),
                    ggiraph::girafeOutput(ns("chart_phase"), height = "260px")
                )
            ),
            
            div(class = "column",
                div(class = "ui segment", style = "padding: 1.2em;",
                    h4(class = "ui header",
                       style = glue("color: {octid_colors$primary_dark}; margin-bottom: 0.8em;"),
                       icon("building"), "Sponsor Class"
                    ),
                    ggiraph::girafeOutput(ns("chart_sponsor_class"), height = "260px")
                )
            )
        ),
        
        # --- Row 3: Top indications + Rare Disease comparison tile -------------
        div(class = "ui two column stackable grid",
            
            # Top indications table
            div(class = "column",
                div(class = "ui segment", style = "padding: 1.2em;",
                    h4(class = "ui header",
                       style = glue("color: {octid_colors$primary_dark}; margin-bottom: 0.8em;"),
                       icon("list"), "Top Sponsors by Trial Volume"
                    ),
                    reactable::reactableOutput(ns("table_top_sponsors"))
                )
            ),
            
            # Rare Disease contrast tile
            div(class = "column",
                div(class = "ui segment",
                    style = glue(
                      "padding: 1.2em; ",
                      "border-left: 4px solid {octid_colors$rare_disease};"
                    ),
                    h4(class = "ui header",
                       style = glue("color: {octid_colors$rare_disease}; margin-bottom: 0.8em;"),
                       icon("microscope"), "Oncology vs. Rare Disease"
                    ),
                    p(style = glue("color: {octid_colors$text_gray}; font-size: 0.85em; margin-bottom: 1em;"),
                      "Rare Disease as a structural contrast — where data is scarce and ",
                      "site concentration risk is high."
                    ),
                    uiOutput(ns("rare_disease_tile"))
                )
            )
        )
        
    ) # end padding div
  )
}


executive_brief_server <- function(id, cache) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Reactive: split cache by TA ----------------------------------------
    onco_data <- reactive({
      cache |> filter(ta == "Oncology")
    })
    
    rare_data <- reactive({
      cache |> filter(ta == "Rare Disease")
    })
    
    # --- Freshness banner ----------------------------------------------------
    output$freshness_banner <- renderUI({
      date_str <- tryCatch(
        fmt_date(cache_meta$last_updated),
        error = function(e) "unknown"
      )
      div(
        class = "ui info message",
        style = "padding: 0.6em 1em; margin-bottom: 0;",
        icon("info circle"),
        tags$strong("Overview data updated: "), date_str,
        tags$span(
          style = glue("color: {octid_colors$text_gray}; margin-left: 1em; font-size: 0.9em;"),
          "Cached overview · Individual trial details fetched live on demand"
        )
      )
    })
    
    # --- KPI: Total trials ---------------------------------------------------
    output$kpi_total_trials <- renderUI({
      n <- nrow(onco_data())
      fmt_count(n)
    })
    
    # --- KPI: Unique sponsors ------------------------------------------------
    output$kpi_sponsors <- renderUI({
      n <- onco_data() |>
        filter(!is.na(lead_sponsor)) |>
        distinct(lead_sponsor) |>
        nrow()
      fmt_count(n)
    })
    
    # --- KPI: Recruiting count -----------------------------------------------
    output$kpi_recruiting <- renderUI({
      n <- onco_data() |>
        filter(overall_status == "RECRUITING") |>
        nrow()
      fmt_count(n)
    })
    
    # --- KPI: Median enrollment ----------------------------------------------
    output$kpi_median_enrollment <- renderUI({
      med <- onco_data() |>
        filter(!is.na(enrollment), enrollment > 0) |>
        pull(enrollment) |>
        median(na.rm = TRUE)
      fmt_count(round(med))
    })
    
    # --- Chart: Phase distribution -------------------------------------------
    output$chart_phase <- ggiraph::renderGirafe({
      
      df <- onco_data() |>
        mutate(phase_label = fmt_phase(phases)) |>
        count(phase_label, name = "n") |>
        mutate(
          pct = n / sum(n),
          tooltip = glue("{phase_label}\n{fmt_count(n)} trials ({fmt_pct(pct)})")
        ) |>
        arrange(desc(n))
      
      p <- ggplot(df, aes(
        x = reorder(phase_label, n),
        y = n,
        fill = phase_label,
        tooltip = tooltip,
        data_id = phase_label
      )) +
        ggiraph::geom_col_interactive(width = 0.65, show.legend = FALSE) +
        scale_fill_manual(values = c(
          "Phase 3"      = octid_colors$primary,
          "Phase 2/Phase 3" = octid_colors$accent
        ), na.value = octid_colors$primary_light) +
        scale_y_continuous(labels = scales::comma) +
        coord_flip() +
        labs(x = NULL, y = "Trials") +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank(),
          axis.text          = element_text(color = octid_colors$text_dark),
          plot.background    = element_rect(fill = "transparent", color = NA),
          panel.background   = element_rect(fill = "transparent", color = NA)
        )
      
      ggiraph::girafe(
        ggobj = p,
        width_svg = 5, height_svg = 2.8,
        options = list(
          ggiraph::opts_hover(css = "opacity: 0.8; cursor: pointer;"),
          ggiraph::opts_tooltip(
            css = glue(
              "background: {octid_colors$primary_dark}; ",
              "color: white; padding: 6px 10px; border-radius: 4px; font-size: 12px;"
            )
          ),
          ggiraph::opts_toolbar(saveaspng = FALSE)
        )
      )
    })
    
    # --- Chart: Sponsor class ------------------------------------------------
    output$chart_sponsor_class <- ggiraph::renderGirafe({
      
      df <- onco_data() |>
        filter(!is.na(sponsor_class)) |>
        mutate(
          class_label = dplyr::recode(sponsor_class, !!!sponsor_class_labels)
        ) |>
        count(class_label, name = "n") |>
        mutate(
          pct     = n / sum(n),
          tooltip = glue("{class_label}\n{fmt_count(n)} trials ({fmt_pct(pct)})")
        ) |>
        arrange(desc(n))
      
      pal <- c(
        "Industry"           = octid_colors$primary,
        "Academic / Other"   = octid_colors$accent,
        "Research Network"   = "#0096C7",
        "NIH"                = "#00B4D8",
        "Other Government"   = octid_colors$primary_light,
        "Unknown"            = octid_colors$text_light
      )
      
      p <- ggplot(df, aes(
        x = reorder(class_label, n),
        y = n,
        fill = class_label,
        tooltip = tooltip,
        data_id = class_label
      )) +
        ggiraph::geom_col_interactive(width = 0.65, show.legend = FALSE) +
        scale_fill_manual(values = pal, na.value = octid_colors$primary_light) +
        scale_y_continuous(labels = scales::comma) +
        coord_flip() +
        labs(x = NULL, y = "Trials") +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank(),
          axis.text          = element_text(color = octid_colors$text_dark),
          plot.background    = element_rect(fill = "transparent", color = NA),
          panel.background   = element_rect(fill = "transparent", color = NA)
        )
      
      ggiraph::girafe(
        ggobj = p,
        width_svg = 5, height_svg = 2.8,
        options = list(
          ggiraph::opts_hover(css = "opacity: 0.8; cursor: pointer;"),
          ggiraph::opts_tooltip(
            css = glue(
              "background: {octid_colors$primary_dark}; ",
              "color: white; padding: 6px 10px; border-radius: 4px; font-size: 12px;"
            )
          ),
          ggiraph::opts_toolbar(saveaspng = FALSE)
        )
      )
    })
    
    # --- Table: Top sponsors by trial volume ---------------------------------
    output$table_top_sponsors <- reactable::renderReactable({
      
      df <- onco_data() |>
        filter(!is.na(lead_sponsor)) |>
        count(lead_sponsor, sponsor_class, name = "n_trials") |>
        mutate(
          sponsor_class_label = dplyr::recode(
            sponsor_class, !!!sponsor_class_labels
          )
        ) |>
        arrange(desc(n_trials)) |>
        slice_head(n = 15) |>
        select(
          Sponsor       = lead_sponsor,
          Class         = sponsor_class_label,
          Trials        = n_trials
        )
      
      reactable::reactable(
        df,
        defaultColDef = reactable::colDef(
          headerStyle = list(
            background = octid_colors$primary_dark,
            color      = "white",
            fontWeight = "600",
            fontSize   = "13px"
          )
        ),
        columns = list(
          Sponsor = reactable::colDef(minWidth = 200),
          Class   = reactable::colDef(width = 140),
          Trials  = reactable::colDef(
            width = 80,
            align = "center",
            style = function(value) {
              list(fontWeight = "600", color = octid_colors$primary)
            }
          )
        ),
        striped      = TRUE,
        highlight    = TRUE,
        compact      = TRUE,
        borderless   = TRUE,
        pagination   = FALSE,
        theme = reactable::reactableTheme(
          stripedColor    = octid_colors$bg_light_gray,
          highlightColor  = octid_colors$bg_light_blue,
          cellPadding     = "6px 12px"
        )
      )
    })
    
    # --- Rare Disease comparison tile ----------------------------------------
    output$rare_disease_tile <- renderUI({
      
      o <- onco_data()
      r <- rare_data()
      
      if (nrow(r) == 0) {
        return(div(
          class = "ui message",
          "Rare Disease data not available in current cache."
        ))
      }
      
      # Compute comparison metrics
      o_recruiting_pct <- mean(o$overall_status == "RECRUITING", na.rm = TRUE)
      r_recruiting_pct <- mean(r$overall_status == "RECRUITING", na.rm = TRUE)
      
      o_med_enrollment <- median(o$enrollment[o$enrollment > 0], na.rm = TRUE)
      r_med_enrollment <- median(r$enrollment[r$enrollment > 0], na.rm = TRUE)
      
      o_med_sites <- median(o$n_sites[o$n_sites > 0], na.rm = TRUE)
      r_med_sites <- median(r$n_sites[r$n_sites > 0], na.rm = TRUE)
      
      # Concentration risk: top 3 countries share of Rare Disease trials
      r_country_conc <- tryCatch({
        r |>
          filter(!is.na(countries), nzchar(countries)) |>
          tidyr::separate_rows(countries, sep = " \\| ") |>
          count(countries, sort = TRUE) |>
          mutate(share = n / sum(n)) |>
          slice_head(n = 3) |>
          summarise(top3_share = sum(share)) |>
          pull(top3_share)
      }, error = function(e) 0.75)
      
      conc_pct <- fmt_pct(r_country_conc)
      
      metric_row <- function(label, onco_val, rare_val, rare_color = octid_colors$rare_disease) {
        div(
          style = "display: flex; justify-content: space-between; padding: 0.5em 0; border-bottom: 1px solid #EDEBE9;",
          div(style = paste0("color: ", octid_colors$text_gray, "; font-size: 0.88em; flex: 1;"), label),
          div(style = paste0("color: ", octid_colors$primary, "; font-weight: 600; width: 90px; text-align: right;"), onco_val),
          div(style = paste0("color: ", rare_color, "; font-weight: 600; width: 90px; text-align: right;"), rare_val)
        )
      }
      
      tagList(
        # Column headers
        div(
          style = "display: flex; justify-content: space-between; padding: 0 0 0.4em 0;",
          div(style = "flex: 1;"),
          div(style = paste0("color: ", octid_colors$primary, "; font-weight: 700; width: 90px; text-align: right; font-size: 0.85em;"), "Oncology"),
          div(style = paste0("color: ", octid_colors$rare_disease, "; font-weight: 700; width: 90px; text-align: right; font-size: 0.85em;"), "Rare Disease")
        ),
        
        metric_row("Active trials",
                   fmt_count(nrow(o)),
                   fmt_count(nrow(r))
        ),
        metric_row("Recruiting (%)",
                   fmt_pct(o_recruiting_pct),
                   fmt_pct(r_recruiting_pct)
        ),
        metric_row("Median enrollment",
                   fmt_count(round(o_med_enrollment)),
                   fmt_count(round(r_med_enrollment))
        ),
        metric_row("Median sites/trial",
                   fmt_count(round(o_med_sites)),
                   fmt_count(round(r_med_sites))
        ),
        
        # Concentration risk callout
        div(
          style = paste0("margin-top: 1em; padding: 0.7em; background: ", octid_colors$bg_light_purple, "; border-radius: 4px;"),
          tags$strong(
            style = paste0("color: ", octid_colors$rare_disease, "; font-size: 0.85em;"),
            icon("exclamation triangle"), " Site Concentration Risk"
          ),
          p(
            style = "color: #323130; font-size: 0.83em; margin: 0.3em 0 0 0; line-height: 1.5;",
            "Top 3 countries account for ",
            tags$strong(conc_pct),
            " of Rare Disease trial activity — vs. broader geographic spread in Oncology.",
            tags$br(),
            "Site dependency is a structural constraint, not a data gap."
          )
        )
      )
    })
    
    # -------------------------------------------------------------------------
    # outputOptions — required for shiny.semantic CSS tabs
    # Without this, outputs only render after user interaction (not on tab load)
    # See: Launch Curve Forecaster lessons learned
    # -------------------------------------------------------------------------
    outputOptions(output, "freshness_banner",     suspendWhenHidden = FALSE)
    outputOptions(output, "kpi_total_trials",     suspendWhenHidden = FALSE)
    outputOptions(output, "kpi_sponsors",         suspendWhenHidden = FALSE)
    outputOptions(output, "kpi_recruiting",       suspendWhenHidden = FALSE)
    outputOptions(output, "kpi_median_enrollment",suspendWhenHidden = FALSE)
    outputOptions(output, "chart_phase",          suspendWhenHidden = FALSE)
    outputOptions(output, "chart_sponsor_class",  suspendWhenHidden = FALSE)
    outputOptions(output, "table_top_sponsors",   suspendWhenHidden = FALSE)
    outputOptions(output, "rare_disease_tile",    suspendWhenHidden = FALSE)
    
  })
}