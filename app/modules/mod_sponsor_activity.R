# =============================================================================
# OCTID — mod_sponsor_activity.R
# Sponsor Activity Tab
# Questions answered:
#   - Which sponsors are most active in oncology right now?
#   - Industry vs. academic split — how has it changed?
#   - Which companies are running the most Phase 3 trials?
# =============================================================================

sponsor_activity_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    # ── Filter bar ────────────────────────────────────────────────────────────
    div(
      class = "ui segment",
      style = "background: #F5F5F5; border: none; margin-bottom: 1.5em; padding: 1em 1.5em;",
      
      div(
        class = "ui three column stackable grid",
        
        # Therapeutic Area filter
        div(
          class = "column",
          div(
            style = "font-size: 0.85em; color: #605E5C; margin-bottom: 0.4em; font-weight: 600;",
            "THERAPEUTIC AREA"
          ),
          selectInput(
            ns("filter_ta"),
            label    = NULL,
            choices  = c("All" = "all", "Oncology" = "Oncology", "Rare Disease" = "Rare Disease"),
            selected = "all",
            width    = "100%"
          )
        ),
        
        # Phase filter
        div(
          class = "column",
          div(
            style = "font-size: 0.85em; color: #605E5C; margin-bottom: 0.4em; font-weight: 600;",
            "PHASE"
          ),
          selectInput(
            ns("filter_phase"),
            label    = NULL,
            choices  = c("All Phases" = "all", "Phase 3" = "PHASE3", "Phase 2/3" = "PHASE2|PHASE3"),
            selected = "all",
            width    = "100%"
          )
        ),
        
        # Sponsor class filter
        div(
          class = "column",
          div(
            style = "font-size: 0.85em; color: #605E5C; margin-bottom: 0.4em; font-weight: 600;",
            "SPONSOR TYPE"
          ),
          selectInput(
            ns("filter_sponsor_class"),
            label    = NULL,
            choices  = c(
              "All Types"   = "all",
              "Industry"    = "INDUSTRY",
              "Academic"    = "OTHER",
              "NIH / Govt"  = "NIH"
            ),
            selected = "all",
            width    = "100%"
          )
        )
      )
    ),
    
    # ── KPI row ───────────────────────────────────────────────────────────────
    div(
      class = "ui four column stackable grid",
      style = "margin-bottom: 1.5em;",
      
      # Total sponsors
      div(
        class = "column",
        div(
          class = "ui segment",
          style = paste0(
            "text-align: center; padding: 1.5em 1em; ",
            "border-top: 3px solid ", octid_colors$primary, ";"
          ),
          div(style = paste0("font-size: 2.2em; font-weight: 700; color: ", octid_colors$primary, ";"),
              uiOutput(ns("kpi_total_sponsors"), inline = TRUE)),
          div(style = "font-size: 0.85em; color: #605E5C; margin-top: 0.3em;",
              "Active Sponsors")
        )
      ),
      
      # Industry share
      div(
        class = "column",
        div(
          class = "ui segment",
          style = paste0(
            "text-align: center; padding: 1.5em 1em; ",
            "border-top: 3px solid ", octid_colors$accent, ";"
          ),
          div(style = paste0("font-size: 2.2em; font-weight: 700; color: ", octid_colors$accent, ";"),
              uiOutput(ns("kpi_industry_pct"), inline = TRUE)),
          div(style = "font-size: 0.85em; color: #605E5C; margin-top: 0.3em;",
              "Industry-Sponsored")
        )
      ),
      
      # Median enrollment
      div(
        class = "column",
        div(
          class = "ui segment",
          style = paste0(
            "text-align: center; padding: 1.5em 1em; ",
            "border-top: 3px solid ", octid_colors$success, ";"
          ),
          div(style = paste0("font-size: 2.2em; font-weight: 700; color: ", octid_colors$success, ";"),
              uiOutput(ns("kpi_median_enroll"), inline = TRUE)),
          div(style = "font-size: 0.85em; color: #605E5C; margin-top: 0.3em;",
              "Median Enrollment (top sponsors)")
        )
      ),
      
      # Top sponsor
      div(
        class = "column",
        div(
          class = "ui segment",
          style = paste0(
            "text-align: center; padding: 1.5em 1em; ",
            "border-top: 3px solid ", octid_colors$warning, ";"
          ),
          div(style = paste0("font-size: 1.2em; font-weight: 700; color: ", octid_colors$warning, ";"),
              uiOutput(ns("kpi_top_sponsor"), inline = TRUE)),
          div(style = "font-size: 0.85em; color: #605E5C; margin-top: 0.3em;",
              "Most Active Sponsor")
        )
      )
    ),
    
    # ── Main two-panel layout ─────────────────────────────────────────────────
    div(
      class = "ui two column stackable grid",
      
      # Left: Bar chart
      div(
        class = "ten wide column",
        div(
          class = "ui segment",
          style = "padding: 1.5em;",
          
          div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 1em;",
            div(
              h3(style = paste0("color: ", octid_colors$primary_dark, "; margin: 0;"),
                 "Sponsor Trial Volume"),
              div(style = "color: #605E5C; font-size: 0.9em;",
                  "Top 20 by active trial count — click a bar to see trials"),
              div(style = "color: #605E5C; font-size: 0.82em; margin-top: 0.3em; font-style: italic;",
                  "Share shown as % of trials within current filter selection")
            ),
            div(
              sliderInput(
                ns("top_n"),
                label = "Show top:",
                min   = 5,
                max   = 30,
                value = 20,
                step  = 5,
                width = "160px"
              )
            )
          ),
          
          girafeOutput(ns("sponsor_bar_chart"), height = "480px")
        )
      ),
      
      # Right: Detail panel
      div(
        class = "six wide column",
        div(
          class = "ui segment",
          style = "padding: 1.5em; min-height: 200px;",
          
          uiOutput(ns("detail_panel"))
        )
      )
    ),
    
    # ── Sponsor summary table ─────────────────────────────────────────────────
    div(
      class = "ui segment",
      style = "margin-top: 1.5em; padding: 1.5em;",
      
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 1em;",
        h3(style = paste0("color: ", octid_colors$primary_dark, "; margin: 0;"),
           "Full Sponsor Ranking"),
        div(
          style = "display: flex; gap: 1em; align-items: center;",
          div(style = "color: #605E5C; font-size: 0.9em;",
              uiOutput(ns("table_row_count"), inline = TRUE)),
          downloadLink(
            ns("download_csv"),
            label = tagList(icon("download"), " Export CSV"),
            class = "ui tiny primary button"
          )
        )
      ),
      
      reactableOutput(ns("sponsor_table"))
    )
  )
}


# =============================================================================
# SERVER
# =============================================================================

sponsor_activity_server <- function(id, cache) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ── Reactive: filtered data ───────────────────────────────────────────────
    filtered_trials <- reactive({
      df <- cache
      
      # TA filter
      ta_sel <- if (is.null(input$filter_ta)) "all" else input$filter_ta
      if (ta_sel != "all") {
        df <- df[df$ta == ta_sel, ]
      }
      
      # Phase filter
      phase_sel <- if (is.null(input$filter_phase)) "all" else input$filter_phase
      if (phase_sel != "all") {
        df <- df[df$phases == phase_sel, ]
      }
      
      # Sponsor class filter
      sc_sel <- if (is.null(input$filter_sponsor_class)) "all" else input$filter_sponsor_class
      if (sc_sel != "all") {
        df <- df[df$sponsor_class == sc_sel, ]
      }
      
      df
    })
    
    # ── Reactive: sponsor summary table ──────────────────────────────────────
    sponsor_summary <- reactive({
      df <- filtered_trials()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      
      # Aggregate by sponsor
      agg <- df |>
        dplyr::group_by(lead_sponsor, sponsor_class) |>
        dplyr::summarise(
          n_trials        = dplyr::n(),
          n_phase3        = sum(phases == "PHASE3", na.rm = TRUE),
          n_phase23       = sum(phases == "PHASE2|PHASE3", na.rm = TRUE),
          total_enrollment = sum(as.numeric(enrollment), na.rm = TRUE),
          median_enrollment = stats::median(as.numeric(enrollment), na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::arrange(dplyr::desc(n_trials)) |>
        dplyr::mutate(
          rank = dplyr::row_number(),
          trial_share = round(n_trials / sum(n_trials) * 100, 1)
        )
      
      agg
    })
    
    # ── Reactive: selected sponsor (from bar chart click) ─────────────────────
    selected_sponsor <- reactiveVal(NULL)
    
    # ── KPI outputs ───────────────────────────────────────────────────────────
    output$kpi_total_sponsors <- renderText({
      ss <- sponsor_summary()
      if (is.null(ss)) return("—")
      format(nrow(ss), big.mark = ",")
    })
    
    output$kpi_industry_pct <- renderText({
      df <- filtered_trials()
      if (is.null(df) || nrow(df) == 0) return("—")
      n_industry <- sum(df$sponsor_class == "INDUSTRY", na.rm = TRUE)
      paste0(round(n_industry / nrow(df) * 100, 0), "%")
    })
    
    output$kpi_median_enroll <- renderText({
      ss <- sponsor_summary()
      if (is.null(ss) || nrow(ss) == 0) return("—")
      top20 <- head(ss, 20)
      med <- stats::median(top20$median_enrollment, na.rm = TRUE)
      if (is.na(med)) return("—")
      format(round(med, 0), big.mark = ",")
    })
    
    output$kpi_top_sponsor <- renderText({
      ss <- sponsor_summary()
      if (is.null(ss) || nrow(ss) == 0) return("—")
      top <- ss$lead_sponsor[1]
      # Truncate long names
      if (nchar(top) > 28) paste0(substr(top, 1, 26), "…") else top
    })
    
    # ── Bar chart ─────────────────────────────────────────────────────────────
    output$sponsor_bar_chart <- renderGirafe({
      ss <- sponsor_summary()
      if (is.null(ss) || nrow(ss) == 0) {
        p <- ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
                            label = "No sponsors found for current filters.",
                            color = "#605E5C", size = 5) +
          ggplot2::theme_void()
        return(girafe(ggobj = p))
      }
      
      top_n_val <- if (is.null(input$top_n)) 20 else input$top_n
      df_plot <- head(ss, top_n_val)
      
      # Sponsor class color mapping
      class_colors <- c(
        "INDUSTRY" = octid_colors$primary,
        "OTHER"    = octid_colors$accent,
        "NIH"      = octid_colors$success,
        "FED"      = octid_colors$warning
      )
      
      # Pre-compute tooltips (hard rule: no function calls inside aes())
      df_plot <- df_plot |>
        dplyr::mutate(
          label_short = dplyr::if_else(
            nchar(lead_sponsor) > 30,
            paste0(substr(lead_sponsor, 1, 28), "…"),
            lead_sponsor
          ),
          bar_color = dplyr::coalesce(class_colors[sponsor_class], octid_colors$primary),
          tooltip_text = paste0(
            "<b>", lead_sponsor, "</b><br>",
            "Trials: ", n_trials, " (", trial_share, "% of filtered total)<br>",
            "Phase 3: ", n_phase3, " | Phase 2/3: ", n_phase23, "<br>",
            "Sponsor type: ", sponsor_class, "<br>",
            "Median enrollment: ", round(median_enrollment, 0)
          ),
          data_id = lead_sponsor
        )
      
      p <- ggplot2::ggplot(
        df_plot,
        ggplot2::aes(
          x    = n_trials,
          y    = stats::reorder(label_short, n_trials),
          fill = sponsor_class
        )
      ) +
        ggiraph::geom_col_interactive(
          ggplot2::aes(
            tooltip  = tooltip_text,
            data_id  = data_id,
            onclick  = paste0(
              "Shiny.setInputValue('", ns("selected_sponsor_click"),
              "', '", gsub("'", "\\\\'", df_plot$lead_sponsor), "', {priority: 'event'});"
            )
          ),
          alpha = 0.9,
          width = 0.75
        ) +
        ggplot2::scale_fill_manual(
          values = class_colors,
          name   = "Sponsor type",
          na.value = octid_colors$primary
        ) +
        ggplot2::scale_x_continuous(
          expand = ggplot2::expansion(mult = c(0, 0.05)),
          labels = scales::comma
        ) +
        ggplot2::labs(
          x = "Number of active trials",
          y = NULL
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor   = ggplot2::element_blank(),
          axis.text.y        = ggplot2::element_text(size = 9, color = "#323130"),
          axis.text.x        = ggplot2::element_text(size = 9, color = "#605E5C"),
          axis.title.x       = ggplot2::element_text(size = 9, color = "#605E5C"),
          legend.position    = "bottom",
          legend.title       = ggplot2::element_text(size = 9),
          legend.text        = ggplot2::element_text(size = 9),
          plot.margin        = ggplot2::margin(t = 10, r = 15, b = 10, l = 10)
        )
      
      girafe(
        ggobj = p,
        options = list(
          opts_hover(css = "fill-opacity: 1; stroke: white; stroke-width: 1px;"),
          opts_selection(
            type = "single",
            css  = paste0("fill: ", octid_colors$primary_dark, "; fill-opacity: 1;")
          ),
          opts_tooltip(
            css      = "background: #323130; color: white; padding: 8px 12px; border-radius: 4px; font-size: 13px;",
            use_fill = FALSE
          ),
          opts_toolbar(saveaspng = FALSE)
        ),
        width_svg  = 7,
        height_svg = 6
      )
    })
    
    # ── Bar chart click → update selected sponsor ─────────────────────────────
    # ggiraph selection via selected_
    observeEvent(input[[paste0("sponsor_bar_chart_selected")]], {
      sel <- input[[paste0("sponsor_bar_chart_selected")]]
      if (!is.null(sel) && length(sel) > 0 && sel != "") {
        selected_sponsor(sel)
      }
    })
    
    # ── Detail panel ──────────────────────────────────────────────────────────
    output$detail_panel <- renderUI({
      sel <- selected_sponsor()
      
      if (is.null(sel) || sel == "") {
        return(
          div(
            style = "display: flex; align-items: center; justify-content: center; height: 200px;",
            div(
              style = "text-align: center; color: #605E5C;",
              icon("mouse pointer", style = "font-size: 2em; color: #EDEBE9; display: block; margin: 0 auto 0.5em;"),
              div("Click a bar to see", style = "font-size: 1em;"),
              div("sponsor trial details", style = "font-size: 0.9em;")
            )
          )
        )
      }
      
      # Get sponsor trials
      df <- filtered_trials()
      sp_trials <- df[df$lead_sponsor == sel, ]
      
      if (nrow(sp_trials) == 0) {
        return(div(style = "color: #605E5C;", "No trials found for this sponsor."))
      }
      
      # Sponsor class label
      sc_raw   <- sp_trials$sponsor_class[1]
      sc_label <- dplyr::case_when(
        sc_raw == "INDUSTRY" ~ "Industry",
        sc_raw == "OTHER"    ~ "Academic / Other",
        sc_raw == "NIH"      ~ "NIH / Government",
        TRUE                 ~ sc_raw
      )
      
      # Trial list
      trial_rows <- lapply(seq_len(min(nrow(sp_trials), 8)), function(i) {
        trial <- sp_trials[i, ]
        phase_badge_color <- if (!is.null(trial$phases) && !is.na(trial$phases) &&
                                 grepl("3", trial$phases)) {
          octid_colors$primary
        } else {
          octid_colors$accent
        }
        status_color <- if (!is.null(trial$overall_status) && !is.na(trial$overall_status) &&
                            trial$overall_status == "RECRUITING") {
          octid_colors$success
        } else {
          octid_colors$warning
        }
        
        div(
          style = paste0(
            "padding: 0.6em 0; border-bottom: 1px solid ", octid_colors$border, ";"
          ),
          div(
            style = "display: flex; justify-content: space-between; align-items: flex-start; gap: 0.5em;",
            div(
              style = "flex: 1; font-size: 0.85em; color: #323130; line-height: 1.4;",
              if (!is.null(trial$brief_title) && !is.na(trial$brief_title)) {
                substr(trial$brief_title, 1, 80)
              } else if (!is.null(trial$nct_id)) {
                trial$nct_id
              } else {
                "—"
              }
            ),
            div(
              style = "flex-shrink: 0;",
              div(
                style = paste0(
                  "display: inline-block; padding: 0.15em 0.5em; border-radius: 3px; ",
                  "background: ", phase_badge_color, "; color: white; font-size: 0.75em;"
                ),
                if (!is.null(trial$phases) && !is.na(trial$phases)) trial$phases else "—"
              )
            )
          ),
          div(
            style = "display: flex; gap: 0.75em; margin-top: 0.25em; font-size: 0.78em; color: #605E5C;",
            if (!is.null(trial$overall_status) && !is.na(trial$overall_status)) {
              div(
                style = paste0("color: ", status_color, ";"),
                trial$overall_status
              )
            },
            if (!is.null(trial$enrollment) && !is.na(trial$enrollment)) {
              div(paste0("n=", format(as.integer(trial$enrollment), big.mark = ",")))
            }
          )
        )
      })
      
      # "…and N more" note
      remaining_note <- if (nrow(sp_trials) > 8) {
        div(
          style = "padding-top: 0.5em; font-size: 0.82em; color: #605E5C; font-style: italic;",
          paste0("… and ", nrow(sp_trials) - 8, " more trial",
                 if (nrow(sp_trials) - 8 > 1) "s" else "")
        )
      } else {
        NULL
      }
      
      tagList(
        # Sponsor header
        div(
          style = paste0(
            "margin-bottom: 1em; padding-bottom: 0.75em; ",
            "border-bottom: 2px solid ", octid_colors$primary, ";"
          ),
          div(
            style = paste0("font-size: 1.1em; font-weight: 700; color: ", octid_colors$primary_dark, ";"),
            sel
          ),
          div(
            style = "display: flex; gap: 1em; margin-top: 0.4em; font-size: 0.85em; color: #605E5C;",
            div(
              style = paste0(
                "padding: 0.2em 0.7em; border-radius: 3px; ",
                "background: ", octid_colors$bg_light_blue, "; ",
                "color: ", octid_colors$primary, "; font-weight: 600;"
              ),
              sc_label
            ),
            div(paste0(nrow(sp_trials), " active trial", if (nrow(sp_trials) > 1) "s" else "")),
            div(paste0(
              "Phase 3: ",
              sum(sp_trials$phases == "PHASE3", na.rm = TRUE)
            ))
          )
        ),
        
        # Trial list
        div(
          style = "font-size: 0.88em; color: #605E5C; margin-bottom: 0.5em; font-weight: 600;",
          "ACTIVE TRIALS"
        ),
        do.call(tagList, trial_rows),
        remaining_note
      )
    })
    
    # ── Sponsor ranking table ─────────────────────────────────────────────────
    output$sponsor_table <- renderReactable({
      ss <- sponsor_summary()
      
      if (is.null(ss) || nrow(ss) == 0) {
        return(reactable(
          data.frame(Message = "No sponsors found for current filters."),
          columns = list(Message = colDef(name = ""))
        ))
      }
      
      # Format for display
      ss_display <- ss |>
        dplyr::select(rank, lead_sponsor, sponsor_class, n_trials,
                      trial_share, n_phase3, n_phase23, total_enrollment) |>
        dplyr::mutate(
          total_enrollment = round(total_enrollment, 0)
        )
      
      reactable(
        ss_display,
        theme = reactableTheme(
          stripedColor   = "#F5F5F5",
          highlightColor = octid_colors$bg_light_blue,
          cellPadding    = "8px 12px"
        ),
        striped    = TRUE,
        highlight  = TRUE,
        searchable = TRUE,
        pagination = TRUE,
        defaultPageSize = 15,
        defaultSorted   = "n_trials",
        defaultSortOrder = "desc",
        columns = list(
          rank = colDef(
            name  = "#",
            width = 48,
            style = list(color = "#605E5C", fontSize = "0.85em")
          ),
          lead_sponsor = colDef(
            name  = "Sponsor",
            minWidth = 200,
            cell = function(value) {
              div(style = paste0("font-weight: 600; color: ", octid_colors$primary_dark, ";"), value)
            }
          ),
          sponsor_class = colDef(
            name  = "Type",
            width = 130,
            cell = function(value) {
              label <- dplyr::case_when(
                value == "INDUSTRY" ~ "Industry",
                value == "OTHER"    ~ "Academic",
                value == "NIH"      ~ "NIH / Gov",
                TRUE                ~ value
              )
              bg_col <- dplyr::case_when(
                value == "INDUSTRY" ~ octid_colors$bg_light_blue,
                value == "OTHER"    ~ "#F3E8F9",
                value == "NIH"      ~ "#E8F5E9",
                TRUE                ~ "#F5F5F5"
              )
              txt_col <- dplyr::case_when(
                value == "INDUSTRY" ~ octid_colors$primary,
                value == "OTHER"    ~ octid_colors$rare_disease,
                value == "NIH"      ~ octid_colors$success,
                TRUE                ~ "#605E5C"
              )
              div(
                style = paste0(
                  "display: inline-block; padding: 0.2em 0.6em; border-radius: 3px; ",
                  "background: ", bg_col, "; color: ", txt_col, "; font-size: 0.85em;"
                ),
                label
              )
            }
          ),
          n_trials = colDef(
            name   = "Active Trials",
            width  = 110,
            format = colFormat(separators = TRUE),
            style  = function(value) {
              list(fontWeight = "700", color = octid_colors$primary)
            }
          ),
          trial_share = colDef(
            name  = "Share %",
            width = 80,
            cell  = function(value) {
              paste0(value, "%")
            },
            style = list(color = "#605E5C", fontSize = "0.9em")
          ),
          n_phase3 = colDef(
            name  = "Phase 3",
            width = 90,
            style = list(color = "#323130")
          ),
          n_phase23 = colDef(
            name  = "Phase 2/3",
            width = 90,
            style = list(color = "#323130")
          ),
          total_enrollment = colDef(
            name   = "Total Enrollment",
            width  = 140,
            format = colFormat(separators = TRUE),
            style  = list(color = "#605E5C")
          )
        )
      )
    })
    
    # ── Row count label ───────────────────────────────────────────────────────
    output$table_row_count <- renderText({
      ss <- sponsor_summary()
      if (is.null(ss)) return("")
      paste0(format(nrow(ss), big.mark = ","), " sponsors")
    })
    
    # ── CSV download ──────────────────────────────────────────────────────────
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("octid_sponsor_activity_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        ss <- sponsor_summary()
        if (is.null(ss)) {
          write.csv(data.frame(), file, row.names = FALSE)
        } else {
          write.csv(ss, file, row.names = FALSE)
        }
      }
    )
    
    # ── outputOptions — CSS tab fix (apply to ALL render* outputs) ────────────
    outputOptions(output, "kpi_total_sponsors",  suspendWhenHidden = FALSE)
    outputOptions(output, "kpi_industry_pct",    suspendWhenHidden = FALSE)
    outputOptions(output, "kpi_median_enroll",   suspendWhenHidden = FALSE)
    outputOptions(output, "kpi_top_sponsor",     suspendWhenHidden = FALSE)
    outputOptions(output, "sponsor_bar_chart",   suspendWhenHidden = FALSE)
    outputOptions(output, "detail_panel",        suspendWhenHidden = FALSE)
    outputOptions(output, "sponsor_table",       suspendWhenHidden = FALSE)
    outputOptions(output, "table_row_count",     suspendWhenHidden = FALSE)
    # NOTE: download_csv NOT included — downloadHandler doesn't use outputOptions
    
  })
}