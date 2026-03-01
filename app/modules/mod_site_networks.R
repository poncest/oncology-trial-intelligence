# =============================================================================
# OCTID — mod_site_networks.R
# Site & Investigator Networks Tab
# Questions answered:
#   - Where is trial activity geographically concentrated?
#   - Site concentration risk — especially for Rare Disease
#   - Which countries have the most active trial footprint?
# =============================================================================

site_networks_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    # ── Filter bar ────────────────────────────────────────────────────────────
    div(
      class = "ui segment",
      style = "background: #F5F5F5; border: none; margin-bottom: 1.5em; padding: 1em 1.5em;",
      
      div(
        class = "ui three column stackable grid",
        
        # TA filter
        div(
          class = "column",
          div(style = "font-size: 0.85em; color: #605E5C; margin-bottom: 0.4em; font-weight: 600;",
              "THERAPEUTIC AREA"),
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
          div(style = "font-size: 0.85em; color: #605E5C; margin-bottom: 0.4em; font-weight: 600;",
              "PHASE"),
          selectInput(
            ns("filter_phase"),
            label    = NULL,
            choices  = c("All Phases" = "all", "Phase 3" = "PHASE3", "Phase 2/3" = "PHASE2|PHASE3"),
            selected = "all",
            width    = "100%"
          )
        ),
        
        # Status filter
        div(
          class = "column",
          div(style = "font-size: 0.85em; color: #605E5C; margin-bottom: 0.4em; font-weight: 600;",
              "STATUS"),
          selectInput(
            ns("filter_status"),
            label    = NULL,
            choices  = c(
              "All Active"             = "all",
              "Recruiting"             = "RECRUITING",
              "Active, Not Recruiting" = "ACTIVE_NOT_RECRUITING"
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
      
      div(
        class = "column",
        div(
          class = "ui segment",
          style = paste0("text-align: center; padding: 1.5em 1em; border-top: 3px solid ", octid_colors$primary, ";"),
          div(style = paste0("font-size: 2.2em; font-weight: 700; color: ", octid_colors$primary, ";"),
              uiOutput(ns("kpi_countries"), inline = TRUE)),
          div(style = "font-size: 0.85em; color: #605E5C; margin-top: 0.3em;", "Countries with Active Trials")
        )
      ),
      
      div(
        class = "column",
        div(
          class = "ui segment",
          style = paste0("text-align: center; padding: 1.5em 1em; border-top: 3px solid ", octid_colors$accent, ";"),
          div(style = paste0("font-size: 2.2em; font-weight: 700; color: ", octid_colors$accent, ";"),
              uiOutput(ns("kpi_total_sites"), inline = TRUE)),
          div(style = "font-size: 0.85em; color: #605E5C; margin-top: 0.3em;", "Total Site-Mentions")
        )
      ),
      
      div(
        class = "column",
        div(
          class = "ui segment",
          style = paste0("text-align: center; padding: 1.5em 1em; border-top: 3px solid ", octid_colors$warning, ";"),
          div(style = paste0("font-size: 2.2em; font-weight: 700; color: ", octid_colors$warning, ";"),
              uiOutput(ns("kpi_single_site"), inline = TRUE)),
          div(style = "font-size: 0.85em; color: #605E5C; margin-top: 0.3em;", "Single-Site Trials")
        )
      ),
      
      div(
        class = "column",
        div(
          class = "ui segment",
          style = paste0("text-align: center; padding: 1.5em 1em; border-top: 3px solid ", octid_colors$rare_disease, ";"),
          div(style = paste0("font-size: 2.2em; font-weight: 700; color: ", octid_colors$rare_disease, ";"),
              uiOutput(ns("kpi_rd_countries"), inline = TRUE)),
          div(style = "font-size: 0.85em; color: #605E5C; margin-top: 0.3em;", "Rare Disease Countries")
        )
      )
    ),
    
    # ── Rare Disease concentration callout ────────────────────────────────────
    uiOutput(ns("rd_callout")),
    
    # ── Main layout: map + country table ──────────────────────────────────────
    div(
      class = "ui two column stackable grid",
      style = "margin-bottom: 1.5em;",
      
      # Left: map
      div(
        class = "ten wide column",
        div(
          class = "ui segment",
          style = "padding: 1.5em;",
          div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.6em;",
            div(
              h3(style = paste0("color: ", octid_colors$primary_dark, "; margin: 0;"),
                 "Trial Activity by Country"),
              div(style = "color: #605E5C; font-size: 0.9em;",
                  "Bubble size = trial count · click a bubble for details")
            )
          ),
          div(
            style = paste0(
              "margin-bottom: 0.8em; padding: 0.5em 0.8em; border-radius: 4px; ",
              "background: #FFF4CE; border-left: 3px solid #CA5010; ",
              "font-size: 0.82em; color: #605E5C;"
            ),
            icon("exclamation triangle", style = "color: #CA5010;"),
            " Registry site counts reflect trial registration data, not enrollment intensity. ",
            "Number of sites does not indicate enrollment success or site activation rates."
          ),
          leafletOutput(ns("country_map"), height = "420px")
        )
      ),
      
      # Right: top country table
      div(
        class = "six wide column",
        div(
          class = "ui segment",
          style = "padding: 1.5em;",
          h3(style = paste0("color: ", octid_colors$primary_dark, "; margin: 0 0 1em 0;"),
             "Top Countries"),
          reactableOutput(ns("country_table_top"))
        )
      )
    ),
    
    # ── Site concentration chart ───────────────────────────────────────────────
    div(
      class = "ui segment",
      style = "margin-bottom: 1.5em; padding: 1.5em;",
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 1em;",
        div(
          h3(style = paste0("color: ", octid_colors$primary_dark, "; margin: 0;"),
             "Site Concentration Distribution"),
          div(style = "color: #605E5C; font-size: 0.9em;",
              "How many trials run at each site scale — single site vs. global networks")
        )
      ),
      girafeOutput(ns("site_concentration_chart"), height = "260px")
    ),
    
    # ── Full country ranking table ────────────────────────────────────────────
    div(
      class = "ui segment",
      style = "padding: 1.5em;",
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 1em;",
        h3(style = paste0("color: ", octid_colors$primary_dark, "; margin: 0;"),
           "Full Country Ranking"),
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
      reactableOutput(ns("country_table_full"))
    )
  )
}


# =============================================================================
# SERVER
# =============================================================================

site_networks_server <- function(id, cache) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ── Country lookup table (ISO codes for Leaflet) ──────────────────────────
    # Maps display names → ISO-3 codes for choropleth/bubble positioning
    country_coords <- data.frame(
      country = c(
        "United States", "China", "France", "Spain", "Italy", "Canada",
        "Germany", "United Kingdom", "Australia", "Poland", "South Korea",
        "Japan", "Belgium", "Brazil", "Taiwan", "Netherlands",
        "Turkey (T\u00fcrkiye)", "Israel", "Czechia", "Hungary", "Argentina",
        "Austria", "Greece", "Mexico", "Sweden", "Denmark", "Portugal",
        "Romania", "Switzerland", "Russia", "India", "Ukraine", "Chile",
        "Norway", "Finland", "Slovakia", "Colombia", "Peru", "Ireland",
        "Singapore", "Thailand", "Malaysia", "Egypt", "Saudi Arabia",
        "South Africa", "New Zealand", "Bulgaria", "Croatia", "Serbia",
        "Lithuania", "Latvia", "Estonia", "Slovenia", "Belarus", "Kazakhstan",
        "Georgia", "Armenia", "Philippines", "Indonesia", "Vietnam",
        "Hong Kong", "Pakistan", "Jordan", "Lebanon", "Tunisia", "Morocco",
        "Iran", "Iraq", "Kuwait", "Qatar", "United Arab Emirates", "Oman",
        "Kenya", "Nigeria", "Ghana", "Ethiopia", "Tanzania", "Uganda",
        "Cameroon", "Ivory Coast", "Senegal", "Rwanda", "Zambia", "Zimbabwe",
        "Ecuador", "Bolivia", "Paraguay", "Uruguay", "Venezuela", "Panama",
        "Costa Rica", "Guatemala", "Honduras", "Cuba", "Puerto Rico",
        "Dominican Republic", "Trinidad and Tobago"
      ),
      lat = c(
        37.1, 35.9, 46.2, 40.5, 41.9, 56.1, 51.2, 52.4, -25.3, 51.9, 35.9,
        36.2, 50.5, -14.2, 23.7, 52.1, 38.9, 31.1, 49.8, 47.2, -38.4, 47.5,
        39.1, 23.6, 60.1, 56.3, 39.4, 45.9, 46.8, 61.5, 20.6, 49.0, -35.7,
        60.5, 61.9, 48.7, 4.1, -9.2, 53.4, 1.4, 15.9, 4.2, 26.8, 24.7,
        -30.6, -40.9, 42.7, 45.2, 44.0, 55.2, 57.0, 59.4, 46.2, 53.7, 48.0,
        42.3, 40.1, 12.9, -0.8, 14.1, 22.4, 30.4, 31.2, 33.9, 33.9, 28.6,
        33.2, 32.4, 29.3, 25.4, 23.4, 24.5, -0.0, 9.1, 7.9, 9.1, -6.4, 1.4,
        4.0, 7.5, 14.5, -1.9, -15.4, -19.0, -1.8, -16.3, -23.4, -32.5,
        6.4, 8.5, 9.7, 15.2, 14.6, 23.1, 18.2, 18.7, 10.7
      ),
      lng = c(
        -95.7, 104.2, 2.2, -3.7, 12.6, -96.8, 10.5, -1.6, 133.8, 19.1, 127.8,
        138.3, 4.5, -51.9, 120.9, 5.3, 35.2, 34.9, 15.5, 19.5, -63.6, 14.6,
        21.8, -102.5, 18.6, 9.5, -8.2, 24.9, 8.2, 105.3, 78.7, 31.2, -71.5,
        8.5, 26.3, 19.7, -74.3, -75.0, -8.2, 103.8, 101.0, 109.7, 30.8, 45.1,
        25.1, 172.5, 25.5, 16.4, 21.0, 23.9, 24.8, 25.0, 14.8, 28.0, 66.9,
        43.4, 44.9, 122.9, 113.9, 108.3, 114.2, 69.3, 36.2, 35.9, 9.5, -7.1,
        53.7, 43.7, 47.5, 51.2, 53.9, 54.4, 36.8, 8.7, -1.0, 38.5, 34.9,
        32.3, 12.4, -5.6, -14.5, 30.0, 27.8, 29.9, -78.2, -64.2, -57.6,
        -56.0, -66.6, -80.8, -84.1, -90.2, -86.2, -79.5, -66.1, -70.2, -61.2
      ),
      stringsAsFactors = FALSE
    )
    
    # ── Reactive: filtered trials ─────────────────────────────────────────────
    filtered_trials <- reactive({
      df <- cache
      
      ta_sel <- if (is.null(input$filter_ta)) "all" else input$filter_ta
      if (ta_sel != "all") df <- df[df$ta == ta_sel, ]
      
      phase_sel <- if (is.null(input$filter_phase)) "all" else input$filter_phase
      if (phase_sel != "all") df <- df[df$phases == phase_sel, ]
      
      status_sel <- if (is.null(input$filter_status)) "all" else input$filter_status
      if (status_sel != "all") df <- df[df$overall_status == status_sel, ]
      
      df
    })
    
    # ── Reactive: country frequency table ─────────────────────────────────────
    country_freq <- reactive({
      df <- filtered_trials()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      
      # Explode pipe-separated countries
      exploded <- df |>
        dplyr::select(nct_id, countries, n_sites, ta) |>
        tidyr::separate_rows(countries, sep = " \\| ") |>
        dplyr::mutate(countries = trimws(countries)) |>
        dplyr::filter(!is.na(countries), countries != "")
      
      # Aggregate
      agg <- exploded |>
        dplyr::group_by(country = countries) |>
        dplyr::summarise(
          n_trials      = dplyr::n(),
          n_oncology    = sum(ta == "Oncology", na.rm = TRUE),
          n_rare        = sum(ta == "Rare Disease", na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::arrange(dplyr::desc(n_trials)) |>
        dplyr::mutate(rank = dplyr::row_number())
      
      # Join coords
      agg <- dplyr::left_join(agg, country_coords, by = "country")
      
      agg
    })
    
    # ── Reactive: Rare Disease country freq (for callout) ─────────────────────
    rd_country_freq <- reactive({
      df <- cache[cache$ta == "Rare Disease", ]
      if (nrow(df) == 0) return(NULL)
      
      exploded <- df |>
        dplyr::select(nct_id, countries) |>
        tidyr::separate_rows(countries, sep = " \\| ") |>
        dplyr::mutate(countries = trimws(countries)) |>
        dplyr::filter(!is.na(countries), countries != "")
      
      agg <- exploded |>
        dplyr::group_by(country = countries) |>
        dplyr::summarise(n_trials = dplyr::n(), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(n_trials))
      
      agg
    })
    
    # ── KPIs ──────────────────────────────────────────────────────────────────
    output$kpi_countries <- renderText({
      cf <- country_freq()
      if (is.null(cf)) return("—")
      format(nrow(cf), big.mark = ",")
    })
    
    output$kpi_total_sites <- renderText({
      cf <- country_freq()
      if (is.null(cf)) return("—")
      format(sum(cf$n_trials), big.mark = ",")
    })
    
    output$kpi_single_site <- renderText({
      df <- filtered_trials()
      if (is.null(df) || nrow(df) == 0) return("—")
      n <- sum(df$n_sites == 1, na.rm = TRUE)
      paste0(format(n, big.mark = ","),
             " (", round(n / nrow(df) * 100, 0), "%)")
    })
    
    output$kpi_rd_countries <- renderText({
      cf_rd <- rd_country_freq()
      if (is.null(cf_rd)) return("—")
      format(nrow(cf_rd), big.mark = ",")
    })
    
    # ── Rare Disease concentration callout ────────────────────────────────────
    output$rd_callout <- renderUI({
      ta_sel <- if (is.null(input$filter_ta)) "all" else input$filter_ta
      
      # Only show when viewing "All" or "Rare Disease"
      if (ta_sel == "Oncology") return(NULL)
      
      cf_rd <- rd_country_freq()
      if (is.null(cf_rd) || nrow(cf_rd) == 0) return(NULL)
      
      # Top 5 RD countries
      top5 <- head(cf_rd, 5)
      top5_pct <- round(sum(top5$n_trials) / sum(cf_rd$n_trials) * 100, 0)
      top5_names <- paste(top5$country[1:min(3, nrow(top5))], collapse = ", ")
      
      div(
        class = "ui segment",
        style = paste0(
          "margin-bottom: 1.5em; padding: 1.2em 1.5em; ",
          "border-left: 4px solid ", octid_colors$rare_disease, "; ",
          "background: ", octid_colors$bg_light_purple, ";"
        ),
        div(
          style = "display: flex; align-items: flex-start; gap: 1em;",
          icon("map marker alternate",
               style = paste0("color: ", octid_colors$rare_disease, "; font-size: 1.3em; margin-top: 0.1em;")),
          div(
            div(
              style = paste0("font-weight: 700; color: ", octid_colors$rare_disease, "; margin-bottom: 0.3em;"),
              "Rare Disease Site Concentration"
            ),
            div(
              style = "color: #323130; font-size: 0.95em; line-height: 1.5;",
              paste0(
                "Top 5 countries account for ", top5_pct, "% of Rare Disease trial site-mentions. ",
                "Activity is concentrated in ", top5_names, ", and ", nrow(cf_rd) - 3,
                " additional countries — a narrower footprint than Oncology, ",
                "consistent with smaller patient populations and specialized center requirements."
              )
            ),
            div(
              style = "margin-top: 0.6em; font-size: 0.85em; color: #605E5C;",
              paste0(
                "Rare Disease: ", nrow(cf_rd), " countries · ",
                sum(cf_rd$n_trials), " site-mentions · 176 trials"
              )
            )
          )
        )
      )
    })
    
    # ── Leaflet map ───────────────────────────────────────────────────────────
    output$country_map <- renderLeaflet({
      cf <- country_freq()
      
      if (is.null(cf) || nrow(cf) == 0) {
        return(
          leaflet() |>
            addTiles() |>
            setView(lng = 10, lat = 30, zoom = 2)
        )
      }
      
      # Only countries with coordinates
      cf_geo <- cf[!is.na(cf$lat) & !is.na(cf$lng), ]
      
      # Scale bubble radius
      max_trials <- max(cf_geo$n_trials, na.rm = TRUE)
      cf_geo$radius <- 6 + (cf_geo$n_trials / max_trials) * 22
      
      # Color by TA mix
      cf_geo$color <- dplyr::case_when(
        cf_geo$n_rare == 0  ~ octid_colors$primary,
        cf_geo$n_oncology == 0 ~ octid_colors$rare_disease,
        TRUE ~ octid_colors$accent
      )
      
      # Popup content
      cf_geo$popup_html <- paste0(
        "<b>", cf_geo$country, "</b><br>",
        "Active trials: <b>", cf_geo$n_trials, "</b><br>",
        "<span style='color: ", octid_colors$primary, ";'>Oncology: ", cf_geo$n_oncology, "</span><br>",
        "<span style='color: ", octid_colors$rare_disease, ";'>Rare Disease: ", cf_geo$n_rare, "</span>"
      )
      
      leaflet(cf_geo) |>
        addTiles() |>
        
        setView(lng = 10, lat = 30, zoom = 2) |>
        addCircleMarkers(
          lng         = ~lng,
          lat         = ~lat,
          radius      = ~radius,
          color       = "white",
          weight      = 1,
          fillColor   = ~color,
          fillOpacity = 0.75,
          popup       = ~popup_html,
          label       = ~paste0(country, ": ", n_trials, " trials"),
          labelOptions = labelOptions(
            style = list(
              "background"  = "#323130",
              "color"       = "white",
              "padding"     = "4px 8px",
              "border"      = "none",
              "font-size"   = "12px"
            ),
            direction = "auto"
          )
        ) |>
        addLegend(
          position = "bottomright",
          colors   = c(octid_colors$primary, octid_colors$rare_disease, octid_colors$accent),
          labels   = c("Oncology only", "Rare Disease only", "Both"),
          title    = "Trial type",
          opacity  = 0.8
        )
    })
    
    # ── Top countries table (sidebar) ─────────────────────────────────────────
    output$country_table_top <- renderReactable({
      cf <- country_freq()
      if (is.null(cf) || nrow(cf) == 0) {
        return(reactable(data.frame(Message = "No data."),
                         columns = list(Message = colDef(name = ""))))
      }
      
      top15 <- head(cf, 15) |>
        dplyr::select(rank, country, n_trials, n_oncology, n_rare)
      
      reactable(
        top15,
        theme = reactableTheme(
          stripedColor   = "#F5F5F5",
          highlightColor = octid_colors$bg_light_blue,
          cellPadding    = "6px 10px"
        ),
        striped         = TRUE,
        highlight       = TRUE,
        defaultPageSize = 15,
        pagination      = FALSE,
        columns = list(
          rank = colDef(
            name  = "#",
            width = 36,
            style = list(color = "#605E5C", fontSize = "0.82em")
          ),
          country = colDef(
            name     = "Country",
            minWidth = 130,
            cell = function(value) {
              div(style = paste0("font-weight: 600; color: ", octid_colors$primary_dark, ";"), value)
            }
          ),
          n_trials = colDef(
            name  = "Trials",
            width = 65,
            style = function(value) {
              list(fontWeight = "700", color = octid_colors$primary)
            }
          ),
          n_oncology = colDef(
            name  = "Onc",
            width = 52,
            style = list(color = "#323130", fontSize = "0.88em")
          ),
          n_rare = colDef(
            name  = "RD",
            width = 48,
            style = function(value) {
              col <- if (!is.na(value) && value > 0) octid_colors$rare_disease else "#BDBDBD"
              list(color = col, fontSize = "0.88em",
                   fontWeight = if (!is.na(value) && value > 0) "700" else "400")
            }
          )
        )
      )
    })
    
    # ── Site concentration chart ───────────────────────────────────────────────
    output$site_concentration_chart <- renderGirafe({
      df <- filtered_trials()
      
      if (is.null(df) || nrow(df) == 0) {
        p <- ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
                            label = "No data for current filters.",
                            color = "#605E5C", size = 5) +
          ggplot2::theme_void()
        return(girafe(ggobj = p))
      }
      
      # Bin n_sites into categories
      df$site_group <- dplyr::case_when(
        df$n_sites == 0  ~ "0 sites (unreported)",
        df$n_sites == 1  ~ "1 site (single-site)",
        df$n_sites <= 5  ~ "2–5 sites",
        df$n_sites <= 20 ~ "6–20 sites",
        df$n_sites <= 50 ~ "21–50 sites",
        df$n_sites <= 100 ~ "51–100 sites",
        TRUE             ~ "100+ sites (global)"
      )
      
      # Ordered factor
      site_levels <- c(
        "0 sites (unreported)", "1 site (single-site)", "2–5 sites",
        "6–20 sites", "21–50 sites", "51–100 sites", "100+ sites (global)"
      )
      df$site_group <- factor(df$site_group, levels = site_levels)
      
      # Aggregate
      agg <- df |>
        dplyr::group_by(site_group, ta) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop")
      
      # Pre-compute tooltip
      total_by_group <- agg |>
        dplyr::group_by(site_group) |>
        dplyr::summarise(total = sum(n), .groups = "drop")
      
      agg <- dplyr::left_join(agg, total_by_group, by = "site_group") |>
        dplyr::mutate(
          tooltip_text = paste0(
            site_group, "\n",
            ta, ": ", n, " trials\n",
            "Total: ", total
          )
        )
      
      ta_colors <- c("Oncology" = octid_colors$primary, "Rare Disease" = octid_colors$rare_disease)
      
      p <- ggplot2::ggplot(
        agg,
        ggplot2::aes(x = site_group, y = n, fill = ta)
      ) +
        ggiraph::geom_col_interactive(
          ggplot2::aes(tooltip = tooltip_text, data_id = paste0(site_group, "_", ta)),
          position = "stack",
          width    = 0.7,
          alpha    = 0.88
        ) +
        ggplot2::scale_fill_manual(values = ta_colors, name = NULL) +
        ggplot2::scale_y_continuous(
          expand = ggplot2::expansion(mult = c(0, 0.08)),
          labels = scales::comma
        ) +
        ggplot2::labs(x = NULL, y = "Number of trials") +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor   = ggplot2::element_blank(),
          axis.text.x        = ggplot2::element_text(size = 9, color = "#323130"),
          axis.text.y        = ggplot2::element_text(size = 9, color = "#605E5C"),
          axis.title.y       = ggplot2::element_text(size = 9, color = "#605E5C"),
          legend.position    = "right",
          legend.text        = ggplot2::element_text(size = 9),
          plot.margin        = ggplot2::margin(t = 10, r = 15, b = 10, l = 10)
        )
      
      girafe(
        ggobj = p,
        options = list(
          opts_hover(css = "fill-opacity: 1;"),
          opts_tooltip(
            css      = "background: #323130; color: white; padding: 8px 12px; border-radius: 4px; font-size: 13px;",
            use_fill = FALSE
          ),
          opts_toolbar(saveaspng = FALSE)
        ),
        width_svg  = 9,
        height_svg = 3.2
      )
    })
    
    # ── Full country ranking table ────────────────────────────────────────────
    output$country_table_full <- renderReactable({
      cf <- country_freq()
      if (is.null(cf) || nrow(cf) == 0) {
        return(reactable(data.frame(Message = "No countries found."),
                         columns = list(Message = colDef(name = ""))))
      }
      
      cf_display <- cf |>
        dplyr::select(rank, country, n_trials, n_oncology, n_rare) |>
        dplyr::mutate(
          rd_share = dplyr::if_else(
            n_rare > 0,
            paste0(round(n_rare / n_trials * 100, 0), "%"),
            "—"
          )
        )
      
      reactable(
        cf_display,
        theme = reactableTheme(
          stripedColor   = "#F5F5F5",
          highlightColor = octid_colors$bg_light_blue,
          cellPadding    = "8px 12px"
        ),
        striped         = TRUE,
        highlight       = TRUE,
        searchable      = TRUE,
        pagination      = TRUE,
        defaultPageSize = 20,
        defaultSorted   = "n_trials",
        defaultSortOrder = "desc",
        columns = list(
          rank = colDef(
            name  = "#",
            width = 48,
            style = list(color = "#605E5C", fontSize = "0.85em")
          ),
          country = colDef(
            name     = "Country",
            minWidth = 180,
            cell = function(value) {
              div(style = paste0("font-weight: 600; color: ", octid_colors$primary_dark, ";"), value)
            }
          ),
          n_trials = colDef(
            name  = "Active Trials",
            width = 110,
            format = colFormat(separators = TRUE),
            style = function(value) {
              list(fontWeight = "700", color = octid_colors$primary)
            }
          ),
          n_oncology = colDef(
            name  = "Oncology",
            width = 100,
            format = colFormat(separators = TRUE),
            style = list(color = "#323130")
          ),
          n_rare = colDef(
            name  = "Rare Disease",
            width = 120,
            cell = function(value) {
              if (is.na(value) || value == 0) {
                div(style = "color: #BDBDBD;", "—")
              } else {
                div(
                  style = paste0("color: ", octid_colors$rare_disease, "; font-weight: 700;"),
                  format(value, big.mark = ",")
                )
              }
            }
          ),
          rd_share = colDef(
            name  = "RD Share",
            width = 90,
            style = list(color = "#605E5C", fontSize = "0.9em")
          )
        )
      )
    })
    
    # ── Row count label ───────────────────────────────────────────────────────
    output$table_row_count <- renderText({
      cf <- country_freq()
      if (is.null(cf)) return("")
      paste0(format(nrow(cf), big.mark = ","), " countries")
    })
    
    # ── CSV download ──────────────────────────────────────────────────────────
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("octid_site_networks_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        cf <- country_freq()
        if (is.null(cf)) {
          write.csv(data.frame(), file, row.names = FALSE)
        } else {
          write.csv(cf, file, row.names = FALSE)
        }
      }
    )
    
    # ── outputOptions — CSS tab fix ───────────────────────────────────────────
    outputOptions(output, "kpi_countries",            suspendWhenHidden = FALSE)
    outputOptions(output, "kpi_total_sites",          suspendWhenHidden = FALSE)
    outputOptions(output, "kpi_single_site",          suspendWhenHidden = FALSE)
    outputOptions(output, "kpi_rd_countries",         suspendWhenHidden = FALSE)
    outputOptions(output, "rd_callout",               suspendWhenHidden = FALSE)
    outputOptions(output, "country_map",              suspendWhenHidden = FALSE)
    outputOptions(output, "country_table_top",        suspendWhenHidden = FALSE)
    outputOptions(output, "site_concentration_chart", suspendWhenHidden = FALSE)
    outputOptions(output, "country_table_full",       suspendWhenHidden = FALSE)
    outputOptions(output, "table_row_count",          suspendWhenHidden = FALSE)
    # NOTE: download_csv excluded — downloadHandler doesn't use outputOptions
    
  })
}