# =============================================================================
# ui.R — Top-level UI shell
# Sourced AFTER global.R — octid_colors and app_meta are available here
# =============================================================================

ui <- semanticPage(
  title = "OCTID | Oncology Competitive Trial Intelligence Dashboard",
  
  useWaiter(),
  useShinyjs(),
  
  # --- Custom CSS -------------------------------------------------------------
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$style(HTML(glue("
      body {{
        background-color: {octid_colors$bg_light_gray};
        color: {octid_colors$text_dark};
      }}
      .octid-header {{
        background: {octid_colors$primary_dark};
        color: white;
        padding: 1em 1.8em;
        margin-bottom: 0;
        display: flex;
        align-items: center;
        justify-content: space-between;
      }}
      .octid-title {{
        font-size: 1.7em;
        font-weight: 800;
        letter-spacing: 0.04em;
      }}
      .octid-subtitle {{
        font-size: 1em;
        opacity: 0.85;
        margin-left: 1em;
        font-weight: 400;
      }}
      .octid-version {{
        font-size: 0.78em;
        opacity: 0.65;
      }}
      .ui.tabular.menu .active.item {{
        border-top: 4px solid {octid_colors$primary} !important;
        color: {octid_colors$primary_dark} !important;
        font-weight: 700 !important;
        font-size: 1em !important;
        background: {octid_colors$bg_light_blue} !important;
      }}
      .ui.tabular.menu .item {{
        color: {octid_colors$text_dark};
        font-size: 0.97em;
        font-weight: 500;
        padding: 1em 1.3em !important;
      }}
      .ui.tabular.menu .item:hover {{
        color: {octid_colors$primary} !important;
        background: {octid_colors$bg_light_gray} !important;
      }}
      .ui.tabular.menu {{
        background: white;
        padding: 0 1.2em;
        border-bottom: 2px solid {octid_colors$border};
        margin-bottom: 0 !important;
        box-shadow: 0 2px 4px rgba(0,0,0,0.06);
      }}
      .tab-content-wrapper {{
        padding: 1.2em 1.5em;
        background: {octid_colors$bg_light_gray};
        min-height: calc(100vh - 130px);
      }}
      .ui.segment {{
        border-radius: 6px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.07);
      }}
      .ui.statistics .statistic > .value {{
        font-size: 2.2em !important;
      }}
      .ui.statistics .statistic > .label {{
        font-size: 0.82em !important;
        color: {octid_colors$text_gray} !important;
        text-transform: none !important;
        font-weight: 500 !important;
        letter-spacing: 0 !important;
      }}
    ")))
  ),
  
  # --- App header -------------------------------------------------------------
  div(class = "octid-header",
      div(
        tags$span(class = "octid-title", app_meta$title),
        tags$span(class = "octid-subtitle", app_meta$subtitle)
      ),
      div(class = "octid-version",
          glue("v{app_meta$version} · {app_meta$data_source}")
      )
  ),
  
  # --- Tab navigation ---------------------------------------------------------
  div(class = "ui top attached tabular menu",
      a(class = "item active", `data-tab` = "executive-brief",
        icon("chart bar"), "Executive Brief"),
      a(class = "item", `data-tab` = "pipeline-endpoints",
        icon("pills"), "Pipeline & Endpoints"),
      a(class = "item", `data-tab` = "sponsor-activity",
        icon("building"), "Sponsor Activity"),
      a(class = "item", `data-tab` = "site-networks",
        icon("map marker alternate"), "Site & Investigator Networks"),
      a(class = "item", `data-tab` = "methods",
        icon("info circle"), "Methods & Governance")
  ),
  
  # --- Tab content ------------------------------------------------------------
  
  # Executive Brief
  div(class = "ui bottom attached tab segment active", `data-tab` = "executive-brief",
      div(class = "tab-content-wrapper",
          executive_brief_ui("executive_brief")
      )
  ),
  
  # Pipeline & Endpoints
  div(class = "ui bottom attached tab segment", `data-tab` = "pipeline-endpoints",
      div(class = "tab-content-wrapper",
          pipeline_endpoints_ui("pipeline_endpoints")
      )
  ),
  
  # Sponsor Activity
  div(class = "ui bottom attached tab segment", `data-tab` = "sponsor-activity",
      div(class = "tab-content-wrapper",
          sponsor_activity_ui("sponsor_activity")
      )
  ),
  
  # Site & Investigator Networks 
  div(class = "ui bottom attached tab segment", `data-tab` = "site-networks",
      div(class = "tab-content-wrapper",
          site_networks_ui("site_networks")
      )
  ),
  
  # Methods & Governance (placeholder)
  div(class = "ui bottom attached tab segment", `data-tab` = "methods",
      div(class = "tab-content-wrapper",
          methods_ui("methods")
      )
  ),
  
  # --- Tab switching JS -------------------------------------------------------
  tags$script(HTML("
    $(document).ready(function() {
      $('.tabular.menu .item').tab();
    });
  "))
)