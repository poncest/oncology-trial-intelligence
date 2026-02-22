# =============================================================================
# server.R — Top-level server
# =============================================================================

server <- function(input, output, session) {
  
  # Force Semantic UI tab initialization after Shiny flush
  # Required for outputOptions(suspendWhenHidden = FALSE) to take effect
  # on CSS-based tabs — see Launch Curve Forecaster lessons learned
  session$onFlushed(function() {
    shinyjs::runjs("$('.tabular.menu .item').tab();")
  }, once = TRUE)
  
  # Executive Brief — passes full cache; module filters internally by TA
  executive_brief_server("executive_brief", cache = app_cache)
  
  # Remaining modules wired in subsequent build sessions:
  pipeline_endpoints_server("pipeline_endpoints", cache = app_cache)
  sponsor_activity_server("sponsor_activity",     cache = app_cache)
  # site_networks_server("site_networks",           cache = app_cache)
  # methods_server("methods")
  
}