# =============================================================================
# app.R — OCTID Entry Point
# Oncology Competitive Trial Intelligence Dashboard
# Author: Steven Ponce | February 2026
# =============================================================================
# ⚠️  Source order matters. Modules and ui.R sourced AFTER global.R.
#     R/ folder is intentionally EMPTY — avoids auto-sourcing before global.R.
# =============================================================================

source("global.R")

# Modules (explicit source — preserves load order)
source("modules/mod_executive_brief.R")
source("modules/mod_pipeline_endpoints.R")
source("modules/mod_sponsor_activity.R")
source("modules/mod_site_networks.R")
source("modules/mod_methods.R")

# UI & Server (sourced after global.R + modules)
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)

