# =============================================================================
# Site & Investigator Networks Module
# =============================================================================

site_networks_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # TODO: build Site & Investigator Networks UI
    h2("Site & Investigator Networks — placeholder")
  )
}

site_networks_server <- function(id, cache = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # TODO: implement Site & Investigator Networks logic

    # ⚠️  Apply outputOptions to ALL outputs (CSS tab fix — see lessons learned)
    # outputOptions(output, "my_output", suspendWhenHidden = FALSE)

  })
}
