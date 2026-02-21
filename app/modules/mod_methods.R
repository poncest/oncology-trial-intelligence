# =============================================================================
# Methods & Governance Module
# =============================================================================

methods_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # TODO: build Methods & Governance UI
    h2("Methods & Governance — placeholder")
  )
}

methods_server <- function(id, cache = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # TODO: implement Methods & Governance logic

    # ⚠️  Apply outputOptions to ALL outputs (CSS tab fix — see lessons learned)
    # outputOptions(output, "my_output", suspendWhenHidden = FALSE)

  })
}
