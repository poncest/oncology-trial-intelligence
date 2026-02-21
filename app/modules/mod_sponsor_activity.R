# =============================================================================
# Sponsor Activity Module
# =============================================================================

sponsor_activity_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # TODO: build Sponsor Activity UI
    h2("Sponsor Activity — placeholder")
  )
}

sponsor_activity_server <- function(id, cache = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # TODO: implement Sponsor Activity logic

    # ⚠️  Apply outputOptions to ALL outputs (CSS tab fix — see lessons learned)
    # outputOptions(output, "my_output", suspendWhenHidden = FALSE)

  })
}
