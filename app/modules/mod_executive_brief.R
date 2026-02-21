# =============================================================================
# Executive Brief Module
# =============================================================================

executive_brief_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # TODO: build Executive Brief UI
    h2("Executive Brief — placeholder")
  )
}

executive_brief_server <- function(id, cache = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # TODO: implement Executive Brief logic

    # ⚠️  Apply outputOptions to ALL outputs (CSS tab fix — see lessons learned)
    # outputOptions(output, "my_output", suspendWhenHidden = FALSE)

  })
}
