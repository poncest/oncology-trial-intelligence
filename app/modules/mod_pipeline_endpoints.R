# =============================================================================
# Pipeline & Endpoints Module
# =============================================================================

pipeline_endpoints_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # TODO: build Pipeline & Endpoints UI
    h2("Pipeline & Endpoints — placeholder")
  )
}

pipeline_endpoints_server <- function(id, cache = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # TODO: implement Pipeline & Endpoints logic

    # ⚠️  Apply outputOptions to ALL outputs (CSS tab fix — see lessons learned)
    # outputOptions(output, "my_output", suspendWhenHidden = FALSE)

  })
}
