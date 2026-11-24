# Server of the app (#24)

.app_server <- function(input, output, session, default_lang = "en") {
  lang <- .app_lang(session$request, default_lang)

  # Changing the language reloads the app with ?lang= (the interface is built
  # once per language; loaded data are lost, which the selector says)
  shiny::observeEvent(input$lang, {
    if (!identical(input$lang, lang)) {
      shiny::updateQueryString(paste0("?lang=", input$lang), mode = "replace")
      session$reload()
    }
  }, ignoreInit = TRUE)

  data <- .mod_data_server("data", lang)
  analysis <- .mod_analysis_server("analysis", data, lang)
  group <- shiny::reactiveVal(NULL)
  results <- .mod_results_server("results", analysis, group, lang)
  zones <- .mod_zones_server("zones", results, lang)
  .mod_report_server("report", results, zones, group, lang)
  reliability <- .mod_reliability_server("reliability", group, lang)
  invisible(list(data = data, analysis = analysis, results = results, group = group,
                 zones = zones, reliability = reliability))
}
