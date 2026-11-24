# Server of the app (#24)

.app_server <- function(input, output, session, default_lang = "en") {
  # The language is a reactive: changing it rewrites the strings in the
  # browser and redraws the outputs, with no reload, so loaded data and
  # results are kept (Jorge, 2026-11-24)
  lang <- shiny::reactiveVal(.app_lang(session$request, default_lang))
  shiny::observeEvent(input$lang, {
    if (!identical(input$lang, lang())) {
      lang(input$lang)
      shiny::updateQueryString(paste0("?lang=", input$lang), mode = "replace")
      session$sendCustomMessage("mfo-lang", list(lang = input$lang,
                                                 strings = .app_dictionary(input$lang)))
    }
  }, ignoreInit = TRUE)

  data <- .mod_data_server("data", lang)
  analysis <- .mod_analysis_server("analysis", data, lang)
  group <- shiny::reactiveVal(NULL)
  results <- .mod_results_server("results", analysis, group, lang)
  zones <- .mod_zones_server("zones", results, lang)
  .mod_report_server("report", results, zones, group, lang)
  reliability <- .mod_reliability_server("reliability", group, lang)
  invisible(list(lang = lang, data = data, analysis = analysis, results = results,
                 group = group, zones = zones, reliability = reliability))
}
