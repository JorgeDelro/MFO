# Server of the app (#24)

.app_server <- function(input, output, session) {
  data <- .mod_data_server("data")
  analysis <- .mod_analysis_server("analysis", data)
  group <- shiny::reactiveVal(NULL)
  results <- .mod_results_server("results", analysis, group)
  zones <- .mod_zones_server("zones", results)
  .mod_report_server("report", results, zones, group)
  reliability <- .mod_reliability_server("reliability", group)
  invisible(list(data = data, analysis = analysis, results = results, group = group,
                 zones = zones, reliability = reliability))
}
