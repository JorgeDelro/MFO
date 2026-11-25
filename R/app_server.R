# Server of the app (#24)

.app_server <- function(input, output, session) {
  # Plot theme following the dark-mode switch of the navbar
  plot_theme <- shiny::reactive(.app_plot_theme(identical(input$dark_mode, "dark")))
  data <- .mod_data_server("data", plot_theme)
  analysis <- .mod_analysis_server("analysis", data)
  group <- shiny::reactiveVal(NULL)
  results <- .mod_results_server("results", analysis, group, plot_theme)
  zones <- .mod_zones_server("zones", results)
  .mod_report_server("report", results, zones, group)
  reliability <- .mod_reliability_server("reliability", group, plot_theme)
  invisible(list(data = data, analysis = analysis, results = results, group = group,
                 zones = zones, reliability = reliability, plot_theme = plot_theme))
}
