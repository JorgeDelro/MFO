#' Run the MFO Shiny app
#'
#' Opens the app of the package in the browser: load the data of one
#' participant, describe the protocol, fit the curve and download the results
#' (SPEC 10). The app only calls the functions of the package, so its results
#' are those of [read_cart()], [MFO()], [MFO_kinetics()], [MFO_zones()] and
#' [MFO_reliability()]. The data stay on your computer: the app runs locally.
#'
#' The app needs the packages \pkg{shiny} and \pkg{bslib} (in Suggests).
#'
#' @param ... further arguments to [shiny::runApp()], e.g. `port` or
#'   `launch.browser`.
#'
#' @return Called for its side effect (runs the app); returns what
#'   [shiny::runApp()] returns.
#'
#' @export
#'
#' @examples
#' if (interactive()) run_app()
run_app <- function(...) {
  missing_pkgs <- c("shiny", "bslib")[!vapply(c("shiny", "bslib"), requireNamespace,
                                              logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("The app needs ", paste(missing_pkgs, collapse = " and "), ": ",
         "install.packages(c(", paste0("\"", missing_pkgs, "\"", collapse = ", "), ")).",
         call. = FALSE)
  }
  shiny::runApp(.app(theme = .app_theme_offline_safe()), ...)
}


# The app object
.app <- function(theme = NULL) {
  shiny::shinyApp(ui = function(request) .app_ui(request, theme), server = .app_server)
}
