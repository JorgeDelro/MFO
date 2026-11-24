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
#' @param lang language of the interface: `"en"` (default) or `"es"`; it can
#'   be changed in the app.
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
run_app <- function(lang = c("en", "es"), ...) {
  lang <- match.arg(lang)
  missing_pkgs <- c("shiny", "bslib")[!vapply(c("shiny", "bslib"), requireNamespace,
                                              logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("The app needs ", paste(missing_pkgs, collapse = " and "), ": ",
         "install.packages(c(", paste0("\"", missing_pkgs, "\"", collapse = ", "), ")).",
         call. = FALSE)
  }
  shiny::runApp(.app(lang), ...)
}


# The app object; lang is the default language, changed with ?lang= in the URL
.app <- function(lang = "en") {
  shiny::shinyApp(ui = function(request) .app_ui(request, lang), server = function(input, output, session) {
    .app_server(input, output, session, lang)
  })
}
