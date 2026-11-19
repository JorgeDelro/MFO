# Strings of the app interface (#24): one file, inst/app/i18n.csv, with a
# key and one column per language. English is the default; a key missing in a
# language falls back to English, and a key missing everywhere is shown as
# itself, so a gap is visible but never breaks the app.

.app_languages <- stats::setNames(c("en", "es"), c("English", "Espa\u00f1ol"))

.app_strings <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      f <- system.file("app", "i18n.csv", package = "MFO")
      cache <<- utils::read.csv(f, stringsAsFactors = FALSE, encoding = "UTF-8",
                                na.strings = character(0))
    }
    cache
  }
})

.tr <- function(key, lang = "en") {
  s <- .app_strings()
  i <- match(key, s$key)
  if (is.na(i)) return(key)
  if (!lang %in% names(s)) lang <- "en"
  v <- s[[lang]][i]
  if (is.na(v) || v == "") v <- s$en[i]
  v
}

# Language of a request: ?lang= in the URL, else the default
.app_lang <- function(request, default = "en") {
  q <- shiny::parseQueryString(if (is.null(request$QUERY_STRING)) "" else request$QUERY_STRING)
  if (!is.null(q$lang) && q$lang %in% .app_languages) q$lang else default
}
