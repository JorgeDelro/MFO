# Strings of the app interface (#24): one file, inst/app/i18n.csv, with a
# key and one column per language. The app is in English only (Jorge,
# 2026-11-25); the key -> text lookup stays, so another language is one more
# column. A key missing in a language falls back to English, and a key
# missing everywhere is shown as itself, so a gap is visible but never breaks
# the app.

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

# Labels of the choices of a radio input, from their keys
.app_labels <- function(keys) unname(vapply(keys, .tr, character(1)))

# Download of a template of inst/app/templates
.app_template_download <- function(file) {
  shiny::downloadHandler(
    filename = function() file,
    content = function(dest) {
      file.copy(system.file("app", "templates", file, package = "MFO"), dest)
    })
}

# A file input with its button and placeholder from i18n.csv
.app_file_input <- function(id, key, ...) {
  shiny::fileInput(id, .tr(key), buttonLabel = .tr("data.browse"),
                   placeholder = .tr("data.no_file"), ...)
}
