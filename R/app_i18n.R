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

# Language of a request: ?lang= in the URL (kept up to date when the language
# changes, so a reload keeps it), else the default
.app_lang <- function(request, default = "en") {
  q <- shiny::parseQueryString(if (is.null(request$QUERY_STRING)) "" else request$QUERY_STRING)
  if (!is.null(q$lang) && q$lang %in% .app_languages) q$lang else default
}


# Changing the language keeps the state of the session (Jorge, 2026-11-24):
# no reload. Fixed strings of the interface are spans marked with their key;
# the server sends the strings of the new language and the script below
# rewrites them in the browser. Choices of select inputs are relabelled by
# the server (.app_relabel()), and outputs depend on the language reactive,
# so they are redrawn.

# A fixed string of the interface
.i18n <- function(key, lang = "en") shiny::span(`data-i18n` = key, .tr(key, lang))

# Labels of choices: named values for a select input, spans for radio buttons
.app_choices <- function(values, keys, lang = "en") {
  stats::setNames(values, vapply(keys, .tr, character(1), lang = lang))
}
.app_choice_names <- function(keys, lang = "en") lapply(keys, .i18n, lang = lang)

# A file input with its button and placeholder in the interface language
.app_file_input <- function(id, key, lang = "en", ...) {
  htmltools::tagQuery(
    shiny::fileInput(id, .i18n(key, lang), buttonLabel = .i18n("data.browse", lang),
                     placeholder = .tr("data.no_file", lang), ...)
  )$find("input")$filter(function(x, i) identical(x$attribs$type, "text"))$addAttrs(`data-i18n-placeholder` = "data.no_file")$allTags()
}

# All the strings of a language, by key, for the browser
.app_dictionary <- function(lang) {
  s <- .app_strings()
  stats::setNames(as.list(vapply(s$key, .tr, character(1), lang = lang, USE.NAMES = FALSE)),
                  s$key)
}

.app_i18n_js <- "
Shiny.addCustomMessageHandler('mfo-lang', function(m) {
  document.documentElement.lang = m.lang;
  document.querySelectorAll('[data-i18n]').forEach(function(el) {
    var v = m.strings[el.getAttribute('data-i18n')];
    if (v !== undefined) el.textContent = v;
  });
  document.querySelectorAll('[data-i18n-placeholder]').forEach(function(el) {
    var v = m.strings[el.getAttribute('data-i18n-placeholder')];
    if (v !== undefined) el.setAttribute('placeholder', v);
  });
});
"

# A language reactive from a fixed language or a reactive (tests pass "en")
.app_lang_reactive <- function(lang) {
  if (is.function(lang)) lang else function() lang
}

# Relabel select inputs in the new language, keeping what is selected;
# choices(t) returns a named list: input id -> named choices
.app_relabel <- function(session, input, lang, choices) {
  shiny::observeEvent(lang(), {
    t <- function(key) .tr(key, lang())
    ch <- choices(t)
    for (id in names(ch)) {
      shiny::updateSelectInput(session, id, choices = ch[[id]], selected = input[[id]])
    }
  }, ignoreInit = TRUE)
}
