# Interface of the app (#24): one tab per screen of SPEC 10

.app_ui <- function(request, default_lang = "en", theme = NULL) {
  lang <- .app_lang(request, default_lang)
  i <- function(key) .i18n(key, lang)
  bslib::page_navbar(
    title = "MFO",
    id = "tabs",
    theme = if (is.null(theme)) bslib::bs_theme(version = 5) else theme,
    lang = lang,
    # pages scroll; filling the window squeezes the cards and their plots
    fillable = FALSE,
    header = shiny::tags$head(shiny::tags$script(shiny::HTML(.app_i18n_js))),
    bslib::nav_panel(i("tab.data"), .mod_data_ui("data", lang), value = "data"),
    bslib::nav_panel(i("tab.analysis"), .mod_analysis_ui("analysis", lang), value = "analysis"),
    bslib::nav_panel(i("tab.results"), .mod_results_ui("results", lang), value = "results"),
    bslib::nav_panel(i("tab.zones"), .mod_zones_ui("zones", lang), value = "zones"),
    bslib::nav_panel(i("tab.report"), .mod_report_ui("report", lang), value = "report"),
    bslib::nav_panel(i("tab.reliability"), .mod_reliability_ui("reliability", lang),
                     value = "reliability"),
    bslib::nav_spacer(),
    bslib::nav_item(
      shiny::selectInput("lang", NULL, choices = .app_languages, selected = lang,
                         width = "130px")
    ),
    footer = shiny::div(class = "text-muted small p-2",
                        sprintf("MFO %s.", utils::packageVersion("MFO")), i("app.local"))
  )
}
