# Interface of the app (#24): one tab per screen of SPEC 10

.app_ui <- function(request, default_lang = "en") {
  lang <- .app_lang(request, default_lang)
  t <- function(key) .tr(key, lang)
  bslib::page_navbar(
    title = "MFO",
    id = "tabs",
    theme = bslib::bs_theme(version = 5),
    lang = lang,
    bslib::nav_panel(t("tab.data"), .mod_data_ui("data", lang)),
    bslib::nav_panel(t("tab.analysis"), .mod_analysis_ui("analysis", lang)),
    bslib::nav_panel(t("tab.results"), .mod_results_ui("results", lang)),
    bslib::nav_panel(t("tab.zones"), .mod_zones_ui("zones", lang)),
    bslib::nav_panel(t("tab.report"), .mod_report_ui("report", lang)),
    bslib::nav_panel(t("tab.reliability"), .mod_reliability_ui("reliability", lang)),
    bslib::nav_spacer(),
    bslib::nav_item(
      shiny::selectInput("lang", NULL, choices = .app_languages, selected = lang,
                         width = "130px")
    ),
    footer = shiny::div(class = "text-muted small p-2",
                        sprintf("MFO %s. %s", utils::packageVersion("MFO"), t("app.local")))
  )
}
