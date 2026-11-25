# Interface of the app (#24): one tab per screen of SPEC 10

.app_ui <- function(request, theme = NULL) {
  t <- function(key) .tr(key)
  bslib::page_navbar(
    title = t("app.title"),
    id = "tabs",
    theme = if (is.null(theme)) bslib::bs_theme(version = 5) else theme,
    lang = "en",
    # pages scroll; filling the window squeezes the cards and their plots
    fillable = FALSE,
    bslib::nav_panel(t("tab.data"), .mod_data_ui("data"), value = "data"),
    bslib::nav_panel(t("tab.analysis"), .mod_analysis_ui("analysis"), value = "analysis"),
    bslib::nav_panel(t("tab.results"), .mod_results_ui("results"), value = "results"),
    bslib::nav_panel(t("tab.zones"), .mod_zones_ui("zones"), value = "zones"),
    bslib::nav_panel(t("tab.report"), .mod_report_ui("report"), value = "report"),
    bslib::nav_panel(t("tab.reliability"), .mod_reliability_ui("reliability"),
                     value = "reliability"),
    footer = shiny::div(class = "text-muted small p-2",
                        sprintf("MFO %s. %s", utils::packageVersion("MFO"), t("app.local")))
  )
}
