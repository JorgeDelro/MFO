# Report and downloads tab of the app (#24, SPEC 10 point 4): the individual
# HTML report of the current result and the group table of the session.

# The group table as a file: xlsx (openxlsx, in Imports) or csv
.app_write_group <- function(group, file, format = c("xlsx", "csv")) {
  format <- match.arg(format)
  if (format == "xlsx") openxlsx::write.xlsx(group, file) else
    utils::write.csv(group, file, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(file)
}


.mod_report_ui <- function(id, lang = "en") {
  ns <- shiny::NS(id)
  t <- function(key) .tr(key, lang)
  bslib::layout_columns(
    col_widths = c(4, 8),
    bslib::card(
      bslib::card_header(t("rep.report")),
      shiny::uiOutput(ns("status")),
      shiny::textInput(ns("ID"), t("res.ID")),
      shiny::helpText(t("rep.report.help")),
      shiny::downloadButton(ns("report"), t("rep.download_report"))
    ),
    bslib::card(
      bslib::card_header(t("res.group")),
      shiny::tableOutput(ns("group")),
      bslib::layout_columns(
        shiny::downloadButton(ns("group_xlsx"), t("rep.download_xlsx")),
        shiny::downloadButton(ns("group_csv"), t("rep.download_csv")),
        shiny::actionButton(ns("clear"), t("rep.clear"))
      ),
      shiny::helpText(t("rep.group.help"))
    )
  )
}


.mod_report_server <- function(id, results, zones, group, lang = "en") {
  shiny::moduleServer(id, function(input, output, session) {
    t <- function(key) .tr(key, lang)

    current <- shiny::reactive(tryCatch(results(), error = function(e) NULL))
    # zones only if computed for the current run
    current_zones <- shiny::reactive({
      z <- zones()
      a <- current()
      if (!is.null(z) && !is.null(a) && identical(z$run, a$run)) z
    })

    output$status <- shiny::renderUI({
      if (is.null(current())) shiny::div(class = "alert alert-secondary", t("res.none"))
    })

    output$report <- shiny::downloadHandler(
      filename = function() {
        id <- trimws(input$ID)
        paste0("MFO_report", if (.app_valid_id(id)) paste0("_", id), ".html")
      },
      content = function(file) {
        a <- current()
        shiny::req(a)
        id <- trimws(input$ID)
        if (nzchar(id) && !.app_valid_id(id)) {
          shiny::showNotification(t("res.ID.invalid"), type = "error")
          id <- ""
        }
        .app_write_report(file, a = a, zones = current_zones(), id = id)
      })

    output$group <- shiny::renderTable({
      g <- group()
      shiny::req(g)
      g[, intersect(c("ID", "session", "analysis_date", "MFO_g_min", "FAT_MAX_perVO2max",
                      "FatMax_zone_lower", "FatMax_zone_upper", "method_used", "phase_cuts"),
                    names(g))]
    }, digits = 3)

    output$group_xlsx <- shiny::downloadHandler(
      filename = function() "MFO_group.xlsx",
      content = function(file) .app_write_group(group(), file, "xlsx"))
    output$group_csv <- shiny::downloadHandler(
      filename = function() "MFO_group.csv",
      content = function(file) .app_write_group(group(), file, "csv"))

    shiny::observeEvent(input$clear, group(NULL))
  })
}
