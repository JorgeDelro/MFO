# Reliability tab of the app (#24, SPEC 10 point 5, SPEC 12): MFO_reliability()
# on the group table, from this session or uploaded (as downloaded from the
# Report tab), with ID as the participant and the session column.

# Group table -> input of MFO_reliability(): participant = ID, session
.app_reliability_input <- function(group) {
  if (is.null(group) || nrow(group) == 0) stop("The group table is empty.", call. = FALSE)
  if (!all(c("ID", "session") %in% names(group))) {
    stop("The group table needs the columns ID and session.", call. = FALSE)
  }
  out <- group
  out$participant <- as.character(out$ID)
  out$session <- as.character(out$session)
  out
}

# A downloaded group table (xlsx or csv) read back
.app_read_group <- function(path, name = path) {
  if (grepl("\\.xlsx$", name, ignore.case = TRUE)) {
    as.data.frame(readxl::read_excel(path))
  } else {
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  }
}


.mod_reliability_ui <- function(id) {
  ns <- shiny::NS(id)
  t <- function(key) .tr(key)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 340,
      shiny::radioButtons(ns("source"), t("rel.source"),
                          choiceNames = .app_labels(c("rel.source.session",
                                                            "rel.source.file")),
                          choiceValues = c("session", "file")),
      shiny::conditionalPanel(
        sprintf("input['%s'] == 'file'", ns("source")),
        .app_file_input(ns("file"), "rel.file", accept = c(".csv", ".xlsx"))
      ),
      shiny::numericInput(ns("conf"), t("res.conf"), 0.95, min = 0.5, max = 0.99, step = 0.01),
      shiny::helpText(t("rel.help")),
      shiny::actionButton(ns("run"), t("rel.run"), class = "btn-primary")
    ),
    shiny::uiOutput(ns("alerts")),
    bslib::card(bslib::card_header(t("rel.table")), shiny::tableOutput(ns("table"))),
    bslib::card(bslib::card_header(t("rel.pairs")), shiny::tableOutput(ns("pairs"))),
    bslib::card(bslib::card_header(t("rel.plot")),
                shiny::numericInput(ns("pair"), t("rel.pair"), 1, min = 1, step = 1),
                shiny::plotOutput(ns("plot"), height = "420px", fill = FALSE),
                .app_figure_controls(ns, "fig_bland_altman", 120))
  )
}


.mod_reliability_server <- function(id, group) {
  shiny::moduleServer(id, function(input, output, session) {
    t <- function(key) .tr(key)
    out <- shiny::reactiveVal(NULL)
    log <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$run, {
      r <- .app_capture({
        g <- if (identical(input$source, "file")) {
          if (is.null(input$file)) stop(t("rel.no_file"), call. = FALSE)
          .app_read_group(input$file$datapath, input$file$name)
        } else {
          group()
        }
        MFO_reliability(.app_reliability_input(g), conf = input$conf)
      })
      log(r)
      out(r$value)
    })

    output$alerts <- shiny::renderUI({
      l <- log()
      if (is.null(l)) return(shiny::div(class = "alert alert-secondary", t("rel.none")))
      .app_alerts(l)
    })
    output$table <- shiny::renderTable({
      shiny::req(out())
      out()$table
    }, digits = 3)
    output$pairs <- shiny::renderTable({
      shiny::req(out())
      out()$pairs
    }, digits = 3)
    ba_plot <- shiny::reactive({
      shiny::req(out())
      plot(out(), pair = input$pair)
    })
    output$plot <- shiny::renderPlot(ba_plot())
    .app_figure_download(input, output, "fig_bland_altman", ba_plot, "Bland_Altman")
    out
  })
}
