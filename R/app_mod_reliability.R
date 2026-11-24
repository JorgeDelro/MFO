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


.mod_reliability_ui <- function(id, lang = "en") {
  ns <- shiny::NS(id)
  i <- function(key) .i18n(key, lang)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 340,
      shiny::radioButtons(ns("source"), i("rel.source"),
                          choiceNames = .app_choice_names(c("rel.source.session",
                                                            "rel.source.file"), lang),
                          choiceValues = c("session", "file")),
      shiny::conditionalPanel(
        sprintf("input['%s'] == 'file'", ns("source")),
        .app_file_input(ns("file"), "rel.file", lang, accept = c(".csv", ".xlsx"))
      ),
      shiny::numericInput(ns("conf"), i("res.conf"), 0.95, min = 0.5, max = 0.99, step = 0.01),
      shiny::helpText(i("rel.help")),
      shiny::actionButton(ns("run"), i("rel.run"), class = "btn-primary")
    ),
    shiny::uiOutput(ns("alerts")),
    bslib::card(bslib::card_header(i("rel.table")), shiny::tableOutput(ns("table"))),
    bslib::card(bslib::card_header(i("rel.pairs")), shiny::tableOutput(ns("pairs"))),
    bslib::card(bslib::card_header(i("rel.plot")),
                shiny::numericInput(ns("pair"), i("rel.pair"), 1, min = 1, step = 1),
                shiny::plotOutput(ns("plot"), height = "420px", fill = FALSE))
  )
}


.mod_reliability_server <- function(id, group, lang = "en") {
  lang <- .app_lang_reactive(lang)
  shiny::moduleServer(id, function(input, output, session) {
    t <- function(key) .tr(key, lang())
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
      .app_alerts(l, lang = lang())
    })
    output$table <- shiny::renderTable({
      shiny::req(out())
      out()$table
    }, digits = 3)
    output$pairs <- shiny::renderTable({
      shiny::req(out())
      out()$pairs
    }, digits = 3)
    output$plot <- shiny::renderPlot({
      shiny::req(out())
      plot(out(), pair = input$pair)
    })
    out
  })
}
