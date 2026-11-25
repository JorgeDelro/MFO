# Results tab of the app (#24, SPEC 10 point 3): curve with MFO, Fatmax, zone
# and crossover, their intervals on demand, SIN kinetics, the warnings of the
# run, and the button that adds the participant to the session's group table.

# Results of an MFO() run as a table: quantity, value, unit and, when
# computed, the interval
.app_results_table <- function(res, t = function(key) .tr(key, "en")) {
  num <- function(x) if (is.null(x) || length(x) == 0) NA_real_ else as.numeric(x)
  pct_vo2 <- "% VO\u2082max"
  rows <- data.frame(
    key = c("MFO", "MFO_relative", "MFO_FFM", "FAT_MAX", "FAT_MAX_HR", "FAT_MAX_porcHRmax",
            "FAT_MAX_load", "FatMax_zone_lower", "FatMax_zone_upper", "crossover_point",
            "VO2max", "R_squared", "RMSE"),
    quantity = c("MFO", t("tbl.mfo_bm"), t("tbl.mfo_ffm"), "Fatmax", t("tbl.hr_fatmax"),
                 "Fatmax", "Fatmax", t("tbl.zone_lower"), t("tbl.zone_upper"),
                 t("tbl.crossover"), "VO\u2082max", "R\u00b2", "RMSE"),
    value = c(num(res$MFO), num(res$MFO_relative), num(res$MFO_FFM), num(res$FAT_MAX),
              num(res$FAT_MAX_HR), num(res$FAT_MAX_porcHRmax), num(res$FAT_MAX_load),
              num(res$FatMax_zone_lower), num(res$FatMax_zone_upper),
              num(res$crossover_point), num(res$VO2max), num(res$R_squared), num(res$RMSE)),
    unit = c("g/min", "mg/kg/min", "mg/kgFFM/min", pct_vo2, t("tbl.bpm"), t("tbl.pct_hrmax"),
             if (is.na(res$load_unit)) "" else res$load_unit, pct_vo2, pct_vo2, pct_vo2,
             paste0("mL/min (", sub("VO2", "VO\u2082", res$VO2max_type), ")"), "", "g/min"),
    stringsAsFactors = FALSE
  )
  rows$lower <- NA_real_
  rows$upper <- NA_real_
  if (!is.null(res$ci)) {
    i <- match(rownames(res$ci), rows$key)
    rows$lower[i[!is.na(i)]] <- res$ci$lower[!is.na(i)]
    rows$upper[i[!is.na(i)]] <- res$ci$upper[!is.na(i)]
  }
  rows <- rows[!is.na(rows$value), , drop = FALSE]
  rownames(rows) <- NULL
  rows
}

# SIN parameters as a table
.app_sin_table <- function(kin, t = function(key) .tr(key, "en")) {
  out <- data.frame(c("d", "s", "t", "Fatmax (SIN, % VO\u2082max)", "Fatmin (% VO\u2082max)",
                      "R\u00b2", t("tbl.fit")),
                    c(format(round(c(kin$d, kin$s, kin$t), 3)),
                      format(round(c(kin$FatMax_SIN, kin$Fatmin), 1)),
                      format(round(kin$R_squared, 3)), kin$fit),
                    stringsAsFactors = FALSE)
  names(out) <- c(t("tbl.parameter"), t("tbl.value"))
  out
}

# Column names of the results table in the interface language
.app_results_names <- function(tab, t = function(key) .tr(key, "en")) {
  tab$key <- NULL
  names(tab) <- c(t("tbl.quantity"), t("tbl.value"), t("tbl.unit"), t("tbl.lower"),
                  t("tbl.upper"))
  tab
}

# Display names of MFO_db columns in the app, the report and the downloads.
# The columns of the package keep their names (porc_VO2 is a candidate to be
# renamed in 1.0; Jorge, 2026-11-25)
.app_display_names <- function(cols, load_unit = NA) {
  map <- c(Step = "Step", Load = if (is.na(load_unit)) "Load" else paste0("Load (", load_unit, ")"),
           porc_VO2 = "%VO\u2082max", VO2 = "VO\u2082 (mL/min)", VCO2 = "VCO\u2082 (mL/min)",
           RER = "RER", HR = "HR (bpm)", FAT = "FAT (g/min)", CHO = "CHO (g/min)",
           Kcal = "Kcal (kcal/min)")
  out <- map[cols]
  unname(ifelse(is.na(out), cols, out))
}

# The steps of the fit (MFO_db) as shown and downloaded
.app_step_table <- function(res) {
  db <- as.data.frame(res$MFO_db)
  cols <- intersect(c("Step", "Load", "porc_VO2", "VO2", "VCO2", "RER", "HR", "FAT", "CHO",
                      "Kcal"), names(db))
  out <- db[, cols, drop = FALSE]
  names(out) <- .app_display_names(cols, res$load_unit)
  rownames(out) <- NULL
  out
}


.mod_results_ui <- function(id) {
  ns <- shiny::NS(id)
  t <- function(key) .tr(key)
  shiny::tagList(
    shiny::uiOutput(ns("status")),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(t("res.curve")),
        shiny::plotOutput(ns("plot"), height = "380px", fill = FALSE),
        .app_figure_controls(ns, "fig_curve", 120),
        shiny::h6(t("res.steps")),
        .app_scroll_table(ns("steps"), "300px"),
        bslib::layout_columns(
          shiny::downloadButton(ns("steps_xlsx"), t("rep.download_xlsx")),
          shiny::downloadButton(ns("steps_csv"), t("rep.download_csv"))
        )
      ),
      bslib::card(
        bslib::card_header(t("res.values")),
        shiny::tableOutput(ns("values")),
        shiny::hr(),
        shiny::radioButtons(ns("ci"), t("res.ci"),
                            choiceNames = .app_labels(c("res.ci.parametric",
                                                              "res.ci.bootstrap")),
                            choiceValues = c("parametric", "bootstrap"), inline = TRUE),
        bslib::layout_columns(
          shiny::numericInput(ns("B"), t("res.B"), 2000, min = 100, step = 100),
          shiny::numericInput(ns("conf"), t("res.conf"), 0.95, min = 0.5, max = 0.99,
                              step = 0.01),
          shiny::numericInput(ns("seed"), t("res.seed"), 1)
        ),
        shiny::actionButton(ns("compute_ci"), t("res.compute_ci")),
        shiny::helpText(t("res.ci.help"))
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(t("res.sin")),
        shiny::plotOutput(ns("sin_plot"), height = "320px", fill = FALSE),
        .app_figure_controls(ns, "fig_sin", 110)
      ),
      bslib::card(
        bslib::card_header(t("res.sin.values")),
        shiny::tableOutput(ns("sin_values"))
      )
    ),
    bslib::card(
      bslib::card_header(t("res.alerts")),
      shiny::uiOutput(ns("alerts"))
    ),
    bslib::card(
      bslib::card_header(t("res.group")),
      bslib::layout_columns(
        shiny::textInput(ns("ID"), t("res.ID")),
        shiny::textInput(ns("session_id"), t("res.session"), "1")
      ),
      shiny::actionButton(ns("add"), t("res.add")),
      shiny::textOutput(ns("group_n"))
    )
  )
}


.mod_results_server <- function(id, analysis, group) {
  shiny::moduleServer(id, function(input, output, session) {
    t <- function(key) .tr(key)

    # Intervals computed for the current run; dropped when the analysis reruns
    ci_run <- shiny::reactiveVal(NULL)
    ci_log <- shiny::reactiveVal(NULL)
    shiny::observeEvent(analysis(), {
      ci_run(NULL)
      ci_log(NULL)
    })

    current <- shiny::reactive({
      a <- analysis()
      shiny::req(a)
      ci <- ci_run()
      if (!is.null(ci) && identical(ci$run, a$run)) ci else a
    })

    shiny::observeEvent(input$compute_ci, {
      a <- analysis()
      shiny::req(a, a$cart)
      r <- shiny::withProgress(
        message = t("res.computing"), value = 0.5,
        .app_capture(.app_run_analysis(a$cart, a$opts, ci = input$ci, B = input$B,
                                       conf = input$conf, seed = .app_na_null(input$seed))))
      ci_log(r)
      if (is.null(r$error)) ci_run(c(r$value, a[c("opts", "run", "date", "cart", "data")],
                                     list(log = a$log, ci_settings = list(
                                       type = input$ci, B = input$B, conf = input$conf,
                                       seed = input$seed))))
    })

    output$status <- shiny::renderUI({
      if (is.null(analysis())) shiny::div(class = "alert alert-secondary", t("res.none"))
    })
    curve_plot <- shiny::reactive(.app_style_plot(current()$res$MFO_plot))
    sin_plot <- shiny::reactive(.app_style_plot(current()$kin$MFO_kinetics_plot))
    output$plot <- shiny::renderPlot(curve_plot())
    .app_figure_download(input, output, "fig_curve", curve_plot, "curve")
    .app_figure_download(input, output, "fig_sin", sin_plot, "SIN")
    output$steps <- shiny::renderTable(.app_step_table(current()$res), digits = 2, na = "")
    output$steps_xlsx <- shiny::downloadHandler(
      filename = function() "MFO_steps.xlsx",
      content = function(file) .app_write_group(.app_step_table(current()$res), file, "xlsx"))
    output$steps_csv <- shiny::downloadHandler(
      filename = function() "MFO_steps.csv",
      content = function(file) .app_write_group(.app_step_table(current()$res), file, "csv"))
    output$values <- shiny::renderTable({
      .app_results_names(.app_results_table(current()$res, t), t)
    }, digits = 3, na = "")
    output$sin_plot <- shiny::renderPlot(sin_plot())
    output$sin_values <- shiny::renderTable(.app_sin_table(current()$kin, t))
    output$alerts <- shiny::renderUI({
      a <- current()
      l <- a$log
      ci <- ci_log()
      if (!is.null(ci)) {
        l$warnings <- c(l$warnings, ci$warnings)
        l$conditions <- c(l$conditions, Filter(function(x) inherits(x, "warning"),
                                               ci$conditions))
        l$error <- ci$error
      }
      if (length(l$warnings) + length(l$messages) + length(l$error) == 0) {
        return(shiny::div(class = "alert alert-success", t("res.no_alerts")))
      }
      .app_alerts(l)
    })

    # Group table of the session: one row per participant and session, the
    # columns of MFOs() plus session and date
    shiny::observeEvent(input$add, {
      a <- current()
      id <- trimws(input$ID)
      if (!nzchar(id)) {
        shiny::showNotification(t("res.ID.missing"), type = "error")
        return()
      }
      if (!.app_valid_id(id)) {
        shiny::showNotification(t("res.ID.invalid"), type = "error")
        return()
      }
      row <- .app_group_row(id, input$session_id, a)
      g <- group()
      if (!is.null(g)) g <- g[!(g$ID == row$ID & g$session == row$session), , drop = FALSE]
      group(rbind(g, row))
      shiny::showNotification(sprintf(t("res.added"), id, row$session), type = "message")
    })
    output$group_n <- shiny::renderText({
      sprintf(t("res.group.n"), if (is.null(group())) 0 else nrow(group()))
    })

    current
  })
}


# Participant codes, not names: letters, digits, _ and -, up to 20
# characters (Jorge, 2026-11-20)
.app_valid_id <- function(x) {
  is.character(x) && length(x) == 1 && grepl("^[A-Za-z0-9_-]{1,20}$", x)
}


# Row of the group table: the MFOs() row with the session and analysis date
.app_group_row <- function(ID, session, a) {
  row <- .mfo_summary_row(ID, a$res, a$kin)
  cbind(row[1], session = as.character(session),
        analysis_date = format(a$date, "%Y-%m-%d %H:%M"), row[-1],
        phase_cuts = .app_cuts_text(a$data$cuts), stringsAsFactors = FALSE)
}

# Phase cuts as text for the downloads ("" when the phases came as tables)
.app_cuts_text <- function(cuts) {
  if (is.null(cuts) || length(cuts) == 0) return("")
  paste(names(cuts), cuts, sep = " = ", collapse = "; ")
}
