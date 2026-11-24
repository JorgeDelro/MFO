# Data tab of the app (#24, SPEC 10 point 1): source, files or zip, column
# mapping, phases by file/sheet or by cuts on the recording. The reading is
# done by read_cart(); the helpers below have no Shiny code so they can be
# tested directly.

# Contract columns that the mapping offers; the first three are required
.app_map_cols <- c("Time", "VO2", "VCO2", "RER", "HR", "Load", "VE")

# Example data of the package as an MFO_cart (20-s records)
.app_example_data <- function() {
  e <- new.env()
  utils::data(list = c("basal_df", "MFO_df", "VO2max_df"), package = "MFO", envir = e)
  structure(list(db_basal = as.data.frame(e$basal_df), db_MFO = as.data.frame(e$MFO_df),
                 db_graded = as.data.frame(e$VO2max_df), step_time = 20,
                 info = list(device = "example data", interval = 20,
                             averaging = "none (records as exported)", vo2_unit = "mL/min",
                             cuts = NULL, n_breaths = nrow(e$basal_df) + nrow(e$MFO_df) +
                               nrow(e$VO2max_df), n_intervals = NA, empty_intervals = 0,
                             RER_mismatch = NA_integer_)),
            class = "MFO_cart")
}


# Uploaded files (name, datapath, as given by fileInput) -> one row per table:
# zips are unpacked, and with expand_sheets every sheet of an xlsx is a table
.app_tables <- function(files, expand_sheets = TRUE, exdir = tempfile("mfo_zip_")) {
  if (is.null(files) || nrow(files) == 0) {
    return(data.frame(label = character(0), path = character(0), sheet = character(0),
                      stringsAsFactors = FALSE))
  }
  rows <- list()
  add <- function(label, path, sheet = NA_character_) {
    rows[[length(rows) + 1]] <<- data.frame(label = label, path = path, sheet = sheet,
                                            stringsAsFactors = FALSE)
  }
  is_ext <- function(x, e) grepl(paste0("\\.(", paste(e, collapse = "|"), ")$"), x,
                                 ignore.case = TRUE)
  for (i in seq_len(nrow(files))) {
    name <- files$name[i]
    path <- files$datapath[i]
    if (is_ext(name, "zip")) {
      dir <- file.path(exdir, i)
      inside <- utils::unzip(path, exdir = dir)
      inside <- inside[!grepl("(^|/)(__MACOSX|\\.)", sub(paste0("^", dir, "/"), "", inside))]
      for (f in inside) {
        fake <- data.frame(name = sub(paste0("^", dir, "/"), "", f), datapath = f,
                           stringsAsFactors = FALSE)
        rows[[length(rows) + 1]] <- .app_tables(fake, expand_sheets, exdir)
      }
    } else if (expand_sheets && is_ext(name, c("xlsx", "xls"))) {
      sheets <- readxl::excel_sheets(path)
      if (length(sheets) == 1) {
        add(name, path, sheets)
      } else {
        for (sh in sheets) add(paste0(name, " [", sh, "]"), path, sh)
      }
    } else {
      add(name, path)
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}


# Read the data with read_cart(): one table per phase (assign$mode = "phases",
# with assign$basal, $MFO, $graded = table labels or NA) or one recording with
# cuts (assign$mode = "cuts", assign$table, assign$cuts). settings holds the
# read_cart() arguments.
.app_read_data <- function(tables, assign, settings) {
  generic <- identical(settings$device, "generic")
  get <- function(label) {
    i <- match(label, tables$label)
    if (is.na(i)) stop("Table not found: ", label, call. = FALSE)
    tables[i, ]
  }
  read <- function(tab, phase = "MFO", cuts = NULL) {
    args <- list(file = tab$path, interval = settings$interval, phase = phase,
                 cuts = cuts, vo2_unit = settings$vo2_unit, body_mass = settings$body_mass)
    if (generic) {
      args <- c(args, list(device = "generic", columns = settings$columns,
                           skip = settings$skip, units_row = settings$units_row,
                           decimal = settings$decimal, time_origin = settings$time_origin,
                           sheets = if (is.na(tab$sheet)) NULL else tab$sheet))
    } else {
      args$device <- settings$device
    }
    do.call(read_cart, args)
  }
  if (identical(assign$mode, "cuts")) {
    cuts <- assign$cuts[!is.na(assign$cuts)]
    if (length(cuts) == 0) stop("Give basal_end and/or MFO_end.", call. = FALSE)
    return(read(get(assign$table), cuts = cuts))
  }
  phases <- c("basal", "MFO", "graded")
  chosen <- phases[!vapply(phases, function(p) is.null(assign[[p]]) || is.na(assign[[p]]) ||
                                                 assign[[p]] == "", logical(1))]
  if (!"MFO" %in% chosen) stop("Choose the table of the MFO test.", call. = FALSE)
  parts <- lapply(chosen, function(p) read(get(assign[[p]]), phase = p))
  names(parts) <- chosen
  steps <- vapply(parts, function(p) p$step_time, numeric(1))
  if (length(unique(steps)) > 1) {
    stop("The tables have different steps (", paste(names(steps), steps, sep = " = ",
         collapse = ", "), " s); MFO() needs one step_time.", call. = FALSE)
  }
  info <- parts$MFO$info
  info$tables <- unlist(assign[chosen])
  structure(list(db_basal = parts$basal$db_basal, db_MFO = parts$MFO$db_MFO,
                 db_graded = parts$graded$db_graded, step_time = steps[[1]], info = info),
            class = "MFO_cart")
}


# Run expr and keep its value, messages, warnings and error for the alerts.
# The conditions are kept too, so the alerts can word the classed ones of
# the package in the interface language (.app_condition_text())
.app_capture <- function(expr) {
  msgs <- list()
  warns <- list()
  value <- tryCatch(
    withCallingHandlers(expr,
      message = function(m) {
        msgs[[length(msgs) + 1]] <<- m
        invokeRestart("muffleMessage")
      },
      warning = function(w) {
        warns[[length(warns) + 1]] <<- w
        invokeRestart("muffleWarning")
      }),
    error = function(e) structure(conditionMessage(e), class = "app_error"))
  error <- if (inherits(value, "app_error")) as.character(value) else NULL
  text <- function(l) vapply(l, function(x) trimws(conditionMessage(x)), character(1))
  list(value = if (is.null(error)) value, messages = text(msgs), warnings = text(warns),
       error = error, conditions = c(warns, msgs))
}


# A condition in the interface language: the classed conditions of the
# package (mfo_warning_<id>, mfo_message_<id>) have a template cond.<id> in
# i18n.csv whose {field} or {field:format} are filled with the fields of
# the condition; any other condition keeps its message (Jorge, 2026-11-24)
.app_condition_text <- function(cond, lang = "en") {
  if (is.character(cond)) return(cond)
  id <- grep("^mfo_(warning|message)_", class(cond), value = TRUE)
  key <- if (length(id) > 0) sub("^mfo_(warning|message)_", "cond.", id[1])
  if (is.null(key) || !key %in% .app_strings()$key) return(trimws(conditionMessage(cond)))
  .app_fill(.tr(key, lang), cond, lang)
}

# Fill {field} and {field:format} (a sprintf format); numbers take the
# decimal mark of the language
.app_fill <- function(template, fields, lang = "en") {
  marks <- regmatches(template, gregexpr("\\{[A-Za-z_]+(:[^}]+)?\\}", template))[[1]]
  for (m in unique(marks)) {
    inner <- substr(m, 2, nchar(m) - 1)
    name <- sub(":.*$", "", inner)
    fmt <- if (grepl(":", inner)) sub("^[^:]*:", "", inner) else NULL
    v <- fields[[name]]
    txt <- if (is.null(v)) m else if (is.numeric(v)) {
      out <- if (is.null(fmt)) format(v) else sprintf(fmt, v)
      if (lang == "es") out <- chartr(".", ",", out)
      paste(out, collapse = ", ")
    } else {
      paste(as.character(v), collapse = ", ")
    }
    template <- gsub(m, txt, template, fixed = TRUE)
  }
  template
}


# One row per phase: records, time span and step
.app_phase_summary <- function(cart, t = function(key) .tr(key, "en")) {
  rows <- lapply(c("basal", "MFO", "graded"), function(p) {
    db <- cart[[paste0("db_", p)]]
    if (is.null(db)) return(NULL)
    data.frame(phase = p, records = nrow(db),
               duration_s = nrow(db) * cart$step_time,
               columns = paste(names(db), collapse = ", "), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  names(out) <- c(t("tbl.phase"), t("tbl.records"), t("tbl.duration"), t("tbl.columns"))
  out
}


# Select inputs of the tab with translated choices, by input id
.app_data_selects <- function(t) {
  list(device = stats::setNames(c("auto", "cortex", "cosmed", "vyntus", "zan"),
                                c(t("data.device.auto"), "Cortex", "Cosmed", "Vyntus", "ZAN")),
       vo2_unit = stats::setNames(c("auto", "mL/min", "L/min", "mL/kg/min"),
                                  c(t("data.vo2_unit.auto"), "mL/min", "L/min", "mL/kg/min")))
}

.mod_data_ui <- function(id, lang = "en") {
  ns <- shiny::NS(id)
  t <- function(key) .tr(key, lang)
  i <- function(key) .i18n(key, lang)
  radio <- function(id, label, values, keys, ...) {
    shiny::radioButtons(ns(id), i(label), choiceNames = .app_choice_names(keys, lang),
                        choiceValues = values, ...)
  }
  sel <- .app_data_selects(t)
  map_input <- function(col) {
    shiny::selectInput(ns(paste0("map_", col)), col, choices = character(0))
  }
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 380,
      radio("source", "data.source", c("example", "generic", "device"),
            c("data.source.example", "data.source.generic", "data.source.device")),
      shiny::conditionalPanel(
        sprintf("input['%s'] != 'example'", ns("source")),
        .app_file_input(ns("files"), "data.files", lang, multiple = TRUE),
        shiny::helpText(i("data.files.help")),
        shiny::conditionalPanel(
          sprintf("input['%s'] == 'device'", ns("source")),
          shiny::selectInput(ns("device"), i("data.device"), choices = sel$device)
        ),
        radio("assign", "data.assign", c("phases", "cuts"),
              c("data.assign.phases", "data.assign.cuts")),
        shiny::conditionalPanel(
          sprintf("input['%s'] == 'phases'", ns("assign")),
          shiny::selectInput(ns("tab_basal"), i("data.phase.basal"), choices = character(0)),
          shiny::selectInput(ns("tab_MFO"), i("data.phase.MFO"), choices = character(0)),
          shiny::selectInput(ns("tab_graded"), i("data.phase.graded"), choices = character(0))
        ),
        shiny::conditionalPanel(
          sprintf("input['%s'] == 'cuts'", ns("assign")),
          shiny::selectInput(ns("tab_recording"), i("data.recording"), choices = character(0)),
          shiny::numericInput(ns("basal_end"), i("data.basal_end"), value = NA, min = 0),
          shiny::numericInput(ns("MFO_end"), i("data.MFO_end"), value = NA, min = 0),
          shiny::helpText(i("data.cuts.help"))
        ),
        bslib::accordion(
          open = FALSE,
          bslib::accordion_panel(
            i("data.options"), value = "options",
            radio("interval_mode", "data.interval", c("none", "numeric"),
                  c("data.interval.none", "data.interval.numeric")),
            shiny::conditionalPanel(
              sprintf("input['%s'] == 'numeric'", ns("interval_mode")),
              shiny::numericInput(ns("interval"), i("data.interval.s"), value = 10, min = 1)
            ),
            shiny::selectInput(ns("vo2_unit"), i("data.vo2_unit"), choices = sel$vo2_unit),
            shiny::numericInput(ns("body_mass"), i("data.body_mass"), value = NA, min = 0),
            shiny::conditionalPanel(
              sprintf("input['%s'] == 'generic'", ns("source")),
              shiny::numericInput(ns("skip"), i("data.skip"), value = 0, min = 0, step = 1),
              shiny::checkboxInput(ns("units_row"), i("data.units_row"), value = FALSE),
              shiny::radioButtons(ns("decimal"), i("data.decimal"), choices = c(".", ","),
                                  inline = TRUE),
              radio("time_origin", "data.time_origin", c("elapsed", "clock"),
                    c("data.time_origin.elapsed", "data.time_origin.clock"))
            )
          ),
          bslib::accordion_panel(
            i("data.mapping"), value = "mapping",
            shiny::conditionalPanel(
              sprintf("input['%s'] == 'generic'", ns("source")),
              shiny::helpText(i("data.mapping.help")),
              lapply(.app_map_cols, map_input)
            ),
            shiny::conditionalPanel(
              sprintf("input['%s'] != 'generic'", ns("source")),
              shiny::helpText(i("data.mapping.device"))
            )
          )
        )
      ),
      shiny::actionButton(ns("read"), i("data.read"), class = "btn-primary")
    ),
    shiny::uiOutput(ns("alerts")),
    bslib::card(
      bslib::card_header(i("data.recording.plot")),
      shiny::plotOutput(ns("plot"), height = "300px", fill = FALSE,
                        brush = shiny::brushOpts(ns("brush"), direction = "x", resetOnNew = TRUE))
    ),
    bslib::card(
      bslib::card_header(i("data.summary")),
      shiny::tableOutput(ns("summary"))
    )
  )
}


.mod_data_server <- function(id, lang = "en") {
  lang <- .app_lang_reactive(lang)
  shiny::moduleServer(id, function(input, output, session) {
    t <- function(key) .tr(key, lang())
    .app_relabel(session, input, lang, .app_data_selects)

    tables <- shiny::reactive({
      .app_tables(input$files, expand_sheets = identical(input$source, "generic"))
    })

    # Table choices: the MFO test is required, the other phases optional.
    # New files set the defaults; a new language only relabels "none"
    shiny::observe({
      labs <- tables()$label
      none <- stats::setNames("", shiny::isolate(t("data.none")))
      shiny::updateSelectInput(session, "tab_basal", choices = c(none, labs),
                               selected = if (length(labs) > 1) labs[1] else "")
      shiny::updateSelectInput(session, "tab_MFO", choices = labs,
                               selected = if (length(labs) > 1) labs[2] else labs[1])
      shiny::updateSelectInput(session, "tab_graded", choices = c(none, labs),
                               selected = if (length(labs) > 2) labs[3] else "")
      shiny::updateSelectInput(session, "tab_recording", choices = labs)
    })
    shiny::observeEvent(lang(), {
      none <- stats::setNames("", t("data.none"))
      labs <- tables()$label
      for (p in c("tab_basal", "tab_graded")) {
        shiny::updateSelectInput(session, p, choices = c(none, labs), selected = input[[p]])
      }
      h <- header()
      for (col in setdiff(.app_map_cols, c("Time", "VO2", "VCO2"))) {
        id <- paste0("map_", col)
        shiny::updateSelectInput(session, id, choices = c(none, h), selected = input[[id]])
      }
    }, ignoreInit = TRUE)

    # Column mapping from the header of the MFO table (or the recording)
    header <- shiny::reactive({
      tabs <- tables()
      if (!identical(input$source, "generic") || nrow(tabs) == 0) return(character(0))
      lab <- if (identical(input$assign, "cuts")) input$tab_recording else input$tab_MFO
      i <- match(lab, tabs$label)
      if (is.na(i)) i <- 1
      tryCatch(.generic_header(tabs$path[i], if (is.na(tabs$sheet[i])) NULL else tabs$sheet[i],
                               skip = if (is.na(input$skip)) 0 else input$skip,
                               decimal = input$decimal),
               error = function(e) character(0))
    })
    shiny::observe({
      h <- header()
      for (col in .app_map_cols) {
        required <- col %in% c("Time", "VO2", "VCO2")
        choices <- if (required) h else c(stats::setNames("", shiny::isolate(t("data.none"))), h)
        shiny::updateSelectInput(session, paste0("map_", col), choices = choices,
                                 selected = if (col %in% h) col else if (required) h[1] else "")
      }
    })

    settings <- shiny::reactive({
      columns <- vapply(.app_map_cols, function(col) {
        v <- input[[paste0("map_", col)]]
        if (is.null(v)) "" else v
      }, character(1))
      columns <- columns[columns != ""]
      list(device = switch(input$source, generic = "generic",
                           device = if (identical(input$device, "auto")) NULL else input$device),
           columns = if (length(columns) > 0) columns,
           skip = if (is.na(input$skip)) 0 else input$skip,
           units_row = isTRUE(input$units_row),
           decimal = input$decimal,
           time_origin = input$time_origin,
           interval = if (identical(input$interval_mode, "none")) "none" else input$interval,
           vo2_unit = input$vo2_unit,
           body_mass = if (is.na(input$body_mass)) NULL else input$body_mass)
    })

    # Whole recording without cuts, for the plot of the cuts mode
    preview <- shiny::reactive({
      shiny::req(identical(input$assign, "cuts"), input$tab_recording)
      .app_capture(.app_read_data(tables(), list(mode = "phases", MFO = input$tab_recording),
                                  settings()))$value
    })

    # Dragging over the MFO test sets both cuts
    shiny::observeEvent(input$brush, {
      shiny::updateNumericInput(session, "basal_end", value = round(input$brush$xmin))
      shiny::updateNumericInput(session, "MFO_end", value = round(input$brush$xmax))
    })

    result <- shiny::reactiveVal(NULL)
    log <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$read, {
      if (identical(input$source, "example")) {
        out <- .app_capture(.app_example_data())
        meta <- list(source = "example", tables = NULL, cuts = NULL, settings = NULL)
      } else {
        assign <- if (identical(input$assign, "cuts")) {
          list(mode = "cuts", table = input$tab_recording,
               cuts = c(basal_end = input$basal_end, MFO_end = input$MFO_end))
        } else {
          list(mode = "phases", basal = input$tab_basal, MFO = input$tab_MFO,
               graded = input$tab_graded)
        }
        out <- .app_capture(.app_read_data(tables(), assign, settings()))
        meta <- list(source = input$source, tables = assign, settings = settings(),
                     cuts = if (identical(assign$mode, "cuts")) assign$cuts[!is.na(assign$cuts)])
      }
      log(out)
      result(if (is.null(out$error)) c(list(cart = out$value), meta))
    })

    output$alerts <- shiny::renderUI({
      l <- log()
      if (is.null(l)) return(NULL)
      .app_alerts(l, if (is.null(l$error)) t("data.read.ok"), lang())
    })

    output$plot <- shiny::renderPlot({
      cart <- if (identical(input$assign, "cuts") && !identical(input$source, "example")) {
        preview()
      } else {
        result()$cart
      }
      shiny::req(cart)
      .app_recording_plot(cart, c(input$basal_end, input$MFO_end), t)
    })

    output$summary <- shiny::renderTable({
      shiny::req(result())
      .app_phase_summary(result()$cart, t)
    }, digits = 0)

    result
  })
}


# VO2 against time of the phases in a cart, with the cuts as vertical lines.
# One phase: its own Time (the cuts refer to it); several: records one after
# another
.app_recording_plot <- function(cart, cuts = NULL, t = function(key) .tr(key, "en")) {
  rows <- lapply(c("basal", "MFO", "graded"), function(p) {
    db <- cart[[paste0("db_", p)]]
    if (is.null(db)) return(NULL)
    tm <- if (is.numeric(db$Time)) db$Time else seq_len(nrow(db)) * cart$step_time
    data.frame(phase = p, Time = tm, VO2 = db$VO2, stringsAsFactors = FALSE)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) > 1) {
    for (i in seq_along(rows)) {
      start <- if (i == 1) 0 else max(rows[[i - 1]]$Time)
      rows[[i]]$Time <- start + seq_len(nrow(rows[[i]])) * cart$step_time
    }
  }
  d <- do.call(rbind, rows)
  d$phase <- factor(d$phase, levels = c("basal", "MFO", "graded"))
  col <- .app_colours
  p <- ggplot2::ggplot(d, ggplot2::aes(x = Time, y = VO2, colour = phase)) +
    ggplot2::geom_line() +
    ggplot2::scale_colour_manual(
      values = c(basal = col[["secondary"]], MFO = col[["primary"]], graded = col[["info"]]),
      labels = c(basal = t("data.phase.basal"), MFO = t("data.phase.MFO"),
                 graded = t("data.phase.graded"))) +
    ggplot2::labs(x = t("data.plot.time"), y = "VO\u2082 (mL/min)", colour = NULL) +
    ggplot2::theme_minimal()
  cuts <- cuts[!is.na(cuts)]
  if (length(cuts) > 0) p <- p + ggplot2::geom_vline(xintercept = cuts, linetype = "dashed")
  p
}
