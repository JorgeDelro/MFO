# Protocol and analysis tab of the app (#24, SPEC 10 point 2): the arguments
# of MFO_protocol(), MFO() and MFO_kinetics(). The run itself is
# .app_run_analysis(), with no Shiny code, so it can be tested directly.

# Numbers typed as text ("300, 180") for variable steps; NULL if empty
.app_numbers <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) return(NULL)
  v <- suppressWarnings(as.numeric(strsplit(x, "[,; ]+")[[1]]))
  v <- v[!is.na(v)]
  if (length(v) == 0) NULL else v
}

.app_na_null <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) NULL else x

# Default options of the analysis: the defaults of MFO() and MFO_protocol()
.app_default_opts <- function() {
  list(type = "step", mode = "cycling", step_duration = "180", steady_state_duration = "60",
       ramp_window = 30, ramp_step = 30, load_unit = "W", load_start = NA,
       load_increment = NA, author = "Frayn", energy_equivalents = "classic",
       method = "P3", cv_var = "RER", RER_cutoff = "none", RER_custom = 1,
       negative_FAT = "truncate", include_basal = FALSE, use_incomplete_step = FALSE,
       min_steps = 6, vo2max_source = "graded", vo2max_window = 30, VO2max = NA, age = NA,
       body_mass = NA, fat_free_mass = NA, HRmax = NA, sin_fit = "normalised")
}

# The MFO_protocol() of the options
.app_protocol <- function(o) {
  ramp <- identical(o$type, "ramp")
  MFO_protocol(type = o$type,
               step_duration = if (ramp) 180 else .app_numbers(o$step_duration),
               steady_state_duration = if (ramp) 60 else .app_numbers(o$steady_state_duration),
               load_start = .app_na_null(o$load_start),
               load_increment = .app_na_null(o$load_increment),
               load_unit = if (identical(o$type, "relative")) "W/kg" else o$load_unit,
               mode = o$mode, ramp_window = o$ramp_window, ramp_step = o$ramp_step)
}

# MFO() and MFO_kinetics() on a cart with the options; ci for the intervals
.app_run_analysis <- function(cart, o, ci = "none", B = 2000, conf = 0.95, seed = NULL) {
  rer_cutoff <- switch(o$RER_cutoff, none = NULL, custom = o$RER_custom, o$RER_cutoff)
  graded <- identical(o$vo2max_source, "graded")
  res <- MFO(step_time = cart$step_time, db_MFO = cart$db_MFO, db_basal = cart$db_basal,
             db_graded = if (graded) cart$db_graded,
             VO2max = if (!graded) .app_na_null(o$VO2max),
             cv_var = o$cv_var, author = o$author, method = o$method,
             RER_cutoff = rer_cutoff, negative_FAT = o$negative_FAT,
             body_mass = .app_na_null(o$body_mass), fat_free_mass = .app_na_null(o$fat_free_mass),
             HRmax = .app_na_null(o$HRmax), vo2max_window = o$vo2max_window,
             include_basal = isTRUE(o$include_basal),
             energy_equivalents = o$energy_equivalents,
             use_incomplete_step = isTRUE(o$use_incomplete_step), min_steps = o$min_steps,
             protocol = .app_protocol(o), age = .app_na_null(o$age),
             ci = ci, B = B, conf = conf, seed = seed)
  kin <- MFO_kinetics(res$MFO_db, fit = o$sin_fit)
  list(res = res, kin = kin)
}


# Select inputs of the tab with translated choices, by input id
.app_analysis_selects <- function(t) {
  named <- function(values, keys) stats::setNames(values, vapply(keys, t, character(1)))
  list(type = named(c("step", "step_variable", "relative", "ramp"),
                    c("an.type.step", "an.type.step_variable", "an.type.relative",
                      "an.type.ramp")),
       mode = named(c("cycling", "running", "walking"),
                    c("an.mode.cycling", "an.mode.running", "an.mode.walking")),
       energy_equivalents = named(c("classic", "source"),
                                  c("an.energy.classic", "an.energy.source")),
       method = named(c("P3", "P2", "MV", "MRER"),
                      c("an.method.P3", "an.method.P2", "an.method.MV", "an.method.MRER")),
       RER_cutoff = named(c("none", "classic", "safe", "sedentary", "trained", "custom"),
                          c("an.RER.none", "an.RER.classic", "an.RER.safe", "an.RER.sedentary",
                            "an.RER.trained", "an.RER.custom")),
       negative_FAT = named(c("truncate", "exclude", "keep"),
                            c("an.negative.truncate", "an.negative.exclude",
                              "an.negative.keep")),
       sin_fit = named(c("normalised", "absolute"), c("an.sin.normalised", "an.sin.absolute")))
}

.mod_analysis_ui <- function(id, lang = "en") {
  ns <- shiny::NS(id)
  i <- function(key) .i18n(key, lang)
  sel <- .app_analysis_selects(function(key) .tr(key, lang))
  d <- .app_default_opts()
  shiny::tagList(
    bslib::layout_column_wrap(
      width = "320px",
      bslib::card(
        bslib::card_header(i("an.protocol")),
        shiny::selectInput(ns("type"), i("an.type"), choices = sel$type),
        shiny::selectInput(ns("mode"), i("an.mode"), choices = sel$mode),
        shiny::conditionalPanel(
          sprintf("input['%s'] != 'ramp'", ns("type")),
          shiny::textInput(ns("step_duration"), i("an.step_duration"), d$step_duration),
          shiny::textInput(ns("steady_state_duration"), i("an.steady_state"),
                           d$steady_state_duration),
          shiny::helpText(i("an.step.help"))
        ),
        shiny::conditionalPanel(
          sprintf("input['%s'] == 'ramp'", ns("type")),
          shiny::numericInput(ns("ramp_window"), i("an.ramp_window"), d$ramp_window, min = 1),
          shiny::numericInput(ns("ramp_step"), i("an.ramp_step"), d$ramp_step, min = 1)
        ),
        shiny::conditionalPanel(
          sprintf("input['%s'] != 'relative'", ns("type")),
          shiny::selectInput(ns("load_unit"), i("an.load_unit"), c("W", "km/h"))
        ),
        shiny::numericInput(ns("load_start"), i("an.load_start"), d$load_start),
        shiny::numericInput(ns("load_increment"), i("an.load_increment"), d$load_increment),
        shiny::helpText(i("an.load.help"))
      ),
      bslib::card(
        bslib::card_header(i("an.calculation")),
        shiny::selectInput(ns("author"), i("an.author"),
                           c("Frayn", "Jeukendrup", "Peronnet")),
        shiny::selectInput(ns("energy_equivalents"), i("an.energy"), sel$energy_equivalents),
        shiny::selectInput(ns("method"), i("an.method"), sel$method),
        shiny::selectInput(ns("cv_var"), i("an.cv_var"), c("RER", "VO2", "VCO2")),
        shiny::selectInput(ns("RER_cutoff"), i("an.RER_cutoff"), sel$RER_cutoff),
        shiny::conditionalPanel(
          sprintf("input['%s'] == 'custom'", ns("RER_cutoff")),
          shiny::numericInput(ns("RER_custom"), i("an.RER.value"), d$RER_custom,
                              min = 0.7, max = 1.2, step = 0.01)
        ),
        shiny::selectInput(ns("negative_FAT"), i("an.negative"), sel$negative_FAT),
        shiny::checkboxInput(ns("include_basal"), i("an.include_basal"), d$include_basal),
        shiny::checkboxInput(ns("use_incomplete_step"), i("an.incomplete"),
                             d$use_incomplete_step),
        shiny::numericInput(ns("min_steps"), i("an.min_steps"), d$min_steps, min = 1)
      ),
      bslib::card(
        bslib::card_header(i("an.participant")),
        shiny::radioButtons(ns("vo2max_source"), i("an.vo2max"),
                            choiceNames = .app_choice_names(c("an.vo2max.graded",
                                                              "an.vo2max.value"), lang),
                            choiceValues = c("graded", "value")),
        shiny::conditionalPanel(
          sprintf("input['%s'] == 'graded'", ns("vo2max_source")),
          shiny::numericInput(ns("vo2max_window"), i("an.vo2max_window"), d$vo2max_window,
                              min = 1)
        ),
        shiny::conditionalPanel(
          sprintf("input['%s'] == 'value'", ns("vo2max_source")),
          shiny::numericInput(ns("VO2max"), i("an.vo2max.ml"), d$VO2max, min = 0)
        ),
        shiny::numericInput(ns("age"), i("an.age"), d$age, min = 0),
        shiny::numericInput(ns("body_mass"), i("an.body_mass"), d$body_mass, min = 0),
        shiny::numericInput(ns("fat_free_mass"), i("an.ffm"), d$fat_free_mass, min = 0),
        shiny::numericInput(ns("HRmax"), i("an.HRmax"), d$HRmax, min = 0),
        shiny::selectInput(ns("sin_fit"), i("an.sin_fit"), sel$sin_fit)
      )
    ),
    shiny::div(class = "my-3",
               shiny::actionButton(ns("run"), i("an.run"), class = "btn-primary")),
    shiny::uiOutput(ns("alerts"))
  )
}


.mod_analysis_server <- function(id, data, lang = "en") {
  lang <- .app_lang_reactive(lang)
  shiny::moduleServer(id, function(input, output, session) {
    t <- function(key) .tr(key, lang())
    .app_relabel(session, input, lang, .app_analysis_selects)
    opts <- shiny::reactive({
      o <- .app_default_opts()
      for (k in names(o)) if (!is.null(input[[k]])) o[[k]] <- input[[k]]
      o
    })

    out <- shiny::reactiveVal(NULL)
    log <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$run, {
      d <- data()
      if (is.null(d)) {
        log(list(error = t("an.no_data"), warnings = character(0), messages = character(0)))
        out(NULL)
        return()
      }
      o <- opts()
      r <- shiny::withProgress(message = t("an.running"), value = 0.5,
                               .app_capture(.app_run_analysis(d$cart, o)))
      log(r)
      out(if (is.null(r$error)) {
        c(r$value, list(cart = d$cart, opts = o, log = r, run = input$run, date = Sys.time(),
                        data = d[c("source", "tables", "cuts", "settings")]))
      })
    })

    output$alerts <- shiny::renderUI({
      l <- log()
      if (is.null(l)) return(NULL)
      .app_alerts(l, if (is.null(l$error)) t("an.ok"))
    })

    out
  })
}


# Alert boxes for a captured run: error, warnings, messages, then success
.app_alerts <- function(l, ok = NULL) {
  box <- function(x, cls) {
    if (length(x) == 0) return(NULL)
    shiny::div(class = paste("alert", cls), lapply(x, shiny::p))
  }
  shiny::tagList(box(l$error, "alert-danger"), box(l$warnings, "alert-warning"),
                 box(l$messages, "alert-info"),
                 if (!is.null(ok)) shiny::div(class = "alert alert-success", ok))
}
