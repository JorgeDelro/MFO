# Zones and references tab of the app (#24, SPEC 10 point 4, SPEC 15):
# MFO_zones() on the current result. The arguments are those of MFO_zones();
# HRmax and age default to the ones given in Protocol and analysis.

# Groups of Table 1 of Amaro-Gahete et al. (2018), the only comparisons
# MFO_zones() accepts: one selector, so a combination the table does not
# print cannot be chosen (Jorge, 2026-11-24)
.app_compare_by <- c("cohort", "sex", "age", "sex_age", "weight")

# The sex, age_group and weight_status that a "compare by" choice uses; the
# others are dropped even if their selectors keep a value
.app_reference_group <- function(z) {
  by <- if (is.null(z$compare_by)) "cohort" else z$compare_by
  keep <- function(x, used) if (used && !is.null(x) && !identical(x, "")) x
  list(sex = keep(z$sex, by %in% c("sex", "sex_age")),
       age_group = keep(z$age_group, by %in% c("age", "sex_age")),
       weight_status = keep(z$weight_status, by == "weight"))
}

# MFO_zones() with the options of the tab (NULL/NA = not given)
.app_run_zones <- function(res, z) {
  g <- .app_reference_group(z)
  MFO_zones(res,
            HRmax = .app_na_null(z$HRmax),
            predicted_HRmax = isTRUE(z$predicted_HRmax),
            age = .app_na_null(z$age),
            HRmax_formula = z$HRmax_formula,
            population = z$population,
            body_fat = .app_na_null(z$body_fat),
            sex = g$sex, age_group = g$age_group, weight_status = g$weight_status)
}

# The MFO_zones() result as two tables (zones in HR, reference values) and the
# list of differences, with the labels of the interface language
.app_zones_tables <- function(zn, t = function(key) .tr(key, "en")) {
  f <- function(x, d = 1) ifelse(is.na(x), "", formatC(x, format = "f", digits = d))
  zones <- data.frame(
    c(t("tbl.fatmax"), t("tbl.zone_lower"), t("tbl.zone_upper")),
    f(c(zn$FAT_MAX, zn$zone_lower, zn$zone_upper)),
    f(c(zn$FAT_MAX_HR, zn$zone_lower_HR, zn$zone_upper_HR), 0),
    f(c(zn$FAT_MAX_porcHRmax, zn$zone_lower_porcHRmax, zn$zone_upper_porcHRmax)),
    stringsAsFactors = FALSE)
  names(zones) <- c(t("tbl.quantity"), "% VO2max", t("tbl.bpm"), "% HRmax")
  pct <- function(lab) if (is.na(lab)) t("zo.no_percentile") else lab
  reference <- data.frame(
    c("MFO (g/min)", "Fatmax (% VO2max)"),
    c(f(zn$MFO, 3), f(zn$FAT_MAX)),
    c(pct(zn$MFO_percentile_label), pct(zn$Fatmax_percentile_label)),
    stringsAsFactors = FALSE)
  names(reference) <- c(t("tbl.quantity"), t("tbl.value"), t("tbl.percentile"))
  list(zones = zones, reference = reference,
       differences = strsplit(zn$reference_differences, "; ", fixed = TRUE)[[1]])
}

# Labels of MFO_zones() values in the interface language
.app_zones_label <- function(x, t = function(key) .tr(key, "en")) {
  keys <- c("given" = "zo.src.given", "measured" = "zo.src.measured",
            "predicted (Tanaka)" = "zo.src.tanaka", "predicted (Fox)" = "zo.src.fox",
            "inside" = "zo.zone.inside", "partly overlaps" = "zo.zone.partly",
            "outside" = "zo.zone.outside")
  if (is.na(x) || !x %in% names(keys)) return(x)
  t(keys[[x]])
}


.mod_zones_ui <- function(id, lang = "en") {
  ns <- shiny::NS(id)
  t <- function(key) .tr(key, lang)
  named <- function(values, keys) stats::setNames(values, vapply(keys, t, character(1)))
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 340,
      shiny::h6(t("zo.hrmax")),
      shiny::numericInput(ns("HRmax"), t("an.HRmax"), NA, min = 0),
      shiny::checkboxInput(ns("predicted_HRmax"), t("zo.predicted"), FALSE),
      shiny::conditionalPanel(
        sprintf("input['%s']", ns("predicted_HRmax")),
        shiny::numericInput(ns("age"), t("an.age"), NA, min = 0),
        shiny::selectInput(ns("HRmax_formula"), t("zo.formula"),
                           named(c("tanaka", "fox"), c("zo.formula.tanaka",
                                                       "zo.formula.fox")))
      ),
      shiny::helpText(t("zo.hrmax.help")),
      shiny::h6(t("zo.reference")),
      shiny::selectInput(ns("compare_by"), t("zo.compare_by"),
                         named(.app_compare_by, c("zo.by.cohort", "zo.by.sex", "zo.by.age",
                                                   "zo.by.sex_age", "zo.by.weight"))),
      shiny::conditionalPanel(
        sprintf("['sex', 'sex_age'].indexOf(input['%s']) >= 0", ns("compare_by")),
        shiny::selectInput(ns("sex"), t("zo.sex"),
                           named(c("male", "female"), c("zo.sex.male", "zo.sex.female")))
      ),
      shiny::conditionalPanel(
        sprintf("['age', 'sex_age'].indexOf(input['%s']) >= 0", ns("compare_by")),
        shiny::selectInput(ns("age_group"), t("zo.age_group"),
                           named(c("young", "middle-aged"), c("zo.age.young", "zo.age.middle")))
      ),
      shiny::conditionalPanel(
        sprintf("input['%s'] == 'weight'", ns("compare_by")),
        shiny::selectInput(ns("weight_status"), t("zo.weight"),
                           named(c("normal", "overweight", "obese"),
                                 c("zo.weight.normal", "zo.weight.overweight",
                                   "zo.weight.obese")))
      ),
      shiny::helpText(t("zo.groups.help")),
      shiny::h6(t("zo.obesity")),
      shiny::checkboxInput(ns("obesity"), t("zo.obesity.check"), FALSE),
      shiny::conditionalPanel(
        sprintf("input['%s']", ns("obesity")),
        shiny::numericInput(ns("body_fat"), t("zo.body_fat"), NA, min = 0, max = 80)
      ),
      shiny::actionButton(ns("run"), t("zo.run"), class = "btn-primary")
    ),
    shiny::uiOutput(ns("status")),
    bslib::card(bslib::card_header(t("zo.zones")),
                shiny::tableOutput(ns("zones")),
                shiny::textOutput(ns("hrmax_source")),
                shiny::uiOutput(ns("obesity_out"))),
    bslib::card(bslib::card_header(t("zo.percentiles")),
                shiny::textOutput(ns("reference_label")),
                shiny::tableOutput(ns("reference")),
                shiny::h6(t("zo.differences")),
                shiny::uiOutput(ns("differences"))),
    shiny::uiOutput(ns("alerts"))
  )
}


.mod_zones_server <- function(id, results, lang = "en") {
  shiny::moduleServer(id, function(input, output, session) {
    t <- function(key) .tr(key, lang)
    out <- shiny::reactiveVal(NULL)
    log <- shiny::reactiveVal(NULL)

    # HRmax and age of Protocol and analysis as defaults
    shiny::observeEvent(results(), {
      o <- results()$opts
      if (!is.null(o$HRmax) && !is.na(o$HRmax)) shiny::updateNumericInput(session, "HRmax", value = o$HRmax)
      if (!is.null(o$age) && !is.na(o$age)) shiny::updateNumericInput(session, "age", value = o$age)
      out(NULL)
      log(NULL)
    }, ignoreNULL = TRUE)

    shiny::observeEvent(input$run, {
      a <- tryCatch(results(), error = function(e) NULL)
      if (is.null(a)) {
        log(list(error = t("res.none"), warnings = character(0), messages = character(0)))
        out(NULL)
        return()
      }
      z <- list(HRmax = input$HRmax, predicted_HRmax = input$predicted_HRmax, age = input$age,
                HRmax_formula = input$HRmax_formula,
                population = if (isTRUE(input$obesity)) "obesity" else "general",
                body_fat = input$body_fat, compare_by = input$compare_by, sex = input$sex,
                age_group = input$age_group,
                weight_status = input$weight_status)
      z <- c(z[setdiff(names(z), c("sex", "age_group", "weight_status"))],
             .app_reference_group(z))
      r <- .app_capture(.app_run_zones(a$res, z))
      log(r)
      out(if (is.null(r$error)) list(zones = r$value, options = z, log = r, run = a$run))
    })

    tabs <- shiny::reactive({
      shiny::req(out())
      .app_zones_tables(out()$zones, t)
    })
    output$status <- shiny::renderUI({
      if (is.null(out()) && is.null(log())) {
        shiny::div(class = "alert alert-secondary", t("zo.none"))
      }
    })
    output$zones <- shiny::renderTable(tabs()$zones)
    output$hrmax_source <- shiny::renderText({
      zn <- out()$zones
      shiny::req(zn)
      if (is.na(zn$HRmax)) t("zo.no_hrmax") else
        sprintf(t("zo.hrmax.used"), round(zn$HRmax), .app_zones_label(zn$HRmax_source, t))
    })
    output$obesity_out <- shiny::renderUI({
      zn <- out()$zones
      shiny::req(zn, identical(out()$options$population, "obesity"))
      if (is.na(zn$obesity_range_lower)) return(shiny::p(t("zo.obesity.none")))
      shiny::p(sprintf(t("zo.obesity.result"), zn$obesity_range_lower, zn$obesity_range_upper,
                       if (is.na(zn$obesity_zone)) "NA" else .app_zones_label(zn$obesity_zone, t)))
    })
    output$reference_label <- shiny::renderText({
      zn <- out()$zones
      shiny::req(zn)
      sprintf("%s (n = %d)", zn$reference, zn$reference_n)
    })
    output$reference <- shiny::renderTable(tabs()$reference)
    output$differences <- shiny::renderUI(shiny::tags$ul(lapply(tabs()$differences, shiny::tags$li)))
    output$alerts <- shiny::renderUI({
      l <- log()
      if (is.null(l)) return(NULL)
      .app_alerts(l)
    })

    out
  })
}
