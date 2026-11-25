# Individual HTML report of the app (#24, SPEC 10 point 4; contents decided by
# Jorge, 2026-11-19): curve and results table, intervals if computed, SIN
# kinetics, HR zones, percentile with its group and the differences from the
# reference, quality alerts, methods paragraph and references, and a footer
# with the package version and commit, the analysis date, the phase cuts and
# the seed. Built with htmltools (a dependency of shiny) and the plots
# embedded as images, so it needs no pandoc and is one self-contained file.

.app_report_css <- "
body { font-family: system-ui, -apple-system, 'Segoe UI', sans-serif; max-width: 900px;
       margin: 2em auto; padding: 0 1em; color: #222; line-height: 1.45; }
h1 { font-size: 1.6em; } h2 { font-size: 1.25em; margin-top: 1.6em;
     border-bottom: 1px solid #ddd; padding-bottom: .2em; }
table { border-collapse: collapse; margin: .6em 0; }
th, td { border: 1px solid #ddd; padding: .25em .6em; text-align: left; }
th { background: #f4f4f4; }
.alert { padding: .5em .8em; border-radius: 4px; margin: .3em 0; }
.warning { background: #fff3cd; } .info { background: #e7f1ff; } .ok { background: #e6f4ea; }
.methods { background: #fafafa; border-left: 3px solid #888; padding: .6em 1em; }
footer { margin-top: 2em; font-size: .85em; color: #555; border-top: 1px solid #ddd;
         padding-top: .6em; }
img { max-width: 100%; height: auto; }
.print-button { float: right; padding: .4em 1em; border: 0; border-radius: 4px;
                background: #6C63FF; color: #fff; font: inherit; cursor: pointer; }
@media print {
  .print-button { display: none; }
  body { margin: 0; max-width: none; }
  h2 { break-after: avoid; }
  table, img, .alert, .methods { break-inside: avoid; }
}
@page { size: A4; margin: 15mm; }
"

# A data frame as text, as both reports show it
.app_format_table <- function(df, digits = 3) {
  fmt <- function(x) {
    if (is.numeric(x)) ifelse(is.na(x), "", formatC(x, format = "fg", digits = digits))
    else ifelse(is.na(x), "", as.character(x))
  }
  out <- as.data.frame(lapply(df, fmt), stringsAsFactors = FALSE, check.names = FALSE)
  names(out) <- names(df)
  out
}

# A data frame as an HTML table
.app_html_table <- function(df, digits = 3) {
  df <- .app_format_table(df, digits)
  fmt <- function(x) x
  htmltools::tags$table(
    htmltools::tags$thead(htmltools::tags$tr(lapply(names(df), htmltools::tags$th))),
    htmltools::tags$tbody(lapply(seq_len(nrow(df)), function(i) {
      htmltools::tags$tr(lapply(df[i, , drop = FALSE], function(v) htmltools::tags$td(fmt(v))))
    })))
}

.app_plot_tag <- function(p, alt) {
  htmltools::plotTag(print(p), alt = alt, width = 760, height = 460)
}


# The contents of the report, the same for HTML and DOCX: a title and a list
# of blocks (h2, p, table, plot, alert, list, methods, references, footer).
# a: current analysis (with ci_settings if intervals were computed); zones:
# result of the Zones tab for this run, or NULL; id: participant code (""
# if none)
.app_report_content <- function(a, zones = NULL, id = "", generated = Sys.time()) {
  res <- a$res
  kin <- a$kin
  ci <- a$ci_settings
  methods <- .app_methods(a, ci = ci, zones = zones)
  title <- if (nzchar(id)) paste("MFO report:", id) else "MFO report"
  h2 <- function(x) list(type = "h2", text = x)
  p <- function(x) list(type = "p", text = x)
  table <- function(x) list(type = "table", data = x)
  plot <- function(x, alt) list(type = "plot", plot = .app_style_plot(x), alt = alt)
  alert <- function(x, level) list(type = "alert", text = x, level = level)

  results <- .app_results_names(.app_results_table(res))
  if (is.null(ci)) results <- results[setdiff(names(results), c("Lower", "Upper"))]

  # conditions worded with the English templates of the app (as on screen)
  words <- function(l, type) {
    if (is.null(l)) return(character(0))
    if (is.null(l$conditions)) return(l[[paste0(type, "s")]])
    unname(vapply(Filter(function(x) inherits(x, type), l$conditions),
                  .app_condition_text, character(1)))
  }
  alerts <- c(words(a$log, "warning"), words(a$ci_log, "warning"))
  notes <- words(a$log, "message")

  zones_part <- NULL
  zn <- zones$zones
  if (!is.null(zn)) {
    zt <- .app_zones_tables(zn)
    hr_line <- if (is.na(zn$HRmax)) {
      "No HRmax given, measured or predicted: the zone is given in bpm only."
    } else {
      sprintf("HRmax %d bpm (%s).", as.integer(round(zn$HRmax)), .app_zones_label(zn$HRmax_source))
    }
    obesity <- NULL
    if (identical(zones$options$population, "obesity")) {
      obesity <- p(if (is.na(zn$obesity_range_lower)) {
        "Obesity range: none (the source defines more than and less than 35 % body fat, not exactly 35.0 %)."
      } else {
        sprintf("Obesity range (Ch\u00e1vez-Guevara et al. 2023): %d-%d %% HRpeak; individual zone: %s.",
                as.integer(zn$obesity_range_lower), as.integer(zn$obesity_range_upper),
                .app_zones_label(zn$obesity_zone))
      })
    }
    zones_part <- list(
      h2("Heart-rate zones"), table(zt$zones), p(hr_line), obesity,
      h2("Reference values"), p(sprintf("%s (n = %d).", zn$reference, zn$reference_n)),
      table(zt$reference), p("How this test differs from the reference test:"),
      list(type = "list", items = zt$differences))
  }

  sin_part <- NULL
  if (isTRUE(kin$convergence)) {
    sin_part <- list(h2("SIN kinetics"),
                     plot(kin$MFO_kinetics_plot, "SIN kinetics of fat oxidation"),
                     table(.app_sin_table(kin)))
  }

  commit <- .app_commit()
  data_line <- paste0("Data: ", switch(if (is.null(a$data$source)) "" else a$data$source,
                                       example = "example data of the package",
                                       generic = "csv/xlsx file", device = "analyser export",
                                       "not recorded"),
                      "; records every ", format(a$cart$step_time), " s.")
  cuts <- .app_cuts_text(a$data$cuts)
  footer <- c(
    sprintf("MFO %s%s. Analysis: %s. Report: %s.", utils::packageVersion("MFO"),
            if (is.na(commit)) " (commit not recorded: not installed from GitHub)"
            else paste0(", commit ", commit),
            format(a$date, "%Y-%m-%d %H:%M"), format(generated, "%Y-%m-%d %H:%M")),
    data_line,
    paste0("Phase cuts: ", if (nzchar(cuts)) paste0(cuts, " s") else
      "none (one table per phase)."),
    if (!is.null(ci)) sprintf("Intervals: %s, B = %d, level %s, seed %s.", ci$type,
                              as.integer(ci$B), format(ci$conf),
                              if (is.null(ci$seed) || is.na(ci$seed)) "none"
                              else format(ci$seed)),
    "This report was produced by MFO::run_app(), which runs on the user's computer.")

  blocks <- c(
    list(h2("Fat oxidation curve"),
         plot(res$MFO_plot, "Fat and carbohydrate oxidation against % VO\u2082max"),
         h2("Results"), table(results),
         h2("Steps of the fit"), table(.app_step_table(res))),
    sin_part,
    zones_part,
    list(h2("Quality alerts")),
    if (length(alerts) == 0) list(alert("No warnings.", "ok")) else
      lapply(alerts, alert, level = "warning"),
    lapply(notes, alert, level = "info"),
    list(h2("Methods"), list(type = "methods", text = methods$text),
         h2("References"), list(type = "references", items = methods$references),
         list(type = "footer", items = footer)))
  list(title = title, blocks = Filter(Negate(is.null), blocks))
}


# The report as an HTML page (arguments of .app_report_content())
.app_report <- function(a, zones = NULL, id = "", generated = Sys.time()) {
  tags <- htmltools::tags
  r <- .app_report_content(a, zones, id, generated)
  block <- function(b) {
    switch(b$type,
           h2 = tags$h2(b$text),
           p = tags$p(b$text),
           table = .app_html_table(b$data),
           plot = .app_plot_tag(b$plot, b$alt),
           alert = tags$div(class = paste("alert", b$level), b$text),
           list = tags$ul(lapply(b$items, tags$li)),
           methods = tags$p(class = "methods", b$text),
           references = tags$ol(lapply(b$items, tags$li)),
           footer = tags$footer(lapply(b$items, tags$p)))
  }
  body <- tags$body(
    # opens the print dialog of the browser, where "Save as PDF" is a printer
    tags$button(class = "print-button", type = "button", onclick = "window.print()",
                "Save as PDF"),
    tags$h1(r$title),
    lapply(r$blocks, block))
  # the head is written as text: htmltools moves <head> tags out of a page
  paste0("<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n<title>",
         htmltools::htmlEscape(r$title), "</title>\n<style>", .app_report_css,
         "</style>\n</head>\n", as.character(body), "\n</html>\n")
}


# Write the report to a file
.app_write_report <- function(file, ...) {
  page <- .app_report(...)
  con <- file(file, open = "w", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(page, con)
  invisible(file)
}
