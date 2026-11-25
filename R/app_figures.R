# Figure downloads of the app (#24; Jorge, 2026-11-25): every plot can be
# saved as PNG at 300 dpi, PDF or SVG, 180 mm wide by default (a full-width
# journal figure), with width and height set by the user. Only grDevices is
# used: cairo_pdf, quartz and svg print the subscript of VO₂, which pdf()
# cannot.

.app_figure_formats <- c("PNG (300 dpi)" = "png", "PDF" = "pdf", "SVG" = "svg")

# Whether cairo devices really work: capabilities("cairo") can be TRUE while
# the cairo module of R fails to load (macOS without XQuartz, as on the
# GitHub runners), so a device is opened once to see
.app_cairo_ok <- local({
  ok <- NULL
  function() {
    if (is.null(ok)) {
      ok <<- isTRUE(capabilities("cairo")) && tryCatch({
        f <- tempfile(fileext = ".pdf")
        on.exit(unlink(f))
        withCallingHandlers(grDevices::cairo_pdf(f), warning = function(w) stop(w))
        grDevices::dev.off()
        TRUE
      }, error = function(e) FALSE)
    }
    ok
  }
})

# Save a ggplot to file; width and height in mm. PNG uses the default bitmap
# device of the platform; PDF uses cairo_pdf, else quartz on macOS (both
# print VO₂), else pdf(); SVG needs cairo
.app_save_figure <- function(p, file, format = c("png", "pdf", "svg"), width_mm = 180,
                             height_mm = 120) {
  format <- match.arg(format)
  if (!is.numeric(width_mm) || !is.numeric(height_mm) || is.na(width_mm) ||
      is.na(height_mm) || width_mm <= 0 || height_mm <= 0) {
    stop("Give a width and a height in mm greater than 0.", call. = FALSE)
  }
  w <- width_mm / 25.4
  h <- height_mm / 25.4
  cairo <- .app_cairo_ok()
  quartz <- identical(Sys.info()[["sysname"]], "Darwin") && isTRUE(capabilities("aqua"))
  switch(format,
    png = grDevices::png(file, width = w, height = h, units = "in", res = 300),
    pdf = if (cairo) {
      grDevices::cairo_pdf(file, width = w, height = h)
    } else if (quartz) {
      tryCatch(grDevices::quartz(type = "pdf", file = file, width = w, height = h),
               error = function(e) grDevices::pdf(file, width = w, height = h))
    } else {
      grDevices::pdf(file, width = w, height = h)
    },
    svg = if (cairo) grDevices::svg(file, width = w, height = h) else
      stop("SVG needs R with cairo support; use PNG or PDF.", call. = FALSE))
  on.exit(grDevices::dev.off())
  print(p)
  invisible(file)
}

# Controls right under a plot: format, width, height and the download button.
# id is the prefix of the inputs (<id>_format, <id>_width, <id>_height) and
# of the download output (<id>_download)
.app_figure_controls <- function(ns, id, height_mm = 120) {
  t <- function(key) .tr(key)
  shiny::div(
    class = "mt-2",
    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      shiny::selectInput(ns(paste0(id, "_format")), t("fig.format"), .app_figure_formats),
      shiny::numericInput(ns(paste0(id, "_width")), t("fig.width"), 180, min = 20,
                          max = 500, step = 5),
      shiny::numericInput(ns(paste0(id, "_height")), t("fig.height"), height_mm, min = 20,
                          max = 500, step = 5),
      shiny::div(class = "d-flex align-items-end h-100 pb-3",
                 .app_outline(shiny::downloadButton(ns(paste0(id, "_download")),
                                                    t("fig.download"))))
    )
  )
}

# The download handler of those controls; plot() returns the ggplot shown
.app_figure_download <- function(input, output, id, plot, name = id) {
  output[[paste0(id, "_download")]] <- shiny::downloadHandler(
    filename = function() paste0("MFO_", name, ".", input[[paste0(id, "_format")]]),
    content = function(file) {
      .app_save_figure(plot(), file, input[[paste0(id, "_format")]],
                       input[[paste0(id, "_width")]], input[[paste0(id, "_height")]])
    })
}
