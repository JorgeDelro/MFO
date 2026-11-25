# Individual report as DOCX (#24; Jorge, 2026-11-25): the blocks of
# .app_report_content(), the same as the HTML report, written with officer
# (in Suggests; no flextable, no pandoc). Plots are PNG at 300 dpi in the
# light theme; the methods paragraph and the references are plain
# paragraphs, so they can be edited in Word.

# Width of the plots in the document (mm): the text width of an A4 or
# Letter page with 25-mm margins
.app_docx_plot_mm <- c(width = 160, height = 100)

.app_report_docx <- function(file, a, zones = NULL, id = "", generated = Sys.time()) {
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("The Word report needs the package officer: install.packages(\"officer\").",
         call. = FALSE)
  }
  r <- .app_report_content(a, zones, id, generated)
  doc <- officer::read_docx()
  par <- function(text, style = "Normal") {
    doc <<- officer::body_add_par(doc, text, style = style)
  }
  prefix <- c(warning = "Warning: ", info = "Note: ", ok = "")
  # the title is not a numbered heading; the sections are (1., 2., ...)
  doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(
    r$title, officer::fp_text(bold = TRUE, font.size = 18))))
  for (b in r$blocks) {
    switch(b$type,
      h2 = par(b$text, "heading 1"),
      p = par(b$text),
      table = {
        doc <- officer::body_add_table(doc, .app_format_table(b$data),
                                       style = "table_template")
        par("")
      },
      plot = {
        png <- tempfile(fileext = ".png")
        on.exit(unlink(png), add = TRUE)
        .app_save_figure(b$plot, png, "png", .app_docx_plot_mm[["width"]],
                         .app_docx_plot_mm[["height"]])
        doc <- officer::body_add_img(doc, png, width = .app_docx_plot_mm[["width"]] / 25.4,
                                     height = .app_docx_plot_mm[["height"]] / 25.4)
        par(b$alt, "Image Caption")
      },
      alert = par(paste0(prefix[[b$level]], b$text)),
      list = for (x in b$items) par(paste0("• ", x)),
      methods = par(b$text),
      references = for (i in seq_along(b$items)) par(paste0(i, ". ", b$items[[i]])),
      footer = {
        par("")
        for (x in b$items) par(x)
      })
  }
  print(doc, target = file)
  invisible(file)
}
