# Figure downloads (#24; Jorge, 2026-11-25): PNG at 300 dpi, PDF and SVG,
# 180 mm wide by default

test_that("figures are saved at the size and resolution asked", {
  a <- .app_test_analysis()
  p <- .app_style_plot(a$res$MFO_plot)
  png_file <- withr::local_tempfile(fileext = ".png")
  .app_save_figure(p, png_file, "png", 180, 120)
  # PNG header: width and height in pixels at bytes 17-24
  con <- file(png_file, "rb")
  hdr <- readBin(con, "raw", 24)
  close(con)
  px <- function(b) sum(as.integer(b) * 256^(3:0))
  expect_equal(px(hdr[17:20]), round(180 / 25.4 * 300), tolerance = 1)
  expect_equal(px(hdr[21:24]), round(120 / 25.4 * 300), tolerance = 1)
  pdf_file <- withr::local_tempfile(fileext = ".pdf")
  .app_save_figure(p, pdf_file, "pdf", 90, 60)
  expect_identical(readChar(pdf_file, 4), "%PDF")
  skip_if_not(isTRUE(capabilities("cairo")))
  svg_file <- withr::local_tempfile(fileext = ".svg")
  .app_save_figure(p, svg_file, "svg")
  # svg() writes points: 180 mm = 510.24 pt
  expect_match(paste(readLines(svg_file, n = 3), collapse = ""), "width=\"510")
  expect_error(.app_save_figure(p, png_file, "png", NA, 100), "width and a height")
})

test_that("each plot of the app has its download", {
  skip_if_not_installed("shiny")
  a <- .app_test_analysis()
  analysis <- shiny::reactiveVal(a)
  group <- shiny::reactiveVal(NULL)
  shiny::testServer(.mod_results_server, args = list(analysis = analysis, group = group), {
    session$setInputs(fig_curve_format = "png", fig_curve_width = 100,
                      fig_curve_height = 60, fig_sin_format = "pdf", fig_sin_width = 180,
                      fig_sin_height = 110)
    f <- output$fig_curve_download
    expect_match(f, "\\.png$")
    expect_true(file.size(f) > 1000)
    expect_identical(readChar(output$fig_sin_download, 4), "%PDF")
  })
  shiny::testServer(.mod_data_server, {
    session$setInputs(source = "example", assign = "phases", read = 1,
                      fig_recording_format = "png", fig_recording_width = 180,
                      fig_recording_height = 90)
    expect_true(file.size(output$fig_recording_download) > 1000)
  })
})
