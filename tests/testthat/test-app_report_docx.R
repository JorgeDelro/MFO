# Word report (#24; Jorge, 2026-11-25): the same sections as the HTML report,
# written with officer; the file is read back to check it

.docx_text <- function(f) officer::docx_summary(officer::read_docx(f))

test_that("the Word report has the sections, tables and plots of the HTML report", {
  skip_if_not_installed("officer")
  a <- .app_test_analysis()
  a$data <- list(cuts = NULL)
  f <- withr::local_tempfile(fileext = ".docx")
  .app_report_docx(f, a, id = "P01")
  s <- .docx_text(f)
  headings <- s$text[s$style_name %in% "heading 1"]
  html <- .app_report_content(a, id = "P01")
  expect_equal(headings, vapply(Filter(function(b) b$type == "h2", html$blocks),
                                `[[`, character(1), "text"))
  expect_true(any(s$text == "MFO report: P01"))
  # tables: results, steps of the fit and SIN parameters
  n_tables <- sum(vapply(html$blocks, function(b) b$type == "table", logical(1)))
  xml <- paste(readLines(unz(f, "word/document.xml"), warn = FALSE), collapse = "")
  expect_equal(lengths(regmatches(xml, gregexpr("<w:tbl>", xml, fixed = TRUE))), n_tables)
  cells <- s$text[s$content_type == "table cell"]
  expect_true("%VO₂max" %in% cells)
  expect_true(format(round(a$res$MFO, 3)) %in% cells)
  # the methods paragraph and the references are plain, editable paragraphs
  methods <- .app_methods(a)
  expect_true(methods$text %in% s$text)
  expect_true(paste0("1. ", methods$references[[1]]) %in% s$text)
  # plots: PNG at 300 dpi, the width of the text
  media <- grep("^word/media/.*\\.png$", utils::unzip(f, list = TRUE)$Name, value = TRUE)
  expect_length(media, 2)
  dir <- withr::local_tempdir()
  utils::unzip(f, files = media[1], exdir = dir)
  con <- file(file.path(dir, media[1]), "rb")
  hdr <- readBin(con, "raw", 24)
  close(con)
  px <- sum(as.integer(hdr[17:20]) * 256^(3:0))
  expect_equal(px, round(.app_docx_plot_mm[["width"]] / 25.4 * 300), tolerance = 1)
})

test_that("the Word report includes zones and intervals when computed", {
  skip_if_not_installed("officer")
  a <- .app_test_analysis(HRmax = 190)
  a$data <- list(cuts = c(basal_end = 900, MFO_end = 2340), source = "example")
  zn <- suppressMessages(.app_run_zones(a$res, list(HRmax = NA, predicted_HRmax = FALSE,
                                                    age = NA, HRmax_formula = "tanaka",
                                                    population = "general",
                                                    compare_by = "sex", sex = "male")))
  f <- withr::local_tempfile(fileext = ".docx")
  .app_report_docx(f, a, zones = list(zones = zn, options = list(population = "general")))
  s <- .docx_text(f)
  expect_true(all(c("Heart-rate zones", "Reference values") %in%
                    s$text[s$style_name %in% "heading 1"]))
  expect_true(any(grepl("^Phase cuts: basal_end = 900", s$text)))
})

test_that("the Report tab downloads the Word report", {
  skip_if_not_installed("officer")
  skip_if_not_installed("shiny")
  a <- .app_test_analysis()
  a$data <- list(cuts = NULL)
  results <- shiny::reactiveVal(a)
  zones <- shiny::reactiveVal(NULL)
  group <- shiny::reactiveVal(NULL)
  expect_match(as.character(.mod_report_ui("report")), "Download report (Word)", fixed = TRUE)
  shiny::testServer(.mod_report_server, args = list(results = results, zones = zones,
                                                    group = group), {
    session$setInputs(ID = "P01")
    f <- output$report_docx
    expect_match(f, "\\.docx$")
    expect_true("MFO report: P01" %in% .docx_text(f)$text)
  })
})
