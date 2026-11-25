# Theme of the app (#24; Jorge, 2026-11-24): minty with the colours of the
# app, and the same colours in the plots

test_that("the theme has the colours and font of the app", {
  skip_if_not_installed("bslib")
  th <- .app_theme(font = FALSE)
  expect_true(bslib::is_bs_theme(th))
  expect_equal(bslib::theme_bootswatch(th), "minty")
  v <- bslib::bs_get_variables(th, c("primary", "info", "warning", "navbar-bg"))
  expect_equal(unname(v[c("primary", "info", "warning")]),
               unname(.app_colours[c("primary", "info", "warning")]))
  expect_equal(unname(v["navbar-bg"]), "#6C63FF")
})

test_that("the plots of the app take its colours without changing the result", {
  a <- .app_test_analysis()
  a <- suppressWarnings(suppressMessages(.app_run_analysis(a$cart, a$opts, ci = "parametric",
                                                           B = 50, seed = 1)))
  p <- a$res$MFO_plot
  fills_before <- vapply(p$layers, function(l) {
    f <- l$aes_params$fill
    if (is.null(f)) NA_character_ else f
  }, character(1))
  s <- .app_style_plot(p)
  b <- ggplot2::ggplot_build(s)
  colours <- unique(unlist(lapply(b$data, function(d) d$colour)))
  fills <- unique(unlist(lapply(b$data, function(d) d$fill)))
  expect_true(all(c(.app_colours[["primary"]], .app_colours[["info"]]) %in% colours))
  expect_true(.app_colours[["warning"]] %in% fills)
  expect_false("darkorange" %in% c(colours, fills))
  # the plot of the result is not modified
  after <- vapply(a$res$MFO_plot$layers, function(l) {
    f <- l$aes_params$fill
    if (is.null(f)) NA_character_ else f
  }, character(1))
  expect_identical(after, fills_before)
  expect_true("grey40" %in% after)
  k <- ggplot2::ggplot_build(.app_style_plot(a$kin$MFO_kinetics_plot))
  expect_true(.app_colours[["primary"]] %in% unlist(lapply(k$data, function(d) d$colour)))
})

test_that("without network the theme falls back to the system font", {
  skip_if_not_installed("bslib")
  local_mocked_bindings(bs_theme_dependencies = function(...) stop("no network"),
                        .package = "bslib")
  expect_message(th <- .app_theme_offline_safe(), "Inter could not be downloaded")
  expect_true(bslib::is_bs_theme(th))
  expect_equal(bslib::theme_bootswatch(th), "minty")
})

test_that("the plot theme follows the mode, as in the ACWR app", {
  light <- .app_plot_theme(FALSE)
  dark <- .app_plot_theme(TRUE)
  expect_equal(c(light$text, light$grid, light$bg), c("#2C3E50", "#E0E0E0", "transparent"))
  expect_equal(c(dark$text, dark$grid, dark$bg), c("#E0E0E0", "#444444", "transparent"))
  a <- .app_test_analysis()
  p <- .app_style_plot(a$kin$MFO_kinetics_plot, dark)
  th <- ggplot2::theme_get()
  built <- ggplot2::ggplot_build(p)
  expect_equal(p$theme$axis.text$colour, "#E0E0E0")
  expect_equal(p$theme$panel.grid.major$colour, "#444444")
  expect_equal(p$theme$plot.background$fill, "transparent")
  # black observed points would vanish on a dark background
  cols <- unique(unlist(lapply(built$data, function(d) d$colour)))
  expect_false("black" %in% cols)
  expect_true(.app_colours[["primary"]] %in% cols)
  # the default (reports and downloaded figures) is light
  q <- .app_style_plot(a$kin$MFO_kinetics_plot)
  expect_equal(q$theme$axis.text$colour, "#2C3E50")
  expect_true("black" %in% unlist(lapply(ggplot2::ggplot_build(q)$data,
                                         function(d) d$colour)))
  # the Bland-Altman plot takes the palette of the app
  g <- data.frame(participant = rep(paste0("P", 1:4), 2), session = rep(c("1", "2"), each = 4),
                  MFO_g_min = c(0.3, 0.4, 0.5, 0.35, 0.32, 0.41, 0.47, 0.37))
  ba <- .app_style_plot(plot(suppressWarnings(MFO_reliability(g))), dark)
  expect_true(.app_colours[["primary"]] %in%
                unlist(lapply(ggplot2::ggplot_build(ba)$data, function(d) d$colour)))
})

test_that("the app has the dark-mode switch and draws plots in that mode", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  expect_match(as.character(.app_ui(list(QUERY_STRING = ""))), "dark_mode", fixed = TRUE)
  a <- .app_test_analysis()
  analysis <- shiny::reactiveVal(a)
  group <- shiny::reactiveVal(NULL)
  dark <- shiny::reactiveVal(TRUE)
  shiny::testServer(.mod_results_server,
                    args = list(analysis = analysis, group = group,
                                plot_theme = function() .app_plot_theme(dark())), {
    expect_match(output$plot$src, "^data:image/png")
    session$setInputs(fig_curve_format = "png", fig_curve_width = 100,
                      fig_curve_height = 60)
    expect_true(file.size(output$fig_curve_download) > 1000)
  })
})

test_that("the theme keeps the blank legend title of the SIN plot", {
  a <- .app_test_analysis()
  p <- .app_style_plot(a$kin$MFO_kinetics_plot, .app_plot_theme(TRUE))
  expect_s3_class(p$theme$legend.title, "element_blank")
})
