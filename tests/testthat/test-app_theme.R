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
