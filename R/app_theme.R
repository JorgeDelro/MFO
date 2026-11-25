# Theme of the app (#24; Jorge, 2026-11-24): bslib Bootstrap 5 with the
# minty bootswatch and the colours below, Inter as font, and the same
# colours in the ggplot figures of the app (FAT primary, CHO info, the
# interval band warning). The theme is built only inside run_app().

.app_colours <- c(primary = "#6C63FF", secondary = "#A8A4CE", success = "#57CC99",
                  info = "#80B3FF", warning = "#FFD166", danger = "#EF476F")

.app_theme <- function(font = TRUE) {
  family <- if (font) {
    bslib::font_collection(bslib::font_google("Inter", local = TRUE), "system-ui",
                           "sans-serif")
  } else {
    bslib::font_collection("system-ui", "sans-serif")
  }
  col <- as.list(.app_colours)
  bslib::bs_theme(
    version = 5, bootswatch = "minty",
    primary = col$primary, secondary = col$secondary, success = col$success,
    info = col$info, warning = col$warning, danger = col$danger,
    base_font = family, heading_font = family,
    "navbar-bg" = col$primary, "navbar-dark-color" = "#FFFFFF",
    "navbar-dark-hover-color" = "#E0DDFF", "navbar-dark-active-color" = "#FFFFFF",
    "navbar-dark-brand-color" = "#FFFFFF")
}

# The theme with Inter, which is downloaded once into the sass cache and then
# served locally, so the app works without network. If it cannot be
# downloaded (first run offline), the system font is used.
.app_theme_offline_safe <- function() {
  tryCatch({
    theme <- .app_theme()
    bslib::bs_theme_dependencies(theme)
    theme
  }, error = function(e) {
    message("The font Inter could not be downloaded; the app uses the system font.")
    .app_theme(font = FALSE)
  })
}


# The colours of the app in a ggplot of the package. Layers are ggproto
# objects (environments), so changed layers are copies: the plot stored in
# the result of MFO() is not modified.
.app_style_plot <- function(p) {
  col <- .app_colours
  recolour <- function(layer) {
    a <- layer$aes_params
    new <- a
    if (identical(a$fill, "darkorange")) new$fill <- col[["primary"]]
    if (identical(a$fill, "grey40")) {
      new$fill <- col[["warning"]]
      new$alpha <- 0.35
    }
    if (identical(a$colour, "red")) new$colour <- col[["danger"]]
    if (identical(new, a)) return(layer)
    copy <- ggplot2::ggproto(NULL, layer)
    copy$aes_params <- new
    copy
  }
  p$layers <- lapply(p$layers, recolour)
  # MFO() plot: FAT and CHO; MFO_kinetics() plot: the fitted SIN
  values <- c(FAT = col[["primary"]], CHO = col[["info"]], "Fitted SIN" = col[["primary"]],
              "Basic SIN" = "grey60")
  suppressMessages(p + ggplot2::scale_colour_manual(values = values))
}
