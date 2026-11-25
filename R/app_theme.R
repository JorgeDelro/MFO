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


# Secondary buttons (Jorge, 2026-11-25): outline in primary, text and border
# #6C63FF, instead of the filled secondary colour of the theme (light purple
# with white text, low contrast). The main action of each tab stays a filled
# btn-primary. Shiny gives buttons btn-default, which bslib draws in
# secondary, so it is replaced
.app_outline <- function(button, small = FALSE) {
  q <- htmltools::tagQuery(button)
  q$removeClass("btn-default")$addClass("btn-outline-primary")
  if (small) q$addClass("btn-sm")
  q$allTags()
}

# Plot theme of the app, as in the ACWR app (Jorge, 2026-11-25): text and
# grid colours for light or dark mode and a transparent background, so the
# plot takes the background of its card. muted is for black and grey lines
# of the package plots, which would vanish on a dark background.
.app_plot_theme <- function(is_dark = FALSE) {
  list(is_dark = is_dark, bg = "transparent",
       text = if (is_dark) "#E0E0E0" else "#2C3E50",
       grid = if (is_dark) "#444444" else "#E0E0E0",
       muted = if (is_dark) "#B0B0B0" else NA_character_)
}

# The colours of the app in a ggplot of the package (series in the palette
# of the app) and the plot theme. Reports and downloaded figures use the
# light theme (the default); the screen follows the dark-mode switch.
# Layers are ggproto objects (environments), so changed layers are copies:
# the plot stored in the result of MFO() is not modified.
.app_style_plot <- function(p, th = .app_plot_theme(FALSE)) {
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
    if (identical(a$colour, "darkorange")) new$colour <- col[["primary"]]
    if (identical(a$colour, "steelblue")) new$colour <- col[["info"]]
    if (!is.na(th$muted) && (identical(a$colour, "black") || identical(a$colour, "grey40"))) {
      new$colour <- th$muted
    }
    if (identical(new, a)) return(layer)
    copy <- ggplot2::ggproto(NULL, layer)
    copy$aes_params <- new
    copy
  }
  p$layers <- lapply(p$layers, recolour)
  # MFO() plot: FAT and CHO; MFO_kinetics() plot: the fitted SIN
  series <- c("FAT", "CHO", "Fitted SIN", "Basic SIN")
  has_series <- any(vapply(p$layers, function(l) {
    d <- if (is.data.frame(l$data)) l$data else p$data
    is.data.frame(d) && any(unlist(lapply(d, function(x) {
      (is.character(x) || is.factor(x)) && any(as.character(x) %in% series)
    })))
  }, logical(1)))
  if (has_series) {
    values <- c(FAT = col[["primary"]], CHO = col[["info"]], "Fitted SIN" = col[["primary"]],
                "Basic SIN" = "grey60")
    p <- suppressMessages(p + ggplot2::scale_colour_manual(values = values))
  }
  .app_theme_plot(p, th)
}

# Text, grid and background of a plot for the light or dark mode
.app_theme_plot <- function(p, th = .app_plot_theme(FALSE)) {
  # legend.title is left to the plot (the SIN plot has none); it inherits text
  el_text <- ggplot2::element_text(colour = th$text)
  blank_rect <- ggplot2::element_rect(fill = th$bg, colour = NA)
  p + ggplot2::theme(
    text = el_text, axis.text = el_text, axis.title = el_text,
    plot.title = el_text, plot.subtitle = el_text,
    plot.caption = ggplot2::element_text(colour = th$text, hjust = 0),
    legend.text = el_text, strip.text = el_text,
    axis.ticks = ggplot2::element_line(colour = th$grid),
    panel.grid.major = ggplot2::element_line(colour = th$grid),
    panel.grid.minor = ggplot2::element_line(colour = th$grid, linewidth = 0.25),
    panel.border = ggplot2::element_rect(fill = NA, colour = th$grid),
    plot.background = blank_rect, panel.background = blank_rect,
    legend.background = blank_rect, legend.key = blank_rect,
    strip.background = ggplot2::element_rect(fill = th$bg, colour = th$grid))
}
