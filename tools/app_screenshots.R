# Screenshots of the app for the README and vignette("app") (#24).
# Needs chromote and Chrome. Run from the package root:
#   Rscript tools/app_screenshots.R
# It starts the app on port 8768, loads the example data, runs the analysis
# and the zones, and saves the images.

port <- 8768
app <- callr::r_bg(function(port) {
  devtools::load_all(quiet = TRUE)
  run_app(port = port, launch.browser = FALSE)
}, args = list(port = port))
on.exit(app$kill(), add = TRUE)
Sys.sleep(8)

b <- chromote::ChromoteSession$new(width = 1400, height = 1000)
b$Page$navigate(sprintf("http://127.0.0.1:%d/", port))
Sys.sleep(4)
js <- function(x, wait = 2) {
  b$Runtime$evaluate(x)
  Sys.sleep(wait)
}
tab <- function(name) js(sprintf("document.querySelector('a[data-value=\"%s\"]').click()", name))
shot <- function(file, height = 1000) {
  b$screenshot(file, cliprect = c(0, 0, 1400, height), scale = 1)
}

js("document.getElementById('data-read').click()", 3)
shot("vignettes/figures/app-data.png", 900)
tab("Protocol and analysis")
js("document.getElementById('analysis-run').click()", 8)
tab("Results")
js("window.scrollTo(0, 0)", 3)
shot("man/figures/README-app-results.png")
file.copy("man/figures/README-app-results.png", "vignettes/figures/app-results.png",
          overwrite = TRUE)
tab("Zones and references")
js("document.getElementById('zones-sex').selectize.setValue('male')", 1)
js("document.getElementById('zones-run').click()", 4)
shot("vignettes/figures/app-zones.png", 900)
b$close()
