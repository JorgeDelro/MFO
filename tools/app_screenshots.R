# Screenshots of the app for the README, vignette("app") and the manual
# (vignettes/articles/app-manual.Rmd) (#24). Needs chromote and Chrome.
# Run from the package root:
#   Rscript tools/app_screenshots.R
# It starts the app on port 8768, loads the example data, runs the analysis,
# the zones, adds the participant to the group table, runs the reliability
# on a simulated group table, and saves every tab.

port <- 8768
app <- callr::r_bg(function(port) {
  devtools::load_all(quiet = TRUE)
  run_app(port = port, launch.browser = FALSE)
}, args = list(port = port))
on.exit(app$kill(), add = TRUE)
for (i in 1:90) {
  Sys.sleep(1)
  up <- tryCatch({
    readLines(sprintf("http://127.0.0.1:%d/", port), warn = FALSE)
    TRUE
  }, error = function(e) FALSE)
  if (up) break
}

# A group table with two sessions of 12 simulated participants
devtools::load_all(quiet = TRUE)
sim <- simulate_MFO_sessions(n_participants = 12, n_sessions = 2,
                             population = list(VO2max = c(3000, 500)), seed = 1)
group <- do.call(rbind, lapply(seq_len(nrow(sim$truth)), function(i) {
  tr <- sim$truth[i, ]
  tabs <- sim$data[[tr$id]][[tr$session]]
  r <- suppressWarnings(suppressMessages(
    MFO(step_time = 20, db_MFO = tabs$db_MFO, db_basal = tabs$db_basal,
        db_graded = tabs$db_graded, cv_var = "RER", author = "Frayn",
        energy_equivalents = "classic", age = 30)))
  data.frame(ID = tr$id, session = tr$session, MFO_g_min = r$MFO,
             FAT_MAX_perVO2max = r$FAT_MAX)
}))
group_file <- file.path(tempdir(), "MFO_group.csv")
utils::write.csv(group, group_file, row.names = FALSE)

w <- 1400
b <- chromote::ChromoteSession$new(width = w, height = 1000)
b$Page$navigate(sprintf("http://127.0.0.1:%d/", port))
Sys.sleep(5)
js <- function(x, wait = 2) {
  b$Runtime$evaluate(x)
  Sys.sleep(wait)
}
tab <- function(id) js(sprintf("document.querySelector('a[data-value=\"%s\"]').click()", id))
# cliprect is in page coordinates: y is where the screenshot starts
y <- 0
top <- function() y <<- 0
scroll_to <- function(sel) {
  r <- b$Runtime$evaluate(sprintf(
    "document.querySelector('%s').closest('.card').getBoundingClientRect().top + window.scrollY",
    sel))
  y <<- r$result$value - 10
}
shot <- function(file, height = 1000) {
  b$screenshot(file, cliprect = c(0, y, w, height), scale = 1)
}
select <- function(id, value, wait = 1) {
  js(sprintf("document.getElementById('%s').selectize.setValue('%s')", id, value), wait)
}

man <- "vignettes/articles/figures"
dir.create(man, showWarnings = FALSE, recursive = TRUE)

# Read, analyse, zones, group, reliability
js("document.getElementById('data-read').click()", 4)
tab("analysis")
js("document.getElementById('analysis-run').click()", 8)
tab("results")
js("document.getElementById('results-ID').value = 'P01'; $('#results-ID').trigger('change')", 1)
js("document.getElementById('results-add').click()", 2)
tab("zones")
select("zones-compare_by", "sex")
select("zones-sex", "male")
js("document.getElementById('zones-run').click()", 4)
tab("reliability")
js("document.querySelector('input[name=\"reliability-source\"][value=\"file\"]').click()", 1)
doc <- b$DOM$getDocument()
node <- b$DOM$querySelector(doc$root$nodeId, "#reliability-file")
b$DOM$setFileInputFiles(files = list(normalizePath(group_file)), nodeId = node$nodeId)
Sys.sleep(3)
js("document.getElementById('reliability-run').click()", 5)

f <- function(name) file.path(man, sprintf("manual-%s.png", name))
tab("data"); top(); shot(f("data"), 900)
tab("analysis"); top(); shot(f("analysis"))
tab("results"); top(); shot(f("results"))
scroll_to("#results-sin_plot"); shot(f("results-sin"))
tab("zones"); top(); shot(f("zones"))
tab("report"); top(); shot(f("report"), 700)
tab("reliability"); top(); shot(f("reliability"))

# README and vignette("app")
file.copy(file.path(man, "manual-results.png"), "man/figures/README-app-results.png",
          overwrite = TRUE)
file.copy(file.path(man, "manual-results.png"), "vignettes/figures/app-results.png",
          overwrite = TRUE)
file.copy(file.path(man, "manual-data.png"), "vignettes/figures/app-data.png",
          overwrite = TRUE)
file.copy(file.path(man, "manual-zones.png"), "vignettes/figures/app-zones.png",
          overwrite = TRUE)
b$close()
