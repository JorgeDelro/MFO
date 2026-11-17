#' Read a metabolic cart export
#'
#' Reads the breath-by-breath export of a metabolic cart, averages it into
#' fixed intervals and returns the tables that [MFO()] expects, in the
#' package's internal data contract (SPEC 16). This is the only layer that
#' knows the proprietary formats.
#'
#' The file is parsed with `spiro::spiro_raw()` (package \pkg{spiro}, rOpenSci,
#' JOSS 2023; in Suggests), which reads Cortex (MetaSoft Studio), Cosmed
#' (Omnia, Quark CPET, K5), Vyaire Vyntus CPX (SentrySuite) and ZAN exports.
#' MFO has validated none of these devices with a real export of its own yet
#' (one anonymised file per device is planned in `inst/extdata`); ZAN is
#' tested only with the example files of \pkg{spiro}.
#'
#' **Averaging.** The breaths are averaged in consecutive intervals of
#' `interval` s from time 0: each value is the mean of the breaths measured in
#' that interval, not an interpolation. An interval without breaths is kept as
#' a row of `NA`, so the records stay aligned in time (`MFO()` cuts steps by
#' counting records). The default of 10 s divides the usual step durations
#' (60-300 s) and the 30-s windows of VO2max and ramps exactly, so they need no
#' rounding.
#'
#' **Units.** VO2 and VCO2 are returned in mL/min. With `vo2_unit = "auto"` a
#' median VO2 above 100 is read as mL/min; at or below 100 the unit must be
#' given, because L/min and mL/kg/min cannot be told apart reliably. RER is
#' always VCO2/VO2 of the interval means; when the export has its own RER, a
#' warning counts the intervals where they differ by more than 0.02. Load is
#' passed through unconverted: declare its unit in [MFO_protocol()].
#'
#' **Phases.** `cuts` gives the times (s from the start) at which the resting
#' measure ends (`basal_end`) and the MFO test ends (`MFO_end`). With both,
#' the recording is split into basal, MFO and graded tables; with
#' `MFO_end` only, into MFO and graded (a single-day test, see
#' [MFO_protocol()]); with `basal_end` only, into basal and MFO. Without
#' `cuts`, the whole recording is the phase named in `phase`. Detecting the
#' phases from the analyser's markers will come with each device importer.
#'
#' No personal data from the file are kept: the result holds only the
#' measured variables and technical information.
#'
#' @param file path of the export.
#' @param device `NULL` (detected by \pkg{spiro}), `"cortex"`, `"cosmed"`,
#'   `"vyntus"` or `"zan"`.
#' @param interval length of the averaging intervals in seconds (default 10);
#'   it becomes `step_time` for [MFO()].
#' @param cuts optional named vector of cut times in seconds, with `basal_end`
#'   and/or `MFO_end`.
#' @param phase without `cuts`, the phase of the whole recording: `"MFO"`
#'   (default), `"basal"` or `"graded"`.
#' @param body_mass body mass in kg, needed only when `vo2_unit = "mL/kg/min"`.
#' @param vo2_unit unit of VO2 and VCO2 in the export: `"auto"` (default),
#'   `"mL/min"`, `"L/min"` or `"mL/kg/min"`.
#'
#' @return An object of class `MFO_cart`: a list with `db_basal`, `db_MFO`,
#'   `db_graded` (data frames with `Time`, `VO2`, `VCO2`, `RER` and, when the
#'   export has them, `HR`, `Load`, `VE`; `NULL` for a phase not in the file),
#'   `step_time` (= `interval`) and `info` (device, interval, VO2 unit, cuts,
#'   number of breaths, of intervals and of empty intervals, and the RER
#'   mismatch count, `NA` when the export has no RER).
#'
#' @seealso [MFO()], [MFO_protocol()]
#'
#' @export
#'
#' @examples
#' \donttest{
#' if (requireNamespace("spiro", quietly = TRUE)) {
#'   # A graded test exported by a ZAN cart (example file of spiro)
#'   cart <- read_cart(spiro::spiro_example("zan_gxt"), phase = "graded")
#'   cart
#'   VO2max_from_test(cart$db_graded, step_time = cart$step_time)
#' }
#' }
read_cart <- function(file,
                      device = NULL,
                      interval = 10,
                      cuts = NULL,
                      phase = c("MFO", "basal", "graded"),
                      body_mass = NULL,
                      vo2_unit = c("auto", "mL/min", "L/min", "mL/kg/min")) {
  phase <- match.arg(phase)
  vo2_unit <- match.arg(vo2_unit)
  if (!is.null(device)) {
    device <- match.arg(device, c("cortex", "cosmed", "vyntus", "zan"))
  }
  if (!requireNamespace("spiro", quietly = TRUE)) {
    stop("read_cart() needs the package spiro to read cart exports: ",
         "install.packages(\"spiro\").", call. = FALSE)
  }
  if (!file.exists(file)) stop("File not found: ", file, call. = FALSE)

  raw <- spiro::spiro_raw(file, device = device, anonymize = TRUE)
  breaths <- data.frame(Time = raw$time, VO2 = raw$VO2, VCO2 = raw$VCO2)
  if ("HR" %in% names(raw)) breaths$HR <- raw$HR
  if ("load" %in% names(raw)) breaths$Load <- raw$load
  if ("VE" %in% names(raw)) breaths$VE <- raw$VE

  .cart_from_breaths(breaths, interval = interval, cuts = cuts, phase = phase,
                     body_mass = body_mass, vo2_unit = vo2_unit,
                     device = if (is.null(device)) "detected by spiro" else device)
}


#' @export
print.MFO_cart <- function(x, ...) {
  i <- x$info
  cat(sprintf("<MFO_cart> %s, %g-s intervals, VO2 in %s\n", i$device, i$interval,
              i$vo2_unit))
  for (ph in c("basal", "MFO", "graded")) {
    db <- x[[paste0("db_", ph)]]
    if (!is.null(db)) {
      cat(sprintf("  %-7s %4d intervals (%g-%g s)\n", ph, nrow(db),
                  min(db$Time) - i$interval, max(db$Time)))
    }
  }
  cat(sprintf("  %d breaths; %d empty interval(s)", i$n_breaths, i$empty_intervals))
  if (!is.na(i$RER_mismatch)) {
    cat(sprintf("; exported RER off by > 0.02 in %d interval(s)", i$RER_mismatch))
  }
  cat("\n")
  invisible(x)
}
