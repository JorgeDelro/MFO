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
#' **Generic files** (`device = "generic"`). A csv or xlsx file whose
#' columns are mapped with `columns`, as `col_name_*` in [MFO()] (SPEC 16):
#' \itemize{
#' \item `columns` maps the contract names `Time`, `VO2`, `VCO2` and,
#'   optionally, `RER`, `HR`, `Load` and `VE` to the names in the file, e.g.
#'   `c(Time = "t", VO2 = "VO2 STPD")`; names not given are looked up as they
#'   are. Names must match exactly; a missing column stops with the list of
#'   columns in the file.
#' \item `skip` lines (e.g. metadata) come before the header; with
#'   `units_row = TRUE` the line after the header holds the units. A
#'   recognised VO2 unit there ("mL/min", "L/min", "mL/kg/min") is taken as a
#'   declaration of the file, with a message; an explicit `vo2_unit` wins.
#' \item `decimal` is the decimal mark (default "."); the csv field separator
#'   (tab, ";" or ",") is detected from the header line.
#' \item `Time` is the elapsed time from the start of the recording, in seconds,
#'   mm:ss, hh:mm:ss or Excel times (fractions of a day), detected
#'   automatically.
#' \item Records already averaged by the analyser (e.g. every 20 s) are read
#'   with `interval = "none"`: they are kept as they are and `step_time` is the
#'   step of `Time`, which must be regular. A numeric `interval` is for
#'   breath-by-breath files.
#' \item `sheets` reads the phases from the sheets of an xlsx file,
#'   `c(basal = , MFO = , graded = )` (any of them), as an alternative to
#'   `cuts`; one unnamed sheet is the phase named in `phase`.
#' }
#'
#' No personal data from the file are kept: the result holds only the
#' measured variables and technical information.
#'
#' @param file path of the export.
#' @param device `NULL` (detected by \pkg{spiro}), `"cortex"`, `"cosmed"`,
#'   `"vyntus"`, `"zan"`, or `"generic"` for a csv/xlsx file with mapped
#'   columns (does not need \pkg{spiro}).
#' @param interval length of the averaging intervals in seconds (default 10),
#'   or `"none"` for records already averaged by the analyser; it becomes
#'   `step_time` for [MFO()].
#' @param cuts optional named vector of cut times in seconds, with `basal_end`
#'   and/or `MFO_end`.
#' @param phase without `cuts`, the phase of the whole recording: `"MFO"`
#'   (default), `"basal"` or `"graded"`.
#' @param body_mass body mass in kg, needed only when `vo2_unit = "mL/kg/min"`.
#' @param vo2_unit unit of VO2 and VCO2 in the export: `"auto"` (default),
#'   `"mL/min"`, `"L/min"` or `"mL/kg/min"`.
#' @param columns,skip,units_row,decimal,sheets for `device = "generic"`
#'   only: column mapping (named character vector), lines before the header
#'   (default 0), whether a units row follows the header (default `FALSE`),
#'   decimal mark (`"."` or `","`) and xlsx sheets per phase. See *Generic
#'   files*.
#'
#' @return An object of class `MFO_cart`: a list with `db_basal`, `db_MFO`,
#'   `db_graded` (data frames with `Time`, `VO2`, `VCO2`, `RER` and, when the
#'   export has them, `HR`, `Load`, `VE`; `NULL` for a phase not in the file),
#'   `step_time` (= `interval`, or the step of `Time` with `interval = "none"`)
#'   and `info` (device, interval, averaging, VO2 unit, cuts, number of breaths
#'   or records, of intervals and of empty intervals, and the RER mismatch
#'   count, `NA` when the export has no RER).
#'
#' @seealso [MFO()], [MFO_protocol()]
#'
#' @export
#'
#' @examples
#' # A generic csv: metadata line, header, units row, decimal comma, records
#' # already averaged every 20 s (the example MFO test, VO2 in L/min)
#' data(MFO_df, package = "MFO")
#' t <- seq_len(nrow(MFO_df)) * 20
#' f <- tempfile(fileext = ".csv")
#' writeLines(c("Example export;MFO test",
#'              "t;VO2;VCO2;FC;Watts",
#'              "mm:ss;L/min;L/min;1/min;W",
#'              sprintf("%02d:%02d;%s;%s;%s;%s", t %/% 60, t %% 60,
#'                      sub(".", ",", MFO_df$VO2 / 1000, fixed = TRUE),
#'                      sub(".", ",", MFO_df$VCO2 / 1000, fixed = TRUE),
#'                      sub(".", ",", MFO_df$HR, fixed = TRUE), MFO_df$Load)), f)
#' cart <- read_cart(f, device = "generic", interval = "none", skip = 1,
#'                   units_row = TRUE, decimal = ",",
#'                   columns = c(Time = "t", HR = "FC", Load = "Watts"))
#' cart
#' head(cart$db_MFO)
#'
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
                      vo2_unit = c("auto", "mL/min", "L/min", "mL/kg/min"),
                      columns = NULL,
                      skip = 0,
                      units_row = FALSE,
                      decimal = c(".", ","),
                      sheets = NULL) {
  phase <- match.arg(phase)
  vo2_unit <- match.arg(vo2_unit)
  decimal <- match.arg(decimal)
  if (!is.null(device)) {
    device <- match.arg(device, c("cortex", "cosmed", "vyntus", "zan", "generic"))
  }
  if (!file.exists(file)) stop("File not found: ", file, call. = FALSE)
  if (identical(device, "generic")) {
    return(.read_generic(file, interval, cuts, phase, body_mass, vo2_unit, columns,
                         skip, units_row, decimal, sheets))
  }
  if (!is.null(columns) || !is.null(sheets) || skip != 0 || units_row) {
    stop("columns, skip, units_row and sheets are for device = \"generic\".",
         call. = FALSE)
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
  as_is <- grepl("^none", if (is.null(i$averaging)) "" else i$averaging)
  cat(sprintf("<MFO_cart> %s, %g-s %s, VO2 in %s\n", i$device, i$interval,
              if (as_is) "records as exported" else "intervals", i$vo2_unit))
  for (ph in c("basal", "MFO", "graded")) {
    db <- x[[paste0("db_", ph)]]
    if (!is.null(db)) {
      cat(sprintf("  %-7s %4d intervals (%g-%g s)\n", ph, nrow(db),
                  min(db$Time) - i$interval, max(db$Time)))
    }
  }
  cat(sprintf("  %d %s; %d empty interval(s)", i$n_breaths,
              if (as_is) "records" else "breaths", i$empty_intervals))
  if (!is.na(i$RER_mismatch)) {
    cat(sprintf("; exported RER off by > 0.02 in %d interval(s)", i$RER_mismatch))
  }
  cat("\n")
  invisible(x)
}


# device = "generic": one table, or one per sheet, through the shared pipeline
.read_generic <- function(file, interval, cuts, phase, body_mass, vo2_unit, columns,
                          skip, units_row, decimal, sheets) {
  map <- .generic_columns(columns)
  if (!is.numeric(skip) || length(skip) != 1 || is.na(skip) || skip < 0) {
    stop("skip must be a single number of lines (0 or more).", call. = FALSE)
  }
  units_row <- isTRUE(units_row)
  read_one <- function(sheet, ph, cuts) {
    tab <- .read_generic_table(file, sheet, map, skip, units_row, decimal)
    unit <- .generic_unit(vo2_unit, tab$vo2_unit, tab$vco2_unit)
    .cart_from_breaths(tab$data, interval = interval, cuts = cuts, phase = ph,
                       body_mass = body_mass, vo2_unit = unit, device = "generic")
  }
  if (is.null(sheets)) return(read_one(NULL, phase, cuts))

  if (!is.character(sheets) || anyNA(sheets)) {
    stop("sheets must be sheet names, e.g. c(basal = \"Rest\", MFO = \"MFO\").",
         call. = FALSE)
  }
  if (!is.null(cuts)) stop("Use cuts or sheets, not both.", call. = FALSE)
  if (is.null(names(sheets))) {
    if (length(sheets) != 1) {
      stop("Name the sheets by phase: c(basal = , MFO = , graded = ).", call. = FALSE)
    }
    return(read_one(sheets, phase, NULL))
  }
  if (!all(names(sheets) %in% c("basal", "MFO", "graded")) || anyDuplicated(names(sheets))) {
    stop("sheets must be named basal, MFO and/or graded.", call. = FALSE)
  }
  parts <- lapply(names(sheets), function(ph) read_one(sheets[[ph]], ph, NULL))
  names(parts) <- names(sheets)
  steps <- vapply(parts, function(p) p$step_time, numeric(1))
  if (length(unique(steps)) > 1) {
    stop("The sheets have different steps (", paste(names(steps), steps, sep = " = ",
         collapse = ", "), " s); MFO() needs one step_time.", call. = FALSE)
  }
  info <- parts[[1]]$info
  info$sheets <- sheets
  info$vo2_unit <- unique(vapply(parts, function(p) p$info$vo2_unit, character(1)))
  for (k in c("n_breaths", "n_intervals", "empty_intervals")) {
    info[[k]] <- sum(vapply(parts, function(p) p$info[[k]], numeric(1)))
  }
  rer <- vapply(parts, function(p) as.numeric(p$info$RER_mismatch), numeric(1))
  info$RER_mismatch <- if (all(is.na(rer))) NA_integer_ else sum(rer, na.rm = TRUE)
  structure(list(db_basal = parts$basal$db_basal,
                 db_MFO = parts$MFO$db_MFO,
                 db_graded = parts$graded$db_graded,
                 step_time = steps[[1]],
                 info = info),
            class = "MFO_cart")
}
