#' VO2max as the highest rolling mean of a graded test
#'
#' Takes the highest mean VO2 over `window` seconds of consecutive records,
#' instead of the single highest record, which overestimates VO2max when data
#' are averaged over short intervals (SPEC 9.5 and 14). When `window` is not a
#' multiple of `step_time`, the number of records is rounded up so the
#' effective window is never shorter than requested (e.g. 30 s with 20-s
#' records uses 2 records = 40 s).
#'
#' @param VO2 numeric vector of VO2 values (ml/min), one per record.
#' @param step_time how often the data was collected (in seconds).
#' @param window requested averaging window in seconds (default 30).
#'
#' @return list with `VO2max` (ml/min), `window` (effective window in seconds)
#'   and `n_records` (records averaged).
#'
#' @noRd
.vo2max_rolling <- function(VO2, step_time, window = 30) {

  if (!is.numeric(window) || length(window) != 1 || is.na(window) || window <= 0) {
    stop("`vo2max_window` must be a single positive number of seconds.")
  }

  n_records <- as.integer(ceiling(window / step_time - 1e-8))
  n_records <- max(1L, n_records)
  effective_window <- n_records * step_time

  if (!isTRUE(all.equal(effective_window, window))) {
    message(sprintf(
      "VO2max: a %g-s window is not a multiple of step_time (%g s); using %d record(s) = %g s.",
      window, step_time, n_records, effective_window))
  }

  if (sum(!is.na(VO2)) < n_records) {
    stop(sprintf("db_graded has fewer than %d valid VO2 records for a %g-s window.",
                 n_records, effective_window))
  }

  # Mean of each run of n_records consecutive records; windows with NA are skipped
  rolling_means <- stats::filter(VO2, rep(1 / n_records, n_records), sides = 1)
  if (all(is.na(rolling_means))) {
    stop(sprintf("db_graded has no run of %d consecutive VO2 records without NA.", n_records))
  }
  VO2max <- max(rolling_means, na.rm = TRUE)

  list(VO2max = VO2max, window = effective_window, n_records = n_records)
}
