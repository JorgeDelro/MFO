# TODO: source for the plateau (150 ml/min), RER (1.10) and HR (90 %)
# thresholds and for the Tanaka and Fox HRmax equations (Jorge, SPEC 19).

#' VO2max of a graded test, with maximality criteria
#'
#' Computes VO2max as the highest mean VO2 over `window` seconds of
#' consecutive records (as `MFO()` does from `db_graded`) and checks whether
#' the test was maximal. The value is labelled `"VO2max"` when VO2 reached a
#' plateau, or, without a plateau, when both the RER and the heart-rate
#' criteria are met; otherwise it is labelled `"VO2peak"` (SPEC 14).
#'
#' Criteria (thresholds are arguments):
#' \itemize{
#' \item plateau: the mean VO2 of the last stage is less than
#'   `plateau_threshold` ml/min above that of the previous stage. Stages are
#'   runs of equal `Load`, each averaged over its last `window` s; without a
#'   `Load` column the criterion is not evaluated.
#' \item RER: the highest `window`-s mean RER is above `RER_threshold`.
#' \item HR: the highest `window`-s mean HR is at least `HR_threshold` of the
#'   predicted HRmax. Needs `age`; without it the criterion is not evaluated
#'   and a warning says so.
#' }
#' Predicted HRmax: `"tanaka"` 208 - 0.7 x age (default) or `"fox"`
#' 220 - age. The default thresholds and both HRmax equations are pending
#' source verification (SPEC 19).
#'
#' @param db_graded data frame of the graded test with `VO2` (ml/min) and,
#'   for the criteria, `RER`, `HR` (bpm) and `Load`.
#' @param step_time how often the data were collected (s).
#' @param window averaging window in seconds (default 30). If it is not a
#'   multiple of `step_time`, the number of records is rounded up and a
#'   message reports the effective window (as `vo2max_window` in [MFO()]).
#' @param age age in years, for the predicted HRmax (optional).
#' @param HRmax_formula `"tanaka"` (default) or `"fox"`.
#' @param plateau_threshold VO2 increase between the last two stages below
#'   which there is a plateau, ml/min (default 150).
#' @param RER_threshold RER above which the RER criterion is met (default 1.10).
#' @param HR_threshold proportion of the predicted HRmax that the peak HR
#'   must reach (default 0.90).
#'
#' @return An object of class `MFO_VO2max`: a list with `VO2max` (ml/min),
#'   `type` (`"VO2max"` or `"VO2peak"`), `window` (effective, s),
#'   `window_requested`, `n_records`, `criteria` (a data frame with one row
#'   per criterion: `criterion`, `value`, `threshold`, `met`, `NA` when not
#'   evaluated), `HRmax_predicted`, `HRmax_formula` and `age`. It can be passed
#'   to [MFO()] as `VO2max`.
#'
#' @seealso [MFO()]
#'
#' @export
#'
#' @examples
#' data(VO2max_df, package = "MFO")
#' VO2max_from_test(VO2max_df, step_time = 20, age = 30)
VO2max_from_test <- function(db_graded,
                             step_time,
                             window = 30,
                             age = NULL,
                             HRmax_formula = c("tanaka", "fox"),
                             plateau_threshold = 150,
                             RER_threshold = 1.10,
                             HR_threshold = 0.90) {
  .vo2max_test(db_graded, step_time, window, age, match.arg(HRmax_formula),
               plateau_threshold, RER_threshold, HR_threshold, warn_age = TRUE)
}


#' @export
print.MFO_VO2max <- function(x, ...) {
  cat(sprintf("<MFO_VO2max> %s = %.1f ml/min (highest %g-s mean)\n",
              x$type, x$VO2max, x$window))
  for (i in seq_len(nrow(x$criteria))) {
    cr <- x$criteria[i, ]
    status <- if (is.na(cr$met)) "not evaluated" else if (cr$met) "met" else "not met"
    cat(sprintf("  %-8s %-14s value %s, threshold %s\n", cr$criterion, status,
                format(round(cr$value, 3)), format(cr$threshold)))
  }
  invisible(x)
}


# Core of VO2max_from_test(); MFO() calls it with warn_age = FALSE and gives
# a once-per-session message instead, so batches are not flooded
.vo2max_test <- function(db_graded, step_time, window, age, HRmax_formula,
                         plateau_threshold, RER_threshold, HR_threshold,
                         warn_age = TRUE) {
  db_graded <- data.frame(db_graded)
  est <- .vo2max_rolling(db_graded[["VO2"]], step_time = step_time, window = window)
  n <- est$n_records

  # Plateau: last two stages of equal Load, each averaged over its last n records
  plateau <- NA_real_
  load <- suppressWarnings(as.numeric(db_graded[["Load"]]))
  if (length(load) > 0 && sum(!is.na(load)) > 0) {
    stage <- cumsum(c(TRUE, diff(ifelse(is.na(load), -Inf, load)) != 0))
    stages <- unique(stage[!is.na(load)])
    if (length(stages) >= 2) {
      stage_mean <- vapply(utils::tail(stages, 2), function(k) {
        v <- db_graded$VO2[stage == k]
        mean(utils::tail(v, n), na.rm = TRUE)
      }, numeric(1))
      plateau <- stage_mean[2] - stage_mean[1]
    }
  }

  peak_RER <- .rolling_max(db_graded[["RER"]], n)
  peak_HR <- .rolling_max(db_graded[["HR"]], n)
  HRmax_pred <- NA_real_
  HR_ratio <- NA_real_
  if (is.null(age)) {
    if (warn_age) {
      warning("age not given: the heart-rate criterion of VO2max is not evaluated.",
              call. = FALSE)
    }
  } else {
    if (!is.numeric(age) || length(age) != 1 || is.na(age) || age <= 0) {
      stop("age must be a single positive number of years.", call. = FALSE)
    }
    HRmax_pred <- switch(HRmax_formula, tanaka = 208 - 0.7 * age, fox = 220 - age)
    HR_ratio <- peak_HR / HRmax_pred
  }

  criteria <- data.frame(
    criterion = c("plateau", "RER", "HR"),
    value = c(plateau, peak_RER, HR_ratio),
    threshold = c(plateau_threshold, RER_threshold, HR_threshold),
    met = c(plateau < plateau_threshold, peak_RER > RER_threshold,
            HR_ratio >= HR_threshold),
    stringsAsFactors = FALSE
  )
  met <- function(k) isTRUE(criteria$met[criteria$criterion == k])
  type <- if (met("plateau") || (met("RER") && met("HR"))) "VO2max" else "VO2peak"

  structure(list(VO2max = est$VO2max, type = type, window = est$window,
                 window_requested = window, n_records = n, criteria = criteria,
                 HRmax_predicted = HRmax_pred, HRmax_formula = HRmax_formula,
                 age = age),
            class = "MFO_VO2max")
}


# Highest mean of n consecutive records; NA if the column is missing or has
# no run of n records without NA
.rolling_max <- function(x, n) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) < n) return(NA_real_)
  r <- stats::filter(x, rep(1 / n, n), sides = 1)
  if (all(is.na(r))) return(NA_real_)
  max(r, na.rm = TRUE)
}
