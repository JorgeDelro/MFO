#' Describe the protocol of an MFO test
#'
#' Builds an `MFO_protocol` object that tells [MFO()] and [MFOs()] how the
#' test was run: how long each step lasts, which final segment of each step
#' is averaged, how the load was prescribed and the exercise mode. The object
#' only describes the test; it holds no data. Passing it as `protocol`
#' replaces `step_duration` and `steady_state_duration` (SPEC 14).
#'
#' @param type protocol type:
#'   \itemize{
#'   \item `"step"` (default): steps of fixed duration, e.g. 3 min with
#'     35-W increments (Achten et al. 2002,
#'     \doi{10.1097/00005768-200201000-00015}).
#'   \item `"step_variable"`: steps of different duration, e.g. a longer first
#'     step on a treadmill (Amaro-Gahete et al. 2019,
#'     \doi{10.3389/fphys.2019.00909}). `step_duration` is a vector; if the
#'     data go on after its last element, that last duration is repeated, so
#'     `c(300, 180)` means a 5-min first step followed by 3-min steps.
#'   \item `"relative"`: fixed-duration steps whose load is prescribed relative
#'     to body mass (W/kg), as in the relative power FATmax test of
#'     postmenopausal females (2026,
#'     \url{https://pmc.ncbi.nlm.nih.gov/articles/PMC13315815/}). Steps are
#'     cut as in `"step"`; the load unit is `"W/kg"`.
#'   \item `"ramp"`: continuous load increase (e.g. 10-15 W/min) with no
#'     steps (ramp vs. graded protocols in postmenopausal women, Physiologia
#'     2025, 5(1):3, \doi{10.3390/physiologia5010003}). Each point of the
#'     curve is the mean of `ramp_window` s; windows start every `ramp_step` s
#'     (default: back to back, no overlap). `step_duration` and
#'     `steady_state_duration` are not used, `method = "MV"` is not available
#'     and `min_steps` counts non-overlapping windows. With overlapping windows
#'     the points are not independent: R² and the SIN standard errors are
#'     optimistic.
#'   \item `"single_day"`: the MFO test followed, after a short pause, by the
#'     maximal test in the same recording (Sports Med Health Sci 2026,
#'     8(3):327-334, \doi{10.1016/j.smhs.2025.02.004}). Steps are cut as in
#'     `"step"` up to `cut_time`; the records after it are the graded test,
#'     used for VO2max when `db_graded` and `VO2max` are not given. Automatic
#'     detection of the phases belongs to the importers (SPEC 16).
#'   }
#' @param step_duration duration of each step in seconds (default 180). A
#'   single number for `"step"` and `"relative"`; a vector for
#'   `"step_variable"`.
#' @param steady_state_duration final segment of each step that is averaged,
#'   in seconds (default 60). A single number, or for `"step_variable"` a
#'   vector of the same length as `step_duration` (its last element is
#'   repeated with the last duration). It cannot be longer than its step.
#' @param load_start,load_increment optional load of the first step and
#'   increment per step, in `load_unit`. Used only when the test data have no
#'   `Load` column. For `"ramp"`, `load_start` is the load at time 0 and
#'   `load_increment` the increase per minute; each window takes the load at
#'   its midpoint.
#' @param ramp_window,ramp_step for `"ramp"`: length of each averaging window
#'   and time between window starts, in seconds (defaults 30 and
#'   `ramp_window`).
#' @param cut_time for `"single_day"`: time from the start of the recording
#'   (s) at which the MFO test ends and the graded test begins.
#' @param load_unit unit of the load: `"W"` (default), `"km/h"` or `"W/kg"`
#'   (always `"W/kg"` for `type = "relative"`).
#' @param mode exercise mode: `"cycling"` (default), `"running"` or
#'   `"walking"`. Fat oxidation kinetics differ between modes (Cheneviere et
#'   al. 2010); the mode is reported with the results.
#'
#' @return An object of class `MFO_protocol`: a list with the arguments above.
#'
#' @seealso [MFO()], [MFOs()]
#'
#' @export
#'
#' @examples
#' # Classic protocol: 3-min steps, last minute averaged
#' MFO_protocol()
#'
#' # Treadmill test with a 5-min first step and 3-min steps after it
#' MFO_protocol("step_variable", step_duration = c(300, 180), mode = "walking")
#'
#' # Relative power test: 4-min steps of 0.15 W/kg from 0.45 W/kg
#' MFO_protocol("relative", step_duration = 240,
#'              load_start = 0.45, load_increment = 0.15)
#'
#' # Ramp of 15 W/min from 20 W, 30-s windows
#' MFO_protocol("ramp", load_start = 20, load_increment = 15)
#'
#' # MFO test and maximal test in one recording; the MFO part lasts 24 min
#' MFO_protocol("single_day", cut_time = 24 * 60)
MFO_protocol <- function(type = c("step", "step_variable", "relative", "ramp",
                                  "single_day"),
                         step_duration = 180,
                         steady_state_duration = 60,
                         load_start = NULL,
                         load_increment = NULL,
                         load_unit = c("W", "km/h", "W/kg"),
                         mode = c("cycling", "running", "walking"),
                         ramp_window = 30,
                         ramp_step = ramp_window,
                         cut_time = NULL) {

  type <- match.arg(type)
  mode <- match.arg(mode)
  if (type == "relative") {
    if (!missing(load_unit) && !identical(load_unit, "W/kg")) {
      stop('type = "relative" uses load_unit = "W/kg".', call. = FALSE)
    }
    load_unit <- "W/kg"
  } else {
    load_unit <- match.arg(load_unit)
  }

  .check_seconds(step_duration, "step_duration")
  .check_seconds(steady_state_duration, "steady_state_duration")
  if (type == "ramp") {
    .check_seconds(ramp_window, "ramp_window")
    .check_seconds(ramp_step, "ramp_step")
    if (length(ramp_window) != 1 || length(ramp_step) != 1) {
      stop("ramp_window and ramp_step must be single numbers.", call. = FALSE)
    }
    if (ramp_step > ramp_window) {
      stop("ramp_step cannot be longer than ramp_window (records would be skipped).",
           call. = FALSE)
    }
  }
  if (type == "single_day") {
    if (is.null(cut_time)) stop('type = "single_day" needs cut_time (s).', call. = FALSE)
    .check_seconds(cut_time, "cut_time")
    if (length(cut_time) != 1) stop("cut_time must be a single number.", call. = FALSE)
  } else if (!is.null(cut_time)) {
    stop('cut_time is only used with type = "single_day".', call. = FALSE)
  }
  if (type == "step_variable") {
    if (length(steady_state_duration) != 1 &&
        length(steady_state_duration) != length(step_duration)) {
      stop("steady_state_duration must be a single number or have the same ",
           "length as step_duration.", call. = FALSE)
    }
  } else if (length(step_duration) != 1 || length(steady_state_duration) != 1) {
    stop(sprintf('type = "%s" needs a single step_duration and steady_state_duration; ',
                 type), 'use type = "step_variable" for steps of different duration.',
         call. = FALSE)
  }
  if (any(steady_state_duration > step_duration)) {
    stop("steady_state_duration cannot be longer than its step_duration.", call. = FALSE)
  }

  if (xor(is.null(load_start), is.null(load_increment))) {
    stop("Give both load_start and load_increment, or neither.", call. = FALSE)
  }
  if (!is.null(load_start)) {
    if (!is.numeric(load_start) || length(load_start) != 1 || is.na(load_start) ||
        !is.numeric(load_increment) || length(load_increment) != 1 || is.na(load_increment)) {
      stop("load_start and load_increment must be single numbers.", call. = FALSE)
    }
  }

  structure(list(type = type,
                 step_duration = step_duration,
                 steady_state_duration = steady_state_duration,
                 load_start = load_start,
                 load_increment = load_increment,
                 load_unit = load_unit,
                 mode = mode,
                 ramp_window = if (type == "ramp") ramp_window,
                 ramp_step = if (type == "ramp") ramp_step,
                 cut_time = cut_time),
            class = "MFO_protocol")
}


#' @export
print.MFO_protocol <- function(x, ...) {
  cat("<MFO_protocol>", x$type, "\n")
  if (x$type == "ramp") {
    cat("  windows:     ", format(x$ramp_window), "s, one every", format(x$ramp_step), "s\n")
  } else {
    cat("  steps:       ", paste(format(x$step_duration), collapse = ", "), "s",
        if (x$type == "step_variable") "(last one repeated)", "\n")
    cat("  averaged:    last", paste(format(x$steady_state_duration), collapse = ", "),
        "s of each step\n")
  }
  if (x$type == "single_day") {
    cat("  MFO test:     first", format(x$cut_time), "s; graded test after\n")
  }
  if (!is.null(x$load_start)) {
    cat("  load:        ", format(x$load_start), "+", format(x$load_increment),
        x$load_unit, if (x$type == "ramp") "per min" else "per step", "\n")
  } else {
    cat("  load unit:   ", x$load_unit, "\n")
  }
  cat("  mode:        ", x$mode, "\n")
  invisible(x)
}


# Positive, finite seconds
.check_seconds <- function(x, name) {
  if (!is.numeric(x) || length(x) < 1 || anyNA(x) || any(!is.finite(x)) || any(x <= 0)) {
    stop(name, " must be positive number(s) of seconds.", call. = FALSE)
  }
}


# Protocol from the arguments of MFO()/MFOs(): `protocol` wins over
# step_duration/steady_state_duration, which build a "step" protocol (v0.2.x)
.resolve_protocol <- function(protocol, step_duration, steady_state_duration,
                              durations_given) {
  if (is.null(protocol)) {
    return(MFO_protocol("step", step_duration = step_duration,
                        steady_state_duration = steady_state_duration))
  }
  if (!inherits(protocol, "MFO_protocol")) {
    stop("protocol must be built with MFO_protocol().", call. = FALSE)
  }
  if (durations_given) {
    warning("Both protocol and step_duration/steady_state_duration supplied; ",
            "using protocol.", call. = FALSE)
  }
  protocol
}


# Records of each step for n_records rows at step_time s. Returns a data
# frame with, per step, its duration and averaged segment (records and s),
# the first and last record averaged, and whether the step is complete.
# Durations that are not a multiple of step_time are truncated to whole
# records, as in v0.2.x. The last row may be an incomplete step.
.step_bounds <- function(protocol, n_records, step_time) {
  if (protocol$type == "ramp") return(.ramp_bounds(protocol, n_records, step_time))
  dur <- protocol$step_duration
  ss <- rep_len(protocol$steady_state_duration, length(dur))
  n_dur <- as.integer(dur / step_time)
  n_ss <- as.integer(ss / step_time)
  if (any(n_dur < 1) || any(n_ss < 1)) {
    stop("step_duration and steady_state_duration must be at least step_time (",
         format(step_time), " s).", call. = FALSE)
  }

  ends <- integer(0); recs <- integer(0); steady <- integer(0); i <- 0L
  end <- 0L
  repeat {
    i <- i + 1L
    k <- min(i, length(n_dur))
    if (end >= n_records) break
    ends <- c(ends, min(end + n_dur[k], n_records))
    recs <- c(recs, n_dur[k])
    steady <- c(steady, n_ss[k])
    end <- end + n_dur[k]
  }
  data.frame(step = seq_along(ends),
             end = ends,
             records = recs,
             steady_records = steady,
             available = c(ends[1], diff(ends)),
             complete = c(ends[1], diff(ends)) == recs)
}


# Records of a ramp window or step: rounded up, as the VO2max window, so a
# 30-s window with 20-s records uses 2 records (40 s), with a message
.ramp_records <- function(seconds, step_time, name) {
  n <- max(1L, as.integer(ceiling(seconds / step_time - 1e-8)))
  if (!isTRUE(all.equal(n * step_time, seconds))) {
    message(sprintf("%s: %g s is not a multiple of step_time (%g s); using %d record(s) = %g s.",
                    name, seconds, step_time, n, n * step_time))
  }
  n
}


# Ramp windows: ramp_window s long, one every ramp_step s, all complete; the
# records after the last full window are not used
.ramp_bounds <- function(protocol, n_records, step_time) {
  w <- .ramp_records(protocol$ramp_window, step_time, "ramp_window")
  st <- if (protocol$ramp_step == protocol$ramp_window) w else
    .ramp_records(protocol$ramp_step, step_time, "ramp_step")
  if (n_records < w) {
    return(data.frame(step = integer(0), end = integer(0), records = integer(0),
                      steady_records = integer(0), available = integer(0),
                      complete = logical(0)))
  }
  ends <- seq(w, n_records, by = st)
  data.frame(step = seq_along(ends), end = ends, records = w, steady_records = w,
             available = w, complete = TRUE)
}


# Number of non-overlapping windows that n ramp points are worth (min_steps):
# the first window plus one more every ramp_window of advance
.independent_points <- function(n, protocol, step_time) {
  if (protocol$type != "ramp") return(n)
  if (n == 0) return(0)
  w <- suppressMessages(.ramp_records(protocol$ramp_window, step_time, ""))
  st <- suppressMessages(.ramp_records(protocol$ramp_step, step_time, ""))
  floor((n - 1) * st / w) + 1
}
