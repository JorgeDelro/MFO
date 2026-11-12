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
#'   `Load` column.
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
MFO_protocol <- function(type = c("step", "step_variable", "relative"),
                         step_duration = 180,
                         steady_state_duration = 60,
                         load_start = NULL,
                         load_increment = NULL,
                         load_unit = c("W", "km/h", "W/kg"),
                         mode = c("cycling", "running", "walking")) {

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
                 mode = mode),
            class = "MFO_protocol")
}


#' @export
print.MFO_protocol <- function(x, ...) {
  cat("<MFO_protocol>", x$type, "\n")
  cat("  steps:       ", paste(format(x$step_duration), collapse = ", "), "s",
      if (x$type == "step_variable") "(last one repeated)", "\n")
  cat("  averaged:    last", paste(format(x$steady_state_duration), collapse = ", "),
      "s of each step\n")
  if (!is.null(x$load_start)) {
    cat("  load:        ", format(x$load_start), "+", format(x$load_increment),
        x$load_unit, "per step\n")
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
