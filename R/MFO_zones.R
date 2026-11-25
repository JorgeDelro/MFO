#' Heart-rate zones and reference values for MFO and Fatmax
#'
#' Expresses Fatmax and the Fatmax zone as heart rate (bpm and \% of HRmax),
#' compares the zone with the recommendation for obesity of Chavez-Guevara et
#' al. (2023) when asked, and places MFO and Fatmax among the published
#' percentiles of [reference_MFO] (SPEC 15).
#'
#' @section HRmax:
#' The HRmax used for the percentages is, in this order: `HRmax` given here or
#' in [MFO()]/[MFOs()]; the peak HR measured in the graded test (`HR_peak`,
#' the highest mean HR over the same window as \VOmax{}); and, only when
#' `predicted_HRmax = TRUE`, the predicted HRmax for `age`: `"tanaka"`
#' 208 - 0.7 x age (default; Tanaka, Monahan and Seals 2001,
#' \doi{10.1016/s0735-1097(00)01054-8}) or `"fox"` 220 - age, the equation
#' in common use described by Tanaka, Monahan and Seals (2001). The column
#' `HRmax_source` says which one was used.
#'
#' @section Obesity:
#' With `population = "obesity"`, the zone in \% HRmax is compared with the
#' range of Chavez-Guevara et al. (2023, \doi{10.1007/s40279-023-01897-y}):
#' 61-66 \% HRpeak with more than 35 \% body fat and 57-64 \% HRpeak with
#' less than 35 \%; `body_fat` chooses the range. The source does not cover
#' exactly 35.0 \%: there is no range (`NA`), with a message. `obesity_zone` is `"inside"` (the
#' whole zone falls in the range), `"partly overlaps"` or `"outside"`. The
#' range refers to the measured HRpeak: with a predicted HRmax it is still
#' shown, with a warning.
#'
#' @section Reference values:
#' MFO (g/min) and Fatmax (\% \VOmax{}) are placed among the empirical
#' percentiles P20, P40, P60 and P80 of Table 1 of Amaro-Gahete,
#' Sanchez-Delgado and Ruiz (2018, \doi{10.3389/fphys.2018.01460}), 167
#' sedentary adults, by linear interpolation. Below P20 or above P80 the
#' label is `"< P20"` or `"> P80"` and the percentile `NA`: there is no
#' extrapolation. A percentile is never returned without its group
#' (`reference`, `reference_n`).
#'
#' The group is chosen with `sex`, `age_group` and `weight_status`, only in
#' the combinations Table 1 prints: sex, age group, sex and age group, or
#' weight status. The source does not publish the age or BMI cut-offs of its
#' groups (young adults 22.1 +/- 2.2 years, middle-aged adults 52.1 +/- 4.6
#' years), so the group is never deduced from a numeric age or BMI.
#'
#' The reference was measured walking on a treadmill. Results with
#' `mode = "walking"` or `"running"` get a percentile; with `"cycling"` the
#' percentile is `NA`, because the mode of exercise changes MFO and Fatmax
#' (Amaro-Gahete et al. 2018). `reference_differences` lists how the test
#' differs from the reference: mode, equation (Frayn without protein in the
#' reference), protocol (3-min steps averaged over the last 60 s), method
#' (third-degree polynomial through the origin in the reference, unlike the
#' unconstrained `"P3"` of [MFO()]) and fasting (5-6 h, not overnight).
#'
#' @param x the result of [MFO()] or the data frame returned by [MFOs()].
#' @param HRmax maximum heart rate in bpm (optional): one value, or one per
#'   row of `x`. It takes precedence over the `HRmax` and `HR_peak` of `x`.
#' @param predicted_HRmax logical; if `TRUE`, the predicted HRmax for `age`
#'   is used when no HRmax was given or measured (default `FALSE`).
#' @param age age in years, needed with `predicted_HRmax = TRUE`; one value or
#'   one per row of `x`, as `body_fat`, `sex`, `age_group` and `weight_status`.
#' @param HRmax_formula `"tanaka"` (default) or `"fox"`.
#' @param population `"general"` (default) or `"obesity"`, which adds the
#'   range of Chavez-Guevara et al. (2023).
#' @param body_fat body fat in \% (needed with `population = "obesity"`).
#' @param sex `"male"` or `"female"` (optional) to choose the reference group.
#' @param age_group `"young"` or `"middle-aged"` (optional), the age groups
#'   of the reference.
#' @param weight_status `"normal"`, `"overweight"` or `"obese"` (optional);
#'   cannot be combined with `sex` or `age_group`.
#'
#' @return A data frame of class `MFO_zones`, one row per result, with `ID`
#'   (results of [MFOs()]), `HRmax`, `HRmax_source` (`"given"`, `"measured"`,
#'   `"predicted (Tanaka)"`, `"predicted (Fox)"` or `NA`), `FAT_MAX` and the
#'   zone (`zone_lower`, `zone_upper`; \% \VOmax{}), `FAT_MAX_HR`,
#'   `zone_lower_HR`, `zone_upper_HR` (bpm), `FAT_MAX_porcHRmax`,
#'   `zone_lower_porcHRmax`, `zone_upper_porcHRmax`, `obesity_range_lower`,
#'   `obesity_range_upper` and `obesity_zone` (`NA` unless
#'   `population = "obesity"`), `MFO`, `MFO_percentile`,
#'   `MFO_percentile_label`, `Fatmax_percentile`, `Fatmax_percentile_label`,
#'   `reference`, `reference_n` and `reference_differences`.
#'
#' @references
#' Amaro-Gahete FJ, Sanchez-Delgado G, Ruiz JR (2018). Commentary:
#' Contextualising maximal fat oxidation during exercise: determinants and
#' normative values. Front Physiol 9: 1460, Table 1.
#' \doi{10.3389/fphys.2018.01460}
#'
#' Chavez-Guevara IA, Amaro-Gahete FJ, Ramos-Jimenez A, Brun JF (2023).
#' Toward exercise guidelines for optimizing fat oxidation during exercise in
#' obesity: a systematic review and meta-regression. Sports Med 53(12):
#' 2399-2416. \doi{10.1007/s40279-023-01897-y}
#'
#' Tanaka H, Monahan KD, Seals DR (2001). Age-predicted maximal heart rate
#' revisited. J Am Coll Cardiol 37(1): 153-156.
#' \doi{10.1016/s0735-1097(00)01054-8}
#'
#' @seealso [MFO()], [MFOs()], [reference_MFO]
#'
#' @section Warnings and messages:
#' Every warning and message of this function is a classed condition:
#' `mfo_warning_<id>` and `mfo_warning` (or `mfo_message_<id>` and
#' `mfo_message`), with the numbers of the text as fields (for example
#' `mfo_warning_negative_fat` has `n`, the number of steps). The text is
#' English; the classes let a program, such as [run_app()], handle a given
#' warning or word it in another language:
#' `withCallingHandlers(expr, mfo_warning_negative_fat = function(w) w$n)`.
#'
#' @export
#'
#' @examples
#' data(list = c("MFO_df", "basal_df", "VO2max_df"), package = "MFO")
#' res <- suppressWarnings(MFO(step_time = 20, db_MFO = MFO_df,
#'                             db_basal = basal_df, db_graded = VO2max_df,
#'                             cv_var = "RER", author = "Frayn"))
#' # MFO_df is a cycling test: zones in HR, but no percentile, because the
#' # reference was measured walking on a treadmill
#' MFO_zones(res, sex = "male", age_group = "young")
#' # Range for obesity of Chavez-Guevara et al. (2023)
#' MFO_zones(res, population = "obesity", body_fat = 38)
MFO_zones <- function(x,
                      HRmax = NULL,
                      predicted_HRmax = FALSE,
                      age = NULL,
                      HRmax_formula = c("tanaka", "fox"),
                      population = c("general", "obesity"),
                      body_fat = NULL,
                      sex = NULL,
                      age_group = NULL,
                      weight_status = NULL) {
  HRmax_formula <- match.arg(HRmax_formula)
  population <- match.arg(population)
  d <- .zones_input(x)
  n <- nrow(d)

  HRmax <- .zones_arg(HRmax, n, "HRmax", numeric = TRUE)
  age <- .zones_arg(age, n, "age", numeric = TRUE)
  body_fat <- .zones_arg(body_fat, n, "body_fat", numeric = TRUE)
  sex <- .zones_arg(sex, n, "sex", choices = c("male", "female"))
  age_group <- .zones_arg(age_group, n, "age_group", choices = c("young", "middle-aged"))
  weight_status <- .zones_arg(weight_status, n, "weight_status",
                              choices = c("normal", "overweight", "obese"))
  if (any(!is.na(weight_status) & (!is.na(sex) | !is.na(age_group)))) {
    stop("Table 1 of Amaro-Gahete et al. (2018) does not cross weight_status with ",
         "sex or age_group; choose one.", call. = FALSE)
  }

  # --- HRmax: given > measured peak > predicted on request (SPEC 15.1) ---
  given <- ifelse(is.na(HRmax), d$HRmax, HRmax)
  hrmax <- given
  source <- ifelse(is.na(given), NA_character_, "given")
  use_peak <- is.na(hrmax) & !is.na(d$HR_peak)
  hrmax[use_peak] <- d$HR_peak[use_peak]
  source[use_peak] <- "measured"
  if (isTRUE(predicted_HRmax)) {
    need <- is.na(hrmax)
    if (any(need & is.na(age))) {
      stop("predicted_HRmax = TRUE needs age.", call. = FALSE)
    }
    if (!all(need)) {
      .mfo_message("hrmax_precedence",
                   paste0("An HRmax was given or measured for ", sum(!need), " result(s); it ",
                          "takes precedence over the predicted HRmax."), n = sum(!need))
    }
    pred <- switch(HRmax_formula, tanaka = 208 - 0.7 * age, fox = 220 - age)
    hrmax[need] <- pred[need]
    source[need] <- if (HRmax_formula == "tanaka") "predicted (Tanaka)" else "predicted (Fox)"
  }
  if (all(is.na(hrmax))) {
    .mfo_message("no_hrmax",
                 paste0("No HRmax given, measured or predicted: the zone is given in bpm only. ",
                        "Use HRmax, a db_graded with HR, or predicted_HRmax = TRUE with age."))
  }
  pct <- function(hr) hr / hrmax * 100

  out <- data.frame(
    HRmax = hrmax,
    HRmax_source = source,
    FAT_MAX = d$FAT_MAX,
    zone_lower = d$zone_lower,
    zone_upper = d$zone_upper,
    FAT_MAX_HR = d$FAT_MAX_HR,
    zone_lower_HR = d$zone_lower_HR,
    zone_upper_HR = d$zone_upper_HR,
    FAT_MAX_porcHRmax = pct(d$FAT_MAX_HR),
    zone_lower_porcHRmax = pct(d$zone_lower_HR),
    zone_upper_porcHRmax = pct(d$zone_upper_HR),
    obesity_range_lower = NA_real_,
    obesity_range_upper = NA_real_,
    obesity_zone = NA_character_,
    stringsAsFactors = FALSE
  )
  if (!is.null(d$ID)) out <- cbind(ID = d$ID, out, stringsAsFactors = FALSE)

  # --- Obesity range of Chavez-Guevara et al. (2023) (SPEC 15.1) ---
  if (population == "obesity") {
    if (any(is.na(body_fat))) {
      stop('population = "obesity" needs body_fat (%) to choose the range of ',
           "Chavez-Guevara et al. (2023).", call. = FALSE)
    }
    # The source defines > 35 % and < 35 %; exactly 35 % gets no range
    high <- body_fat > 35
    covered <- body_fat != 35
    if (any(!covered)) {
      .mfo_message("obesity_35",
                   paste0("Chavez-Guevara et al. (2023) define > 35 % and < 35 % body fat; ",
                          "35.0 % is not covered, so there is no range for ", sum(!covered),
                          " result(s)."), n = sum(!covered))
    }
    out$obesity_range_lower <- ifelse(covered, ifelse(high, 61, 57), NA_real_)
    out$obesity_range_upper <- ifelse(covered, ifelse(high, 66, 64), NA_real_)
    out$obesity_zone <- .zone_overlap(out$zone_lower_porcHRmax, out$zone_upper_porcHRmax,
                                      out$obesity_range_lower, out$obesity_range_upper)
    if (any(grepl("^predicted", source))) {
      .mfo_warning("obesity_predicted_hrmax",
                   paste0("The range of Chavez-Guevara et al. (2023) is in % of the measured ",
                          "HRpeak; with a predicted HRmax the comparison is approximate."))
    }
  }

  # --- Reference values: Amaro-Gahete et al. (2018), Table 1 (SPEC 15.2-15.3) ---
  ref <- MFO::reference_MFO
  rows <- vapply(seq_len(n), function(i) {
    .reference_row(ref, sex[i], age_group[i], weight_status[i])
  }, integer(1))
  comparable <- d$mode %in% c("walking", "running")
  if (any(!comparable)) {
    modes <- paste(unique(d$mode[!comparable]), collapse = "\", \"")
    .mfo_message("no_percentile",
                 paste0("No percentile for ", sum(!comparable), " result(s) with mode \"",
                        modes, "\": the ",
                        "reference (Amaro-Gahete et al. 2018) was measured walking on a treadmill, ",
                        "and the mode of exercise changes MFO and Fatmax. Set ",
                        "MFO_protocol(mode = ) if the test was walking or running."),
                 n = sum(!comparable), mode = modes)
  }
  mfo <- .reference_percentile(d$MFO, ref[rows, paste0("MFO_p", c(20, 40, 60, 80))])
  fmx <- .reference_percentile(d$FAT_MAX, ref[rows, paste0("Fatmax_p", c(20, 40, 60, 80))])
  out$MFO <- d$MFO
  out$MFO_percentile <- ifelse(comparable, mfo$percentile, NA_real_)
  out$MFO_percentile_label <- ifelse(comparable, mfo$label, NA_character_)
  out$Fatmax_percentile <- ifelse(comparable, fmx$percentile, NA_real_)
  out$Fatmax_percentile_label <- ifelse(comparable, fmx$label, NA_character_)
  out$reference <- paste0(ref$cohort[rows], ": ", ref$population[rows])
  out$reference_n <- ref$n[rows]
  out$reference_differences <- vapply(seq_len(n), function(i) {
    .reference_differences(d[i, , drop = FALSE])
  }, character(1))

  rownames(out) <- NULL
  structure(out, class = c("MFO_zones", "data.frame"), population = population)
}


#' @export
print.MFO_zones <- function(x, digits = 1, ...) {
  # A subset of columns prints as a plain data frame
  needed <- c("FAT_MAX", "FAT_MAX_HR", "FAT_MAX_porcHRmax", "zone_lower", "zone_upper",
              "zone_lower_HR", "zone_upper_HR", "zone_lower_porcHRmax",
              "zone_upper_porcHRmax", "HRmax", "HRmax_source", "obesity_range_lower",
              "obesity_range_upper", "obesity_zone", "reference", "reference_n", "MFO",
              "MFO_percentile_label", "Fatmax_percentile_label", "reference_differences")
  if (!all(needed %in% names(x))) {
    print(as.data.frame(unclass(x), stringsAsFactors = FALSE), ...)
    return(invisible(x))
  }
  f <- function(v, d = digits) ifelse(is.na(v), "NA", formatC(v, format = "f", digits = d))
  for (i in seq_len(nrow(x))) {
    r <- x[i, , drop = FALSE]
    cat(if (!is.null(r$ID)) paste0("== ", r$ID, " ==\n") else "MFO zones\n")
    cat("  Fatmax:      ", f(r$FAT_MAX), "% VO2max, ", f(r$FAT_MAX_HR, 0), " bpm, ",
        f(r$FAT_MAX_porcHRmax), "% HRmax\n", sep = "")
    cat("  Fatmax zone: ", f(r$zone_lower), "-", f(r$zone_upper), "% VO2max, ",
        f(r$zone_lower_HR, 0), "-", f(r$zone_upper_HR, 0), " bpm, ",
        f(r$zone_lower_porcHRmax), "-", f(r$zone_upper_porcHRmax), "% HRmax\n", sep = "")
    cat("  HRmax:       ", f(r$HRmax, 0), " bpm (",
        if (is.na(r$HRmax_source)) "not available" else r$HRmax_source, ")\n", sep = "")
    if (!is.na(r$obesity_zone) || identical(attr(x, "population"), "obesity")) {
      cat("  Obesity range (Chavez-Guevara et al. 2023): ", f(r$obesity_range_lower, 0),
          "-", f(r$obesity_range_upper, 0), "% HRpeak; zone ",
          if (is.na(r$obesity_zone)) "not available" else r$obesity_zone, "\n", sep = "")
    }
    cat("  Reference:   ", r$reference, " (n = ", r$reference_n, ")\n", sep = "")
    cat("    MFO ", f(r$MFO, 2), " g/min: ",
        if (is.na(r$MFO_percentile_label)) "no percentile" else r$MFO_percentile_label,
        "\n    Fatmax ", f(r$FAT_MAX), " % VO2max: ",
        if (is.na(r$Fatmax_percentile_label)) "no percentile" else r$Fatmax_percentile_label,
        "\n", sep = "")
    cat("    Differences: ", r$reference_differences, "\n", sep = "")
  }
  invisible(x)
}


# --- Internal helper: one row per result with the columns MFO_zones() needs,
# from an MFO() result or an MFOs() data frame ---
.zones_input <- function(x) {
  get <- function(obj, name, default = NA) {
    v <- obj[[name]]
    if (is.null(v)) rep(default, if (is.data.frame(obj)) nrow(obj) else 1) else v
  }
  if (is.data.frame(x) && "MFO_g_min" %in% names(x)) {
    data.frame(
      ID = x$ID,
      MFO = x$MFO_g_min,
      FAT_MAX = x$FAT_MAX_perVO2max,
      zone_lower = x$FatMax_zone_lower,
      zone_upper = x$FatMax_zone_upper,
      FAT_MAX_HR = as.numeric(get(x, "FAT_MAX_HR")),
      zone_lower_HR = as.numeric(get(x, "FatMax_zone_lower_HR")),
      zone_upper_HR = as.numeric(get(x, "FatMax_zone_upper_HR")),
      HRmax = as.numeric(get(x, "HRmax")),
      HR_peak = as.numeric(get(x, "HR_peak")),
      mode = as.character(get(x, "mode")),
      author = as.character(get(x, "author")),
      method_used = as.character(get(x, "method_used")),
      protocol_type = as.character(get(x, "protocol")),
      step_duration = NA_real_,
      steady_state_duration = NA_real_,
      stringsAsFactors = FALSE
    )
  } else if (is.list(x) && !is.null(x$MFO_db)) {
    p <- x$protocol
    one <- function(v) if (length(v) == 1) as.numeric(v) else NA_real_
    data.frame(
      MFO = x$MFO,
      FAT_MAX = x$FAT_MAX,
      zone_lower = as.numeric(x$FatMax_zone_lower),
      zone_upper = as.numeric(x$FatMax_zone_upper),
      FAT_MAX_HR = as.numeric(get(x, "FAT_MAX_HR")),
      zone_lower_HR = as.numeric(get(x, "FatMax_zone_lower_HR")),
      zone_upper_HR = as.numeric(get(x, "FatMax_zone_upper_HR")),
      HRmax = as.numeric(get(x, "HRmax")),
      HR_peak = as.numeric(get(x, "HR_peak")),
      mode = as.character(get(x, "mode")),
      author = as.character(get(x, "author")),
      method_used = as.character(get(x, "method_used")),
      protocol_type = as.character(if (is.null(p$type)) NA else p$type),
      step_duration = if (is.null(p)) NA_real_ else one(p$step_duration),
      steady_state_duration = if (is.null(p)) NA_real_ else one(p$steady_state_duration),
      stringsAsFactors = FALSE
    )
  } else {
    stop("x must be the result of MFO() or the data frame returned by MFOs().",
         call. = FALSE)
  }
}


# --- Internal helper: recycle an argument to one value per result ---
.zones_arg <- function(v, n, name, numeric = FALSE, choices = NULL) {
  if (is.null(v)) return(if (numeric) rep(NA_real_, n) else rep(NA_character_, n))
  if (!length(v) %in% c(1, n)) {
    stop(name, " must have length 1 or one value per result (", n, ").", call. = FALSE)
  }
  if (numeric && !is.numeric(v)) stop(name, " must be numeric.", call. = FALSE)
  if (!is.null(choices) && any(!is.na(v) & !v %in% choices)) {
    stop(name, " must be one of \"", paste(choices, collapse = "\", \""), "\".",
         call. = FALSE)
  }
  rep(v, length.out = n)
}


# --- Internal helper: Table 1 row for the combination of groups it prints ---
.reference_row <- function(ref, sex, age_group, weight_status) {
  if (!is.na(weight_status)) {
    i <- which(ref$weight_status %in% weight_status)
  } else {
    i <- which(ref$grouping != "By BMI Status" &
               (if (is.na(sex)) is.na(ref$sex) else ref$sex %in% sex) &
               (if (is.na(age_group)) is.na(ref$age_group) else ref$age_group %in% age_group))
  }
  stopifnot(length(i) == 1)
  i
}


# --- Internal helper: percentile by linear interpolation between P20-P80,
# "< P20" / "> P80" outside, no extrapolation (SPEC 15.3) ---
.reference_percentile <- function(value, q) {
  q <- as.matrix(q)
  p <- c(20, 40, 60, 80)
  percentile <- rep(NA_real_, length(value))
  label <- rep(NA_character_, length(value))
  for (i in seq_along(value)) {
    v <- value[i]
    if (is.na(v)) next
    if (v < q[i, 1]) {
      label[i] <- "< P20"
    } else if (v > q[i, 4]) {
      label[i] <- "> P80"
    } else {
      percentile[i] <- stats::approx(q[i, ], p, xout = v, ties = mean)$y
      label[i] <- sprintf("P%.0f", percentile[i])
    }
  }
  list(percentile = percentile, label = label)
}


# --- Internal helper: zone (% HRmax) against a range ---
.zone_overlap <- function(lower, upper, range_lower, range_upper) {
  ifelse(is.na(lower) | is.na(upper), NA_character_,
         ifelse(lower >= range_lower & upper <= range_upper, "inside",
                ifelse(upper < range_lower | lower > range_upper, "outside",
                       "partly overlaps")))
}


# --- Internal helper: how a result differs from the Amaro-Gahete et al.
# (2018) test (SPEC 15.2) ---
.reference_differences <- function(d) {
  out <- character(0)
  if (d$mode %in% "running") {
    out <- c(out, "mode: running (reference: treadmill walking)")
  } else if (!d$mode %in% "walking") {
    out <- c(out, paste0("mode: ", d$mode, " (reference: treadmill walking, no percentile)"))
  }
  if (is.na(d$author)) {
    out <- c(out, "equation: not recorded (reference: Frayn without protein)")
  } else if (d$author != "Frayn") {
    out <- c(out, paste0("equation: ", d$author, " (reference: Frayn without protein)"))
  }
  ref_protocol <- "3-min steps, last 60 s"
  if (!d$protocol_type %in% "step") {
    out <- c(out, paste0("protocol: ", d$protocol_type, " (reference: ", ref_protocol, ")"))
  } else if ((!is.na(d$step_duration) && d$step_duration != 180) ||
             (!is.na(d$steady_state_duration) && d$steady_state_duration != 60)) {
    out <- c(out, paste0("protocol: ", d$step_duration, "-s steps, last ",
                         d$steady_state_duration, " s (reference: ", ref_protocol, ")"))
  }
  out <- c(out, paste0("method: ", d$method_used,
                       " (reference: third-degree polynomial through (0;0), Croci et al. 2014)"),
           "fasting: reference 5-6 h, not overnight")
  paste(out, collapse = "; ")
}
