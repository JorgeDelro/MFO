# Internal pipeline shared by the importers (SPEC 16): breath-by-breath data
# (or records already averaged by the analyser) in, the internal data
# contract out. read_cart() feeds it from spiro or, with device = "generic",
# from a mapped csv/xlsx (R/utils-generic.R).

# Columns of the internal contract: required, then optional in this order
.contract_required <- c("Time", "VO2", "VCO2", "RER")
.contract_optional <- c("HR", "Load", "VE")


# VO2 and VCO2 in mL/min. "auto" accepts only a median VO2 above 100, which
# can only be mL/min; at or below 100 the unit must be given, because L/min
# and mL/kg/min cannot be told apart reliably (Jorge, 2026-11-17).
.to_ml_min <- function(breaths, vo2_unit, body_mass) {
  med <- stats::median(breaths$VO2, na.rm = TRUE)
  if (vo2_unit == "auto") {
    if (is.na(med) || med <= 100) {
      stop(sprintf(paste0("The median VO2 is %s: set vo2_unit to \"L/min\" or ",
                          "\"mL/kg/min\" (with body_mass); only values above 100 ",
                          "are read as mL/min automatically."), format(signif(med, 4))),
           call. = FALSE)
    }
    vo2_unit <- "mL/min"
  }
  factor <- switch(vo2_unit,
    "mL/min" = 1,
    "L/min" = 1000,
    "mL/kg/min" = {
      if (is.null(body_mass) || !is.numeric(body_mass) || length(body_mass) != 1 ||
          is.na(body_mass) || body_mass <= 0) {
        stop("vo2_unit = \"mL/kg/min\" needs body_mass (kg).", call. = FALSE)
      }
      body_mass
    })
  breaths$VO2 <- breaths$VO2 * factor
  breaths$VCO2 <- breaths$VCO2 * factor
  list(breaths = breaths, unit = vo2_unit)
}


# Mean of the breaths in each interval of `interval` s, from time 0. Every
# interval is kept: one without breaths is a row of NA, so the records stay
# aligned in time (MFO() counts records). Time is the end of the interval.
.bin_breaths <- function(breaths, interval) {
  bin <- floor(breaths$Time / interval)
  n_bins <- max(bin) + 1
  cols <- setdiff(names(breaths), "Time")
  out <- data.frame(Time = seq_len(n_bins) * interval)
  for (col in cols) {
    m <- tapply(breaths[[col]], factor(bin, levels = 0:(n_bins - 1)),
                function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE))
    out[[col]] <- as.numeric(m)
  }
  out
}


# Breath-by-breath data frame (Time, VO2, VCO2 and optional RER, HR, Load, VE)
# -> MFO_cart object with one table per phase. interval = "none": the rows
# are records already averaged by the analyser, kept as they are, and the
# step is read from Time (Jorge, 2026-11-19)
.cart_from_breaths <- function(breaths, interval, cuts, phase, body_mass, vo2_unit,
                               device) {
  as_is <- identical(interval, "none")
  if (!as_is && (!is.numeric(interval) || length(interval) != 1 || is.na(interval) ||
                 interval <= 0)) {
    stop("interval must be a single positive number of seconds, or \"none\" for ",
         "records already averaged.", call. = FALSE)
  }
  missing_cols <- setdiff(c("Time", "VO2", "VCO2"), names(breaths))
  if (length(missing_cols) > 0) {
    stop("The data have no column(s) ", paste(missing_cols, collapse = ", "), ".",
         call. = FALSE)
  }
  keep <- intersect(c("Time", "VO2", "VCO2", "RER", .contract_optional), names(breaths))
  breaths <- as.data.frame(lapply(breaths[keep], function(x) suppressWarnings(as.numeric(x))))
  breaths <- breaths[!is.na(breaths$Time) & breaths$Time >= 0, , drop = FALSE]
  n_breaths <- nrow(breaths)
  if (n_breaths == 0) stop("The data have no breaths with a valid Time.", call. = FALSE)

  units <- .to_ml_min(breaths, vo2_unit, body_mass)
  if (as_is) {
    interval <- .regular_step(breaths$Time)
    binned <- units$breaths
    rownames(binned) <- NULL
  } else {
    binned <- .bin_breaths(units$breaths, interval)
  }

  # RER is always VCO2/VO2 of the interval means; an exported RER is compared
  # with it and intervals differing by more than 0.02 are counted (SPEC 16)
  rer <- binned$VCO2 / binned$VO2
  rer_mismatch <- NA_integer_
  if ("RER" %in% names(binned)) {
    rer_mismatch <- sum(abs(binned$RER - rer) > 0.02, na.rm = TRUE)
    if (rer_mismatch > 0) {
      .mfo_warning("rer_mismatch",
                   sprintf(paste0("The exported RER differs from VCO2/VO2 by more than 0.02 in ",
                                  "%d of %d intervals; RER is recomputed as VCO2/VO2."),
                           rer_mismatch, nrow(binned)), n = rer_mismatch, total = nrow(binned))
    }
  }
  binned$RER <- rer
  binned <- binned[c(.contract_required, intersect(.contract_optional, names(binned)))]
  empty <- sum(is.na(binned$VO2))

  phases <- .split_phases(binned, cuts, phase, interval)

  structure(list(db_basal = phases$basal,
                 db_MFO = phases$MFO,
                 db_graded = phases$graded,
                 step_time = interval,
                 info = list(device = device, interval = interval,
                             averaging = if (as_is) "none (records as exported)" else
                               "mean of breaths per interval",
                             vo2_unit = units$unit, cuts = cuts,
                             n_breaths = n_breaths, n_intervals = nrow(binned),
                             empty_intervals = empty, RER_mismatch = rer_mismatch)),
            class = "MFO_cart")
}


# Phases by the user's cut times (s): basal_end and/or MFO_end. Without cuts
# the whole recording is the phase named in `phase`. Each record covers
# (Time - interval, Time]; a cut falls on a record boundary, other values are
# rounded to the nearest one (with binned breaths the boundaries are the
# multiples of interval)
.split_phases <- function(binned, cuts, phase, interval) {
  out <- list(basal = NULL, MFO = NULL, graded = NULL)
  if (is.null(cuts)) {
    out[[phase]] <- binned
    return(out)
  }
  if (!is.numeric(cuts) || is.null(names(cuts)) ||
      !all(names(cuts) %in% c("basal_end", "MFO_end")) || anyDuplicated(names(cuts)) ||
      anyNA(cuts)) {
    stop("cuts must be a named number vector with basal_end and/or MFO_end (s).",
         call. = FALSE)
  }
  boundaries <- c(binned$Time[1] - interval, binned$Time)
  rec <- vapply(cuts, function(x) which.min(abs(boundaries - x)) - 1L, integer(1))
  at <- boundaries[rec + 1]
  if (!isTRUE(all.equal(as.numeric(at), as.numeric(cuts)))) {
    rounded <- paste(names(cuts), at, sep = " = ", collapse = ", ")
    .mfo_message("cuts_rounded",
                 sprintf("cuts rounded to the nearest record boundary (%g-s records): %s.",
                         interval, rounded), interval = interval, cuts = rounded)
  }
  bounds <- c(0, rec[intersect(c("basal_end", "MFO_end"), names(rec))], nrow(binned))
  if (any(diff(bounds) <= 0)) {
    stop("cuts must be increasing (basal_end < MFO_end) and inside the recording.",
         call. = FALSE)
  }
  segments <- lapply(seq_len(length(bounds) - 1), function(i) {
    binned[(bounds[i] + 1):bounds[i + 1], , drop = FALSE]
  })
  names_seg <- if (all(c("basal_end", "MFO_end") %in% names(cuts))) {
    c("basal", "MFO", "graded")
  } else if ("basal_end" %in% names(cuts)) {
    c("basal", "MFO")
  } else {
    c("MFO", "graded")
  }
  for (i in seq_along(segments)) {
    seg <- segments[[i]]
    rownames(seg) <- NULL
    out[[names_seg[i]]] <- seg
  }
  out
}


# Step of records already averaged by the analyser: Time must increase by the
# same amount from record to record (Jorge, 2026-11-19)
.regular_step <- function(time) {
  if (length(time) < 2) {
    stop("interval = \"none\" needs at least two records.", call. = FALSE)
  }
  d <- diff(time)
  # the most frequent step (the first one on ties)
  r <- round(d, 6)
  step <- as.numeric(names(which.max(table(factor(r, levels = unique(r))))))
  bad <- which(abs(d - step) > 1e-6 * max(1, step))
  if (step <= 0 || length(bad) > 0) {
    i <- if (length(bad) > 0) bad[1] else 1
    stop(sprintf(paste0("interval = \"none\" needs records at a regular step, but ",
                        "Time goes from %s to %s s (record %d to %d) while the usual ",
                        "step is %s s. Use a numeric interval for breath-by-breath data."),
                 format(time[i]), format(time[i + 1]), i, i + 1, format(step)),
         call. = FALSE)
  }
  step
}
