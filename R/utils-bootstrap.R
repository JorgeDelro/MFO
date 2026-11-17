# Residual bootstrap of the curve fit (SPEC 13.1). The model is refitted to
# fitted values plus resampled residuals; the residuals are modified,
# r / sqrt(1 - h) and centred, so that their spread matches that of the errors
# (Davison & Hinkley 1997, ch. 6). For a linear fit h is the diagonal of the
# hat matrix X (X'X)^-1 X'.

# Modified, centred residuals of a linear model
.modified_residuals <- function(mod) {
  h <- stats::hatvalues(mod)
  if (any(h > 1 - 1e-8)) {
    stop("The bootstrap needs more steps than model parameters (leverage 1).",
         call. = FALSE)
  }
  m <- stats::residuals(mod) / sqrt(1 - h)
  m - mean(m)
}

# B bootstrap replicates of the fit of `method` to MFO_db. Returns a matrix
# with one row per replicate that kept the method (fallbacks such as P3 -> P2
# are dropped and counted) and columns MFO, FAT_MAX, FatMax_zone_lower,
# FatMax_zone_upper, FAT_MAX_load.
.bootstrap_fit <- function(MFO_db, method, B) {
  fit_fn <- switch(method, P3 = .fit_P3, P2 = .fit_P2, MRER = .fit_MRER,
                   stop('ci = "bootstrap" is not available for method "', method,
                        '": it fits no model. Use "P3", "P2" or "MRER".', call. = FALSE))
  x <- MFO_db$porc_VO2
  if (method == "MRER") {
    # Two regressions of %VO2max: 1 - RER and VO2 (Cheneviere et al. 2009)
    mod_rer <- stats::lm(I(1 - RER) ~ porc_VO2, data = MFO_db)
    mod_vo2 <- stats::lm(VO2 ~ porc_VO2, data = MFO_db)
    m_rer <- .modified_residuals(mod_rer)
    m_vo2 <- .modified_residuals(mod_vo2)
    make <- function() {
      db <- MFO_db
      db$RER <- 1 - (stats::fitted(mod_rer) + sample(m_rer, replace = TRUE))
      db$VO2 <- stats::fitted(mod_vo2) + sample(m_vo2, replace = TRUE)
      db
    }
  } else {
    form <- if (method == "P3") {
      FAT ~ porc_VO2 + I(porc_VO2^2) + I(porc_VO2^3)
    } else {
      FAT ~ porc_VO2 + I(porc_VO2^2)
    }
    mod <- stats::lm(form, data = MFO_db)
    m <- .modified_residuals(mod)
    make <- function() {
      db <- MFO_db
      db$FAT <- stats::fitted(mod) + sample(m, replace = TRUE)
      db
    }
  }

  out <- matrix(NA_real_, B, 5, dimnames = list(NULL, c(
    "MFO", "FAT_MAX", "FatMax_zone_lower", "FatMax_zone_upper", "FAT_MAX_load")))
  kept <- logical(B)
  for (b in seq_len(B)) {
    db <- make()
    r <- suppressWarnings(fit_fn(db))
    if (r$method != method) next
    zone <- .calculate_fatmax_zone(r$fat_predict_fn, r$MFO, r$FAT_MAX, db)
    out[b, ] <- c(r$MFO, r$FAT_MAX, zone$lower, zone$upper, .load_at(MFO_db, r$FAT_MAX))
    kept[b] <- TRUE
  }
  out[kept, , drop = FALSE]
}

# Percentile intervals of the replicates
.percentile_ci <- function(reps, conf) {
  a <- (1 - conf) / 2
  t(apply(reps, 2, function(v) {
    if (all(is.na(v))) return(c(NA_real_, NA_real_))
    stats::quantile(v, c(a, 1 - a), na.rm = TRUE, names = FALSE)
  }))
}

# Run expr with a seed and restore the random state afterwards
.with_seed <- function(seed, expr) {
  if (is.null(seed)) return(expr)
  old <- if (exists(".Random.seed", envir = globalenv())) {
    get(".Random.seed", envir = globalenv())
  }
  on.exit({
    if (is.null(old)) {
      rm(".Random.seed", envir = globalenv())
    } else {
      assign(".Random.seed", old, envir = globalenv())
    }
  })
  set.seed(seed)
  expr
}
