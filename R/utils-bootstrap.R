# Bootstrap of the curve fit (SPEC 13.1). The model is refitted to fitted
# values plus new errors. "bootstrap": the residuals resampled with
# replacement, modified, r / sqrt(1 - h), and centred, so that their spread
# matches that of the errors (Davison & Hinkley 1997, ch. 6); for a linear fit
# h is the diagonal of the hat matrix X (X'X)^-1 X'. "parametric": for each
# replicate sigma^2 is drawn from Inv-chi^2(n - p, s^2), s^2 = RSS / (n - p),
# and then normal errors with that sigma: the posterior of normal regression
# with a flat prior (Gelman et al. 2013, BDA 3rd ed., ch. 14, 14.2), i.e. a
# parametric bootstrap that carries the uncertainty of sigma.

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
.bootstrap_fit <- function(MFO_db, method, B, type = "bootstrap") {
  fit_fn <- switch(method, P3 = .fit_P3, P2 = .fit_P2, MRER = .fit_MRER,
                   stop('ci = "bootstrap" is not available for method "', method,
                        '": it fits no model. Use "P3", "P2" or "MRER".', call. = FALSE))
  x <- MFO_db$porc_VO2
  if (method == "MRER") {
    # Two regressions of %VO2max: 1 - RER and VO2 (Cheneviere et al. 2009)
    mod_rer <- stats::lm(I(1 - RER) ~ porc_VO2, data = MFO_db)
    mod_vo2 <- stats::lm(VO2 ~ porc_VO2, data = MFO_db)
    e_rer <- .error_generator(mod_rer, type)
    e_vo2 <- .error_generator(mod_vo2, type)
    make <- function() {
      db <- MFO_db
      db$RER <- 1 - (stats::fitted(mod_rer) + e_rer())
      db$VO2 <- stats::fitted(mod_vo2) + e_vo2()
      db
    }
  } else {
    form <- if (method == "P3") {
      FAT ~ porc_VO2 + I(porc_VO2^2) + I(porc_VO2^3)
    } else {
      FAT ~ porc_VO2 + I(porc_VO2^2)
    }
    mod <- stats::lm(form, data = MFO_db)
    e <- .error_generator(mod, type)
    make <- function() {
      db <- MFO_db
      db$FAT <- stats::fitted(mod) + e()
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

# Function drawing one set of errors for a fitted linear model
.error_generator <- function(mod, type) {
  n <- length(stats::residuals(mod))
  if (type == "parametric") {
    if (stats::df.residual(mod) < 1) {
      stop("The parametric bootstrap needs more steps than model parameters.",
           call. = FALSE)
    }
    df <- stats::df.residual(mod)
    s2 <- sum(stats::residuals(mod)^2) / df
    # sigma^2 ~ Inv-chi^2(df, s2), i.e. df s2 / chi^2_df
    return(function() stats::rnorm(n, 0, sqrt(df * s2 / stats::rchisq(1, df))))
  }
  m <- .modified_residuals(mod)
  function() sample(m, replace = TRUE)
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
