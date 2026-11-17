#' Simulate repeated MFO tests with a known truth
#'
#' Generates synthetic MFO tests (resting measure, MFO test and graded test)
#' for several participants and sessions, with known true MFO, Fatmax and
#' SIN parameters. It is meant for testing the reliability (SPEC 12) and
#' uncertainty (SPEC 13) modules and for demonstrations, not as a model of
#' physiology.
#'
#' **How a test is built.** Each participant has a true VO2max, MFO and curve
#' shape. Fat oxidation follows the SIN model of Cheneviere et al. (2009,
#' \doi{10.1249/MSS.0b013e31819e2f91}), FAT = MFO x SIN(x; d, s, t), truncated
#' at 0, with x the step intensity (\% VO2max); t is set so that the maximum
#' falls at the participant's Fatmax. Each step has VO2 = x / 100 x VO2max and
#' the VCO2 for which the `author` equation gives that FAT, so [MFO()] recovers
#' the curve exactly when there is no noise. VO2 is constant within a step (no
#' on-kinetics). Steps are cut as in `protocol`, which also gives the load
#' schedule (`Load` column only when `load_start` and `load_increment` are set).
#'
#' **Variability.** Three levels:
#' \itemize{
#' \item between participants: MFO (log-normal), Fatmax (normal, truncated to
#'   `intensity`) and VO2max (normal, positive) from `population`. The MFO and
#'   Fatmax defaults are the sedentary adults of Amaro-Gahete, Sanchez-Delgado
#'   and Ruiz (2018): 0.34 +/- 0.10 g/min and 44.2 +/- 12.4 \% VO2max. VO2max has
#'   no default and must be given.
#' \item between sessions: each session multiplies the participant's MFO and
#'   Fatmax by log-normal factors with coefficients of variation `cv_MFO` and
#'   `cv_Fatmax`. The defaults, 21 \% and 26 \%, are the total test-retest CVs
#'   reported by Chrzanowski-Smith et al. (2020, n = 99), which include
#'   biological, analyser and fitting error; used here as day-to-day
#'   variation, and with analyser noise on top, they make the simulated
#'   test-retest variability an upper bound. For Fatmax, Croci et al. (2014,
#'   n = 15) report CVs of 16.4 \% (SIN), 18.6 \% (P3) and 20.8 \% (MV).
#'   `familiarization` shifts session 1 only (a relative bias).
#' \item within a session: every record of VO2 and VCO2 is multiplied by
#'   (1 + e), with e normal, standard deviation `noise_cv` and correlation
#'   `noise_cor` between the two gases. There is no recommended value: the
#'   default 0 gives noiseless tests, and studies of the fit uncertainty set
#'   it explicitly as a design parameter.
#' }
#'
#' **Resting and graded tests.** Conveniences for [MFO()], not physiological
#' claims: the resting measure lasts 15 min at 10 \% VO2max with RER 0.80; the
#' graded test has 60-s stages from 50 to 100 \% VO2max with RER rising
#' linearly from 0.90 to 1.15, so the last stage gives VO2max exactly. Heart
#' rate rises linearly with \% VO2max from `hr_rest` to the predicted HRmax of
#' Tanaka, Monahan and Seals (2001, \doi{10.1016/s0735-1097(00)01054-8}),
#' 208 - 0.7 x `age`.
#'
#' @param n_participants number of participants.
#' @param n_sessions number of sessions per participant.
#' @param population list with `VO2max` (mean and SD, mL/min; required) and
#'   optionally `MFO` (g/min) and `Fatmax` (\% VO2max), each `c(mean, sd)`. An
#'   SD of 0 gives the same value to every participant.
#' @param shape `"basic"` (d = 0, s = 1; default), `"trained"` (d = 0.18,
#'   s = 1.52, the trained participant of Cheneviere et al. 2009, Fig. 5), or a
#'   list with `d` and `s`, each one number or one per participant.
#' @param cv_MFO,cv_Fatmax between-session coefficients of variation
#'   (proportions; defaults 0.21 and 0.26).
#' @param familiarization relative bias of session 1, one number for MFO and
#'   Fatmax or `c(MFO = , Fatmax = )` (default 0).
#' @param noise_cv,noise_cor analyser noise per record: coefficient of
#'   variation of VO2 and VCO2 and their correlation (defaults 0).
#' @param protocol an [MFO_protocol()] of type `"step"`, `"step_variable"` or
#'   `"relative"`.
#' @param n_steps number of exercise steps (default 8).
#' @param intensity \% VO2max of the first and last step (default 20 and 75).
#' @param step_time record interval in seconds (default 20); step durations
#'   and the averaged segment must be multiples of it.
#' @param author equation whose inverse gives VCO2: "Frayn" (default),
#'   "Jeukendrup" or "Peronnet". Analyse the data with the same `author` to
#'   recover the truth.
#' @param hr_rest,age resting heart rate (bpm) and age (years) for the
#'   heart-rate line (defaults 60 and 30).
#' @param seed optional integer for reproducible results; the random state
#'   of the session is restored afterwards.
#'
#' @return An object of class `MFO_sim`: a list with
#'   \itemize{
#'   \item `data`: `data$P01$S1` is a list with `db_basal`, `db_MFO` and
#'     `db_graded`, ready for [MFO()] with `step_time`;
#'   \item `truth`: one row per participant and session with `id`, `session`,
#'     `VO2max`, `MFO`, `Fatmax`, `d`, `s`, `t` and `Fatmin` (second zero of
#'     the SIN curve);
#'   \item `participants`: the participants' values before the between-session
#'     variation;
#'   \item `settings`: the arguments, including `step_time` and `protocol`.
#'   }
#'
#' @references
#' Amaro-Gahete FJ, Sanchez-Delgado G, Ruiz JR (2018). Commentary:
#' Contextualising maximal fat oxidation during exercise: determinants and
#' normative values. Front Physiol 9: 1460, Table 1 (sedentary adults).
#' \doi{10.3389/fphys.2018.01460}
#'
#' Chrzanowski-Smith OJ, Edinburgh RM, Thomas MP, et al. (2020). The
#' day-to-day reliability of peak fat oxidation and FATMAX. Eur J Appl
#' Physiol 120: 1745-1759. \doi{10.1007/s00421-020-04397-3}
#'
#' Croci I, Borrani F, Byrne NM, et al. (2014). Reproducibility of Fatmax and
#' fat oxidation rates during exercise in recreationally trained males. PLoS
#' ONE 9: e97930. \doi{10.1371/journal.pone.0097930}
#'
#' @seealso [write_MFO_sessions()], [MFO()], [MFO_kinetics()]
#'
#' @export
#'
#' @examples
#' sim <- simulate_MFO_sessions(n_participants = 3, n_sessions = 2,
#'                              population = list(VO2max = c(3000, 500)),
#'                              seed = 1)
#' sim
#' sim$truth
#' s1 <- sim$data$P01$S1
#' r <- MFO(step_time = sim$settings$step_time, db_MFO = s1$db_MFO,
#'          db_basal = s1$db_basal, db_graded = s1$db_graded, cv_var = "RER",
#'          author = "Frayn", energy_equivalents = "classic", age = 30)
#' c(estimated = r$FAT_MAX, true = sim$truth$Fatmax[1])
simulate_MFO_sessions <- function(n_participants = 10,
                                  n_sessions = 2,
                                  population,
                                  shape = "basic",
                                  cv_MFO = 0.21,
                                  cv_Fatmax = 0.26,
                                  familiarization = 0,
                                  noise_cv = 0,
                                  noise_cor = 0,
                                  protocol = MFO_protocol(),
                                  n_steps = 8,
                                  intensity = c(20, 75),
                                  step_time = 20,
                                  author = c("Frayn", "Jeukendrup", "Peronnet"),
                                  hr_rest = 60,
                                  age = 30,
                                  seed = NULL) {

  author <- match.arg(author)
  .check_count(n_participants, "n_participants")
  .check_count(n_sessions, "n_sessions")
  .check_count(n_steps, "n_steps")
  if (!inherits(protocol, "MFO_protocol") ||
      !protocol$type %in% c("step", "step_variable", "relative")) {
    stop('protocol must be an MFO_protocol() of type "step", "step_variable" or ',
         '"relative".', call. = FALSE)
  }
  if (!is.numeric(intensity) || length(intensity) != 2 || anyNA(intensity) ||
      intensity[1] <= 0 || intensity[2] <= intensity[1] || intensity[2] >= 100) {
    stop("intensity must be two increasing numbers between 0 and 100 (% VO2max).",
         call. = FALSE)
  }
  for (nm in c("cv_MFO", "cv_Fatmax", "noise_cv")) {
    v <- get(nm)
    if (!is.numeric(v) || length(v) != 1 || is.na(v) || v < 0) {
      stop(nm, " must be a single number >= 0.", call. = FALSE)
    }
  }
  if (!is.numeric(noise_cor) || length(noise_cor) != 1 || is.na(noise_cor) ||
      abs(noise_cor) > 1) {
    stop("noise_cor must be a single number between -1 and 1.", call. = FALSE)
  }
  fam <- .resolve_familiarization(familiarization)
  pop <- .resolve_population(population)
  shp <- .resolve_shape(shape, n_participants)
  durations <- .sim_durations(protocol, n_steps, step_time)

  if (!is.null(seed)) {
    old <- if (exists(".Random.seed", envir = globalenv())) {
      get(".Random.seed", envir = globalenv())
    }
    on.exit({
      if (is.null(old)) {
        rm(".Random.seed", envir = globalenv())
      } else {
        assign(".Random.seed", old, envir = globalenv())
      }
    }, add = TRUE)
    set.seed(seed)
  }

  # Participants
  ids <- sprintf(paste0("P%0", max(2, nchar(n_participants)), "d"), seq_len(n_participants))
  participants <- data.frame(
    id = ids,
    VO2max = .rtrunc_normal(n_participants, pop$VO2max, lower = 0, upper = Inf),
    MFO = .rlnorm_mean_sd(n_participants, pop$MFO),
    Fatmax = .rtrunc_normal(n_participants, pop$Fatmax, intensity[1], intensity[2]),
    d = shp$d, s = shp$s,
    stringsAsFactors = FALSE)

  # Sessions: log-normal day-to-day factors with mean 1; session 1 shifted
  pct <- seq(intensity[1], intensity[2], length.out = n_steps)
  sessions <- paste0("S", seq_len(n_sessions))
  data <- list()
  truth <- list()
  for (p in seq_len(n_participants)) {
    data[[ids[p]]] <- list()
    for (k in seq_len(n_sessions)) {
      bias <- if (k == 1) fam else c(MFO = 0, Fatmax = 0)
      MFO_k <- participants$MFO[p] * .lnorm_factor(cv_MFO) * (1 + bias[["MFO"]])
      Fatmax_k <- participants$Fatmax[p] * .lnorm_factor(cv_Fatmax) * (1 + bias[["Fatmax"]])
      d <- participants$d[p]
      s <- participants$s[p]
      t <- (pi + 2 * d) * 2^(-1 / s) - d - (pi / 100) * Fatmax_k
      truth[[length(truth) + 1]] <- data.frame(
        id = ids[p], session = sessions[k], VO2max = participants$VO2max[p],
        MFO = MFO_k, Fatmax = .sin_fatmax(d, s, t), d = d, s = s, t = t,
        Fatmin = .sin_fatmin(d, s, t), stringsAsFactors = FALSE)
      data[[ids[p]]][[sessions[k]]] <- .sim_session(
        VO2max = participants$VO2max[p], MFO = MFO_k, d = d, s = s, t = t,
        pct = pct, durations = durations, protocol = protocol,
        step_time = step_time, author = author, noise_cv = noise_cv,
        noise_cor = noise_cor, hr_rest = hr_rest, age = age)
    }
  }
  truth <- do.call(rbind, truth)
  rownames(truth) <- NULL

  structure(list(data = data, truth = truth, participants = participants,
                 settings = list(n_participants = n_participants,
                                 n_sessions = n_sessions, population = pop,
                                 shape = shape, cv_MFO = cv_MFO,
                                 cv_Fatmax = cv_Fatmax, familiarization = fam,
                                 noise_cv = noise_cv, noise_cor = noise_cor,
                                 protocol = protocol, n_steps = n_steps,
                                 intensity = intensity, step_time = step_time,
                                 author = author, hr_rest = hr_rest, age = age,
                                 seed = seed)),
            class = "MFO_sim")
}


#' @export
print.MFO_sim <- function(x, ...) {
  st <- x$settings
  cat(sprintf("<MFO_sim> %d participant(s) x %d session(s), %d steps of %s, %g-s records (%s)\n",
              st$n_participants, st$n_sessions, st$n_steps, st$protocol$type,
              st$step_time, st$author))
  cat(sprintf("  between sessions: CV MFO %g, CV Fatmax %g; familiarization %s\n",
              st$cv_MFO, st$cv_Fatmax,
              paste(names(st$familiarization), st$familiarization, sep = " ", collapse = ", ")))
  cat(sprintf("  analyser noise: CV %g, correlation %g\n", st$noise_cv, st$noise_cor))
  cat(sprintf("  true MFO %.3f-%.3f g/min, Fatmax %.1f-%.1f %% VO2max\n",
              min(x$truth$MFO), max(x$truth$MFO), min(x$truth$Fatmax),
              max(x$truth$Fatmax)))
  invisible(x)
}


#' Write simulated sessions as folders for MFOs()
#'
#' Writes each participant and session of a [simulate_MFO_sessions()] result
#' as a folder `<id>_<session>` (e.g. `P01_S1`) with three xlsx files, the
#' layout that `MFOs(from = "folder")` reads. The participant ID in the
#' `MFOs()` summary is then the folder name. A file `sessions.csv` in `path`
#' maps each folder name (`id`) to its `participant` and `session`; `MFOs()`
#' ignores it, as it reads only folders.
#'
#' @param sim result of [simulate_MFO_sessions()].
#' @param path folder where the participant folders are written (created if
#'   needed).
#' @param db_basal_name,db_MFO_name,db_graded_name file names without the
#'   extension (defaults as the example data).
#' @param overwrite logical; if `FALSE` (default), existing participant
#'   folders stop the function.
#'
#' @return The folders written, invisibly.
#'
#' @seealso [simulate_MFO_sessions()], [MFOs()]
#'
#' @export
#'
#' @examples
#' sim <- simulate_MFO_sessions(2, 2, population = list(VO2max = c(3000, 0)),
#'                              seed = 1)
#' out <- file.path(tempdir(), "sim")
#' write_MFO_sessions(sim, out)
#' list.files(out)
write_MFO_sessions <- function(sim, path,
                               db_basal_name = "basal_df",
                               db_MFO_name = "MFO_df",
                               db_graded_name = "VO2max_df",
                               overwrite = FALSE) {
  if (!inherits(sim, "MFO_sim")) {
    stop("sim must be a result of simulate_MFO_sessions().", call. = FALSE)
  }
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  folders <- character(0)
  for (id in names(sim$data)) {
    for (k in names(sim$data[[id]])) {
      folder <- file.path(path, paste0(id, "_", k))
      if (dir.exists(folder) && !overwrite) {
        stop("Folder exists: ", folder, " (use overwrite = TRUE).", call. = FALSE)
      }
      dir.create(folder, showWarnings = FALSE)
      tabs <- sim$data[[id]][[k]]
      openxlsx::write.xlsx(tabs$db_basal, file.path(folder, paste0(db_basal_name, ".xlsx")))
      openxlsx::write.xlsx(tabs$db_MFO, file.path(folder, paste0(db_MFO_name, ".xlsx")))
      openxlsx::write.xlsx(tabs$db_graded, file.path(folder, paste0(db_graded_name, ".xlsx")))
      folders <- c(folders, folder)
    }
  }
  index <- data.frame(id = basename(folders),
                      participant = rep(names(sim$data), lengths(sim$data)),
                      session = unlist(lapply(sim$data, names), use.names = FALSE),
                      stringsAsFactors = FALSE)
  utils::write.csv(index, file.path(path, "sessions.csv"), row.names = FALSE)
  invisible(folders)
}


# --- Internal helpers -------------------------------------------------------

.check_count <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < 1 || x != round(x)) {
    stop(name, " must be a whole number >= 1.", call. = FALSE)
  }
}

.resolve_familiarization <- function(f) {
  if (!is.numeric(f) || anyNA(f) || any(f <= -1)) {
    stop("familiarization must be relative bias(es) above -1.", call. = FALSE)
  }
  if (length(f) == 1 && is.null(names(f))) return(c(MFO = f, Fatmax = f))
  if (length(f) == 2 && setequal(names(f), c("MFO", "Fatmax"))) return(f[c("MFO", "Fatmax")])
  stop("familiarization must be one number or c(MFO = , Fatmax = ).", call. = FALSE)
}

.resolve_population <- function(population) {
  if (missing(population) || is.null(population$VO2max)) {
    stop("population must give VO2max = c(mean, sd) in mL/min; there is no default.",
         call. = FALSE)
  }
  # Sedentary adults, Amaro-Gahete, Sanchez-Delgado & Ruiz (2018), Table 1
  pop <- list(MFO = c(0.34, 0.10), Fatmax = c(44.2, 12.4))
  pop[names(population)] <- population
  for (nm in c("VO2max", "MFO", "Fatmax")) {
    v <- pop[[nm]]
    if (!is.numeric(v) || length(v) != 2 || anyNA(v) || v[1] <= 0 || v[2] < 0) {
      stop("population$", nm, " must be c(mean, sd) with mean > 0 and sd >= 0.",
           call. = FALSE)
    }
  }
  pop[c("VO2max", "MFO", "Fatmax")]
}

.resolve_shape <- function(shape, n) {
  if (is.character(shape)) {
    shape <- switch(match.arg(shape, c("basic", "trained")),
                    basic = list(d = 0, s = 1),
                    # Cheneviere et al. (2009), Fig. 5, trained participant
                    trained = list(d = 0.18, s = 1.52))
  }
  if (!is.list(shape) || !all(c("d", "s") %in% names(shape))) {
    stop('shape must be "basic", "trained" or a list with d and s.', call. = FALSE)
  }
  ok <- function(x) is.numeric(x) && !anyNA(x) && length(x) %in% c(1, n)
  if (!ok(shape$d) || !ok(shape$s) || any(shape$s <= 0)) {
    stop("shape d and s must be one number or one per participant; s > 0.", call. = FALSE)
  }
  list(d = rep_len(shape$d, n), s = rep_len(shape$s, n))
}

# Step durations from the protocol (step_variable: last one repeated)
.sim_durations <- function(protocol, n_steps, step_time) {
  dur <- protocol$step_duration
  dur <- c(dur, rep(dur[length(dur)], max(0, n_steps - length(dur))))[seq_len(n_steps)]
  ss <- rep_len(protocol$steady_state_duration, length(protocol$step_duration))
  if (any(abs(c(dur, ss) / step_time - round(c(dur, ss) / step_time)) > 1e-8)) {
    stop("step durations and steady_state_duration must be multiples of step_time.",
         call. = FALSE)
  }
  dur
}

.rlnorm_mean_sd <- function(n, mean_sd) {
  if (mean_sd[2] == 0) return(rep(mean_sd[1], n))
  sigma2 <- log(1 + (mean_sd[2] / mean_sd[1])^2)
  stats::rlnorm(n, log(mean_sd[1]) - sigma2 / 2, sqrt(sigma2))
}

# Normal draws truncated to [lower, upper] by rejection
.rtrunc_normal <- function(n, mean_sd, lower, upper) {
  if (mean_sd[2] == 0) {
    if (mean_sd[1] < lower || mean_sd[1] > upper) {
      stop(sprintf("A fixed value of %g is outside [%g, %g].", mean_sd[1], lower, upper),
           call. = FALSE)
    }
    return(rep(mean_sd[1], n))
  }
  out <- numeric(0)
  for (i in 1:1000) {
    x <- stats::rnorm(n, mean_sd[1], mean_sd[2])
    out <- c(out, x[x >= lower & x <= upper])
    if (length(out) >= n) return(out[seq_len(n)])
  }
  stop("Could not draw values inside [", lower, ", ", upper, "]; check population.",
       call. = FALSE)
}

# Multiplicative log-normal factor with mean 1 and coefficient of variation cv
.lnorm_factor <- function(cv) {
  if (cv == 0) return(1)
  sigma2 <- log(1 + cv^2)
  stats::rlnorm(1, -sigma2 / 2, sqrt(sigma2))
}

# True fat oxidation (g/min) at x % VO2max: MFO x SIN, 0 outside its two zeros
.sim_fat <- function(x, MFO, d, s, t) {
  u <- pi^(1 / s) / (pi + 2 * d) * ((pi / 100) * x + d + t)
  inside <- u > 0 & u^s < pi
  out <- numeric(length(x))
  out[inside] <- MFO * sin(u[inside]^s)
  out
}

# VCO2 (mL/min) for which the author equation gives FAT (g/min) at VO2:
# FAT is linear in VCO2, so two evaluations give the exact inverse
.sim_vco2 <- function(VO2, FAT, author, VO2max) {
  f0 <- substrate_oxidation(VO2, 0 * VO2, author, VO2max = VO2max)$FAT
  f1 <- substrate_oxidation(VO2, 0 * VO2 + 1000, author, VO2max = VO2max)$FAT
  (FAT - f0) / (f1 - f0) * 1000
}

# Records of one table: VO2/VCO2 with correlated proportional noise
.sim_records <- function(VO2, VCO2, HR, Load, step_time, noise_cv, noise_cor) {
  n <- length(VO2)
  if (noise_cv > 0) {
    e1 <- stats::rnorm(n)
    e2 <- noise_cor * e1 + sqrt(1 - noise_cor^2) * stats::rnorm(n)
    VO2 <- VO2 * (1 + noise_cv * e1)
    VCO2 <- VCO2 * (1 + noise_cv * e2)
  }
  db <- data.frame(Time = seq_len(n) * step_time, HR = HR)
  if (!is.null(Load)) db$Load <- Load
  db$VO2 <- VO2
  db$VCO2 <- VCO2
  db$RER <- VCO2 / VO2
  db
}

.sim_session <- function(VO2max, MFO, d, s, t, pct, durations, protocol, step_time,
                         author, noise_cv, noise_cor, hr_rest, age) {
  HRmax <- 208 - 0.7 * age
  hr <- function(p) hr_rest + (HRmax - hr_rest) * p / 100
  reps <- as.integer(round(durations / step_time))

  # MFO test
  VO2_step <- pct / 100 * VO2max
  VCO2_step <- .sim_vco2(VO2_step, .sim_fat(pct, MFO, d, s, t), author, VO2max)
  load_step <- if (!is.null(protocol$load_start)) {
    protocol$load_start + (seq_along(pct) - 1) * protocol$load_increment
  }
  db_MFO <- .sim_records(rep(VO2_step, reps), rep(VCO2_step, reps), rep(hr(pct), reps),
                         if (!is.null(load_step)) rep(load_step, reps),
                         step_time, noise_cv, noise_cor)

  # Resting measure: 15 min at 10 % VO2max, RER 0.80
  n_rest <- as.integer(round(900 / step_time))
  db_basal <- .sim_records(rep(0.10 * VO2max, n_rest), rep(0.08 * VO2max, n_rest),
                           rep(hr_rest, n_rest), NULL, step_time, noise_cv, noise_cor)

  # Graded test: 60-s stages from 50 to 100 % VO2max, RER 0.90 -> 1.15
  g_pct <- seq(50, 100, by = 10)
  g_rer <- seq(0.90, 1.15, length.out = length(g_pct))
  g_reps <- max(1L, as.integer(round(60 / step_time)))
  g_load <- if (!is.null(load_step)) {
    load_step[length(load_step)] + seq_along(g_pct) * protocol$load_increment
  }
  db_graded <- .sim_records(rep(g_pct / 100 * VO2max, each = g_reps),
                            rep(g_rer * g_pct / 100 * VO2max, each = g_reps),
                            rep(hr(g_pct), each = g_reps),
                            if (!is.null(g_load)) rep(g_load, each = g_reps),
                            step_time, noise_cv, noise_cor)

  list(db_basal = db_basal, db_MFO = db_MFO, db_graded = db_graded)
}
