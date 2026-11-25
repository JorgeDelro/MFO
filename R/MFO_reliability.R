#' Test-retest reliability of MFO outcomes
#'
#' Computes the reliability of MFO, Fatmax and the other outcomes of [MFOs()]
#' from two or more identical tests per participant (SPEC 12): intraclass
#' correlation, standard error of measurement, minimal detectable change,
#' coefficients of variation, systematic bias and limits of agreement.
#'
#' **Participants and sessions.** `results` is the summary of [MFOs()], one
#' row per test. Each row is assigned to a participant and a session from, in
#' this order: columns `participant` and `session` of `results`; the table
#' `sessions` (columns `id`, `participant`, `session`, as the `sessions.csv`
#' written by [write_MFO_sessions()]), matched by `results$ID`; or the `ID`
#' split at its last underscore (`"P01_S1"` is participant `"P01"`, session
#' `"S1"`). Sessions are ordered by their trailing number when they all have
#' one (S1, S2, ..., S10), otherwise by first appearance. For each variable
#' only the participants with a value in every session are used, and a
#' warning gives how many were left out.
#'
#' **Statistics.** From the two-way ANOVA (participants x sessions) with mean
#' squares MSR (participants), MSC (sessions) and MSE (error), for n
#' participants and k sessions:
#' \itemize{
#' \item ICC(2,1), two-way random effects, absolute agreement, single
#'   measure (Shrout & Fleiss 1979; McGraw & Wong 1996, ICC(A,1)):
#'   (MSR - MSE) / (MSR + (k - 1) MSE + k (MSC - MSE) / n), with the F-based
#'   confidence interval of McGraw & Wong (1996). The interval is checked
#'   numerically against `psych::ICC` 2.6.9 (ICC2), which applies their
#'   erratum (p. 390); the text of the erratum has not been consulted. It is
#'   classified on the lower bound of the
#'   interval (Koo & Li 2016): < 0.50 poor, 0.50-0.75 moderate, 0.75-0.90
#'   good, > 0.90 excellent.
#' \item SEM = sqrt(MSE), MDC95 = 1.96 sqrt(2) SEM and MDC90 = 1.645 sqrt(2)
#'   SEM (Weir 2005); CV = 100 SEM / mean.
#' \item CV_log: the SEM of the log-transformed values expressed as a
#'   percentage, 100 (exp(SEM_log) - 1), the typical error as a coefficient of
#'   variation of Hopkins (2000), for errors proportional to the value. `NA`
#'   when a value is not positive (d and t can be negative).
#' \item For each pair of consecutive sessions (S2 - S1, S3 - S2, ...):
#'   systematic bias (mean difference) with its t confidence interval, and
#'   95 \% limits of agreement, bias +/- 1.96 SD of the differences (Bland &
#'   Altman 1986). Heteroscedasticity is the Pearson correlation between the
#'   absolute difference and the mean of the pair; a significant one
#'   (p < 0.05) gives a warning suggesting `CV_log`.
#' \item With three sessions or more, familiarization: session 1 against the
#'   mean of the others (paired t). Discarding session 1 is recommended when
#'   p < 0.05 and the mean difference is larger than the SEM.
#' }
#' At least 3 participants are needed; with fewer than 30 a warning recalls
#' the rule of thumb of Koo & Li (2016): at least 30 heterogeneous
#' participants and, whenever possible, 3 sessions.
#'
#' @param results data frame, typically the summary of [MFOs()], with an `ID`
#'   column and/or `participant` and `session` columns.
#' @param sessions optional data frame (or path to a csv file) with `id`,
#'   `participant` and `session`.
#' @param variables columns to analyse. `NULL` (default) uses those of
#'   `MFO_g_min`, `MFO_FFM`, `FAT_MAX_perVO2max`, `FAT_MAX_porcHRmax`,
#'   `FAT_MAX_load`, `FatMax_zone_lower`, `FatMax_zone_upper`, `d`, `s` and
#'   `t` that are present and not all `NA`.
#' @param conf confidence level of the intervals (default 0.95).
#'
#' @return An object of class `MFO_reliability`, a list with:
#'   \itemize{
#'   \item `table`: one row per variable with `variable`, `n`, `k`, `mean`,
#'     `ICC`, `ICC_lower`, `ICC_upper`, `ICC_class`, `SEM`, `MDC95`, `MDC90`,
#'     `CV`, `CV_log`;
#'   \item `pairs`: one row per variable and pair of consecutive sessions with
#'     `variable`, `pair`, `n`, `bias`, `bias_lower`, `bias_upper`,
#'     `sd_diff`, `LoA_lower`, `LoA_upper`, `hetero_r`, `hetero_p`;
#'   \item `familiarization`: with k >= 3, one row per variable with
#'     `variable`, `n`, `diff` (session 1 minus the mean of the others),
#'     `p`, `SEM`, `discard_session1`; otherwise `NULL`;
#'   \item `data`: the long table used (`participant`, `session`, `variable`,
#'     `value`), and `sessions`: the session order.
#'   }
#'   `plot()` draws the Bland-Altman plots of one pair (see
#'   [plot.MFO_reliability()]).
#'
#' @references
#' Bland JM, Altman DG (1986). Statistical methods for assessing agreement
#' between two methods of clinical measurement. Lancet 1(8476): 307-310.
#'
#' Hopkins WG (2000). Measures of reliability in sports medicine and science.
#' Sports Med 30(1): 1-15. \doi{10.2165/00007256-200030010-00001}
#'
#' Koo TK, Li MY (2016). A guideline of selecting and reporting intraclass
#' correlation coefficients for reliability research. J Chiropr Med 15:
#' 155-163. \doi{10.1016/j.jcm.2016.02.012}
#'
#' McGraw KO, Wong SP (1996). Forming inferences about some intraclass
#' correlation coefficients. Psychol Methods 1(1): 30-46,
#' \doi{10.1037/1082-989X.1.1.30}; correction, p. 390.
#'
#' Shrout PE, Fleiss JL (1979). Intraclass correlations: uses in assessing
#' rater reliability. Psychol Bull 86(2): 420-428.
#'
#' Weir JP (2005). Quantifying test-retest reliability using the intraclass
#' correlation coefficient and the SEM. J Strength Cond Res 19: 231-240.
#'
#' @seealso [MFOs()], [simulate_MFO_sessions()], [write_MFO_sessions()]
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
#' sim <- simulate_MFO_sessions(n_participants = 12, n_sessions = 2,
#'                              population = list(VO2max = c(3000, 500)),
#'                              seed = 1)
#' # One row per test, as MFOs() returns it
#' results <- do.call(rbind, lapply(seq_len(nrow(sim$truth)), function(i) {
#'   tr <- sim$truth[i, ]
#'   tabs <- sim$data[[tr$id]][[tr$session]]
#'   r <- suppressWarnings(suppressMessages(
#'     MFO(step_time = 20, db_MFO = tabs$db_MFO, db_basal = tabs$db_basal,
#'         db_graded = tabs$db_graded, cv_var = "RER", author = "Frayn",
#'         energy_equivalents = "classic", age = 30)))
#'   data.frame(ID = paste(tr$id, tr$session, sep = "_"), MFO_g_min = r$MFO,
#'              FAT_MAX_perVO2max = r$FAT_MAX)
#' }))
#' rel <- suppressWarnings(MFO_reliability(results))
#' rel
#' plot(rel)
MFO_reliability <- function(results, sessions = NULL, variables = NULL, conf = 0.95) {
  if (!is.data.frame(results)) stop("results must be a data frame.", call. = FALSE)
  if (!is.numeric(conf) || length(conf) != 1 || is.na(conf) || conf <= 0 || conf >= 1) {
    stop("conf must be a single number between 0 and 1.", call. = FALSE)
  }
  ids <- .reliability_ids(results, sessions)
  session_order <- .order_sessions(unique(ids$session))
  k <- length(session_order)
  if (k < 2) stop("At least 2 sessions per participant are needed.", call. = FALSE)
  n_part <- length(unique(ids$participant))
  if (n_part < 3) stop("At least 3 participants are needed.", call. = FALSE)
  if (n_part < 30) {
    .mfo_warning("rel_few_participants",
                 sprintf(paste0("%d participants: Koo & Li (2016) recommend at least 30 ",
                                "heterogeneous participants and, whenever possible, 3 sessions."),
                         n_part), n = n_part)
  }

  default_vars <- c("MFO_g_min", "MFO_FFM", "FAT_MAX_perVO2max", "FAT_MAX_porcHRmax",
                    "FAT_MAX_load", "FatMax_zone_lower", "FatMax_zone_upper",
                    "d", "s", "t")
  if (is.null(variables)) {
    variables <- default_vars[default_vars %in% names(results)]
    variables <- variables[vapply(variables, function(v)
      any(!is.na(suppressWarnings(as.numeric(results[[v]])))), logical(1))]
    if (length(variables) == 0) stop("results has none of the default variables.", call. = FALSE)
  } else {
    absent <- setdiff(variables, names(results))
    if (length(absent) > 0) {
      stop("Variable(s) not in results: ", paste(absent, collapse = ", "), ".", call. = FALSE)
    }
  }

  long <- do.call(rbind, lapply(variables, function(v) {
    data.frame(participant = ids$participant, session = ids$session, variable = v,
               value = suppressWarnings(as.numeric(results[[v]])),
               stringsAsFactors = FALSE)
  }))

  rows <- list(); pairs <- list(); fam <- list(); hetero <- character(0)
  for (v in variables) {
    Y <- .wide_matrix(long[long$variable == v, ], session_order)
    incomplete <- sum(!stats::complete.cases(Y))
    if (incomplete > 0) {
      .mfo_warning("rel_incomplete",
                   sprintf("%s: %d participant(s) without a value in every session left out.",
                           v, incomplete), variable = v, n = incomplete)
    }
    Y <- Y[stats::complete.cases(Y), , drop = FALSE]
    if (nrow(Y) < 3) {
      .mfo_warning("rel_too_few",
                   sprintf("%s: fewer than 3 complete participants; not analysed.", v),
                   variable = v)
      next
    }
    st <- .icc_a1(Y, conf)
    rows[[v]] <- data.frame(variable = v, n = nrow(Y), k = ncol(Y), mean = mean(Y),
                            ICC = st$ICC, ICC_lower = st$lower, ICC_upper = st$upper,
                            ICC_class = .koo_li_class(st$lower), SEM = st$SEM,
                            MDC95 = 1.96 * sqrt(2) * st$SEM,
                            MDC90 = 1.645 * sqrt(2) * st$SEM,
                            CV = 100 * st$SEM / mean(Y),
                            CV_log = if (all(Y > 0)) 100 * (exp(.icc_a1(log(Y), conf)$SEM) - 1) else NA_real_,
                            stringsAsFactors = FALSE)
    for (j in seq_len(ncol(Y) - 1)) {
      pr <- .agreement(Y[, j], Y[, j + 1], conf)
      pr$variable <- v
      pr$pair <- paste0(session_order[j + 1], "-", session_order[j])
      pairs[[length(pairs) + 1]] <- pr
      if (!is.na(pr$hetero_p) && pr$hetero_p < 0.05) hetero <- c(hetero, v)
    }
    if (ncol(Y) >= 3) {
      dif <- Y[, 1] - rowMeans(Y[, -1, drop = FALSE])
      p <- if (stats::sd(dif) > 0) stats::t.test(dif)$p.value else NA_real_
      fam[[v]] <- data.frame(variable = v, n = nrow(Y), diff = mean(dif), p = p,
                             SEM = st$SEM,
                             discard_session1 = isTRUE(p < 0.05 && abs(mean(dif)) > st$SEM),
                             stringsAsFactors = FALSE)
    }
  }
  if (length(rows) == 0) stop("No variable could be analysed.", call. = FALSE)
  if (length(hetero) > 0) {
    vars <- paste(unique(hetero), collapse = ", ")
    .mfo_warning("rel_heteroscedastic",
                 paste0("Differences grow with the mean (Pearson |difference| vs mean, ",
                        "p < 0.05) for: ", vars, ". Consider CV_log."), variables = vars)
  }

  pair_cols <- c("variable", "pair", "n", "bias", "bias_lower", "bias_upper", "sd_diff",
                 "LoA_lower", "LoA_upper", "hetero_r", "hetero_p")
  out_pairs <- do.call(rbind, pairs)[, pair_cols]
  rownames(out_pairs) <- NULL
  out_table <- do.call(rbind, rows)
  rownames(out_table) <- NULL
  out_fam <- if (length(fam) > 0) do.call(rbind, fam) else NULL
  if (!is.null(out_fam)) rownames(out_fam) <- NULL

  structure(list(table = out_table, pairs = out_pairs, familiarization = out_fam,
                 data = long, sessions = session_order, conf = conf),
            class = "MFO_reliability")
}


#' @export
print.MFO_reliability <- function(x, digits = 3, ...) {
  cat(sprintf("<MFO_reliability> %d participant(s), sessions %s; ICC(2,1) with %g%% CI\n",
              max(x$table$n), paste(x$sessions, collapse = ", "), 100 * x$conf))
  tab <- x$table
  num <- vapply(tab, is.numeric, logical(1))
  tab[num] <- lapply(tab[num], signif, digits = digits)
  print(tab, row.names = FALSE)
  cat("\nBias and limits of agreement by pair:\n")
  pr <- x$pairs
  num <- vapply(pr, is.numeric, logical(1))
  pr[num] <- lapply(pr[num], signif, digits = digits)
  print(pr, row.names = FALSE)
  if (!is.null(x$familiarization)) {
    cat("\nFamiliarization (session 1 vs the mean of the others):\n")
    fm <- x$familiarization
    num <- vapply(fm, is.numeric, logical(1))
    fm[num] <- lapply(fm[num], signif, digits = digits)
    print(fm, row.names = FALSE)
  }
  invisible(x)
}


#' Bland-Altman plots of a reliability analysis
#'
#' Difference against mean of one pair of consecutive sessions, with the
#' bias and the limits of agreement, one panel per variable (Bland & Altman
#' 1986).
#'
#' @param x result of [MFO_reliability()].
#' @param pair which pair of consecutive sessions: 1 (default) is S2 - S1, 2
#'   is S3 - S2, and so on.
#' @param variables variables to draw (default all).
#' @param ... not used.
#'
#' @return A ggplot object.
#'
#' @export
plot.MFO_reliability <- function(x, pair = 1, variables = NULL, ...) {
  k <- length(x$sessions)
  if (!is.numeric(pair) || length(pair) != 1 || pair < 1 || pair > k - 1 ||
      pair != round(pair)) {
    stop(sprintf("pair must be a whole number from 1 to %d.", k - 1), call. = FALSE)
  }
  s1 <- x$sessions[pair]
  s2 <- x$sessions[pair + 1]
  label <- paste0(s2, "-", s1)
  if (is.null(variables)) variables <- x$table$variable
  pts <- do.call(rbind, lapply(variables, function(v) {
    Y <- .wide_matrix(x$data[x$data$variable == v, ], x$sessions)
    Y <- Y[stats::complete.cases(Y), , drop = FALSE]
    data.frame(variable = v, mean = (Y[, s1] + Y[, s2]) / 2, diff = Y[, s2] - Y[, s1])
  }))
  lines <- x$pairs[x$pairs$pair == label & x$pairs$variable %in% variables, ]
  ggplot(pts, aes(x = mean, y = diff)) +
    geom_hline(data = lines, aes(yintercept = bias), colour = "steelblue") +
    geom_hline(data = lines, aes(yintercept = LoA_lower), linetype = "dashed",
               colour = "grey40") +
    geom_hline(data = lines, aes(yintercept = LoA_upper), linetype = "dashed",
               colour = "grey40") +
    geom_point(colour = "darkorange", size = 2) +
    facet_wrap(~variable, scales = "free") +
    labs(x = sprintf("Mean of %s and %s", s1, s2), y = sprintf("Difference %s", label),
         subtitle = "Solid line: bias. Dashed lines: 95 % limits of agreement.") +
    theme_bw()
}


# --- Internal helpers -------------------------------------------------------

# participant and session of each row of results
.reliability_ids <- function(results, sessions) {
  if (all(c("participant", "session") %in% names(results))) {
    return(data.frame(participant = as.character(results$participant),
                      session = as.character(results$session), stringsAsFactors = FALSE))
  }
  if (!"ID" %in% names(results)) {
    stop("results needs an ID column or participant and session columns.", call. = FALSE)
  }
  id <- as.character(results$ID)
  if (!is.null(sessions)) {
    if (is.character(sessions) && length(sessions) == 1) {
      sessions <- utils::read.csv(sessions, stringsAsFactors = FALSE)
    }
    if (!is.data.frame(sessions) || !all(c("id", "participant", "session") %in% names(sessions))) {
      stop("sessions must have columns id, participant and session.", call. = FALSE)
    }
    m <- match(id, as.character(sessions$id))
    if (anyNA(m)) {
      stop("IDs not in sessions: ", paste(id[is.na(m)], collapse = ", "), ".", call. = FALSE)
    }
    return(data.frame(participant = as.character(sessions$participant[m]),
                      session = as.character(sessions$session[m]), stringsAsFactors = FALSE))
  }
  if (!all(grepl("_", id))) {
    stop("Without sessions, every ID must be <participant>_<session>, e.g. P01_S1.",
         call. = FALSE)
  }
  data.frame(participant = sub("_[^_]*$", "", id), session = sub("^.*_", "", id),
             stringsAsFactors = FALSE)
}

.order_sessions <- function(s) {
  num <- suppressWarnings(as.numeric(sub("^.*?([0-9]+)$", "\\1", s)))
  if (all(grepl("[0-9]+$", s)) && !anyNA(num) && !anyDuplicated(num)) s[order(num)] else s
}

# participants x sessions matrix of one variable
.wide_matrix <- function(d, session_order) {
  parts <- unique(d$participant)
  Y <- matrix(NA_real_, length(parts), length(session_order),
              dimnames = list(parts, session_order))
  Y[cbind(match(d$participant, parts), match(d$session, session_order))] <- d$value
  Y
}

# ICC(A,1) with the F-based CI of McGraw & Wong (1996, corrected p. 390),
# and SEM = sqrt(MSE) (Weir 2005)
.icc_a1 <- function(Y, conf) {
  n <- nrow(Y)
  k <- ncol(Y)
  gm <- mean(Y)
  MSR <- k * sum((rowMeans(Y) - gm)^2) / (n - 1)
  MSC <- n * sum((colMeans(Y) - gm)^2) / (k - 1)
  SSE <- sum((Y - gm)^2) - (n - 1) * MSR - (k - 1) * MSC
  MSE <- SSE / ((n - 1) * (k - 1))
  icc <- (MSR - MSE) / (MSR + (k - 1) * MSE + k * (MSC - MSE) / n)
  alpha <- 1 - conf
  a <- k * icc / (n * (1 - icc))
  b <- 1 + k * icc * (n - 1) / (n * (1 - icc))
  v <- (a * MSC + b * MSE)^2 / ((a * MSC)^2 / (k - 1) + (b * MSE)^2 / ((n - 1) * (k - 1)))
  FL <- stats::qf(1 - alpha / 2, n - 1, v)
  FU <- stats::qf(1 - alpha / 2, v, n - 1)
  lower <- n * (MSR - FL * MSE) / (FL * (k * MSC + (k * n - k - n) * MSE) + n * MSR)
  upper <- n * (FU * MSR - MSE) / (k * MSC + (k * n - k - n) * MSE + n * FU * MSR)
  list(ICC = icc, lower = lower, upper = upper, SEM = sqrt(MSE),
       MSR = MSR, MSC = MSC, MSE = MSE)
}

.koo_li_class <- function(lower) {
  if (is.na(lower)) return(NA_character_)
  if (lower < 0.50) "poor" else if (lower < 0.75) "moderate" else if (lower <= 0.90) "good" else "excellent"
}

# Bias, its t CI, limits of agreement and heteroscedasticity for y2 - y1
.agreement <- function(y1, y2, conf) {
  dif <- y2 - y1
  n <- length(dif)
  m <- mean(dif)
  sdd <- stats::sd(dif)
  half <- stats::qt(1 - (1 - conf) / 2, n - 1) * sdd / sqrt(n)
  het_r <- het_p <- NA_real_
  if (n >= 3 && stats::sd(abs(dif)) > 0 && stats::sd((y1 + y2) / 2) > 0) {
    ct <- stats::cor.test(abs(dif), (y1 + y2) / 2, method = "pearson")
    het_r <- unname(ct$estimate)
    het_p <- ct$p.value
  }
  data.frame(n = n, bias = m, bias_lower = m - half, bias_upper = m + half, sd_diff = sdd,
             LoA_lower = m - 1.96 * sdd, LoA_upper = m + 1.96 * sdd,
             hetero_r = het_r, hetero_p = het_p)
}
