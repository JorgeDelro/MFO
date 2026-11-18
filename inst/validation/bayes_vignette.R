# Figures and numbers of vignette("bayes"), computed once (Stan compiles take
# about a minute each). From the package source folder:
#   Rscript -e "devtools::load_all(); source('inst/validation/bayes_vignette.R')"
# Writes vignettes/figures/bayes-*.png and prints the numbers used in the text.
if (!exists("MFO_bayes")) library(MFO)
library(ggplot2)
fig <- function(name, p, h = 4.2) ggsave(file.path("vignettes", "figures", name), p,
                                         width = 7, height = h, dpi = 96)
data(list = c("basal_df", "MFO_df", "VO2max_df"), package = "MFO")
r <- suppressWarnings(suppressMessages(
  MFO(step_time = 20, db_MFO = MFO_df, db_basal = basal_df, db_graded = VO2max_df,
      cv_var = "RER", author = "Frayn", energy_equivalents = "classic", age = 30,
      negative_FAT = "keep")))
db <- r$MFO_db

# 1. Prior predictive: priors first proposed in SPEC 13.2 (d, s, t) vs current
set.seed(1)
n <- 200
x <- seq(0, 100, length.out = 101)
spec <- data.frame(M = pmax(rnorm(n, 0.45, 0.25), 0.01), d = rnorm(n, 0, 0.3),
                   s = rlnorm(n, 0, 0.3), t = rnorm(n, 0, 0.2))
curve_df <- function(p, label) do.call(rbind, lapply(seq_len(nrow(p)), function(i)
  data.frame(draw = i, x = x, FAT = .bayes_curve(p[i, ], "SIN", x), priors = label)))
pc <- MFO_prior_check(db, ndraws = n, seed = 2)
cur <- pc$draws[, c("M", "d", "s", "t")]
pp <- rbind(curve_df(spec, "SPEC 13.2 priors on d, s, t"),
            curve_df(cur, "Current priors on Fatmax, Fatmin, s"))
fig("bayes-prior-predictive.png",
    ggplot(pp, aes(x, FAT, group = draw)) + geom_line(alpha = 0.15, colour = "grey30") +
      facet_wrap(~priors) +
      labs(x = "Exercise intensity (% VO2max)", y = "Fat oxidation (g/min)") + theme_bw())
q <- function(v) round(stats::quantile(v, c(0.05, 0.5, 0.95), na.rm = TRUE), 1)
cat("SPEC priors: Fatmax", q(.sin_fatmax(spec$d, spec$s, spec$t)),
    " Fatmin", q(.sin_fatmin(spec$d, spec$s, spec$t)), "\n")
cat("current priors: Fatmax", q(pc$draws$Fatmax), " Fatmin", q(pc$draws$Fatmin), "\n")

# 2. Fit of the example data, constant and proportional sigma
b <- MFO_bayes(db, seed = 1)
print(b)
fig("bayes-fit.png", plot(b) + labs(caption = NULL), h = 4.6)
bp <- MFO_bayes(db, sigma = "proportional", seed = 1)
lc <- brms::loo(b$fit)
lp <- brms::loo(bp$fit)
print(brms::loo_compare(list(constant = lc, proportional = lp)))
print(rbind(constant = lc$estimates["elpd_loo", ], proportional = lp$estimates["elpd_loo", ]))
cat("Pareto k > 0.7: constant", sum(lc$diagnostics$pareto_k > 0.7),
    " proportional", sum(lp$diagnostics$pareto_k > 0.7), "\n")

# 3. Prior and posterior of each quantity
prior_d <- .bayes_draws(b$fit, "SIN", b$data, "prior_b_")
long <- function(p, what) do.call(rbind, lapply(c("MFO", "Fatmax", "Fatmin", "d", "s", "t"),
  function(v) data.frame(quantity = v, value = p[[v]], what = what)))
ov <- rbind(long(prior_d, "prior"), long(b$draws, "posterior"))
lims <- list(MFO = c(0, 1.2), Fatmax = c(10, 90), Fatmin = c(40, 100), d = c(-1.5, 1.5),
             s = c(0, 3), t = c(-1.5, 2))
ov <- do.call(rbind, lapply(names(lims), function(v) {
  z <- ov[ov$quantity == v, ]
  z[z$value >= lims[[v]][1] & z$value <= lims[[v]][2], ]
}))
ov$quantity <- factor(ov$quantity, levels = names(lims))
fig("bayes-prior-posterior.png",
    ggplot(ov, aes(value, fill = what)) + geom_density(alpha = 0.4, colour = NA) +
      facet_wrap(~quantity, scales = "free") +
      scale_fill_manual(values = c(prior = "grey60", posterior = "darkorange")) +
      labs(x = NULL, y = "Density", fill = NULL) + theme_bw() +
      theme(legend.position = "bottom"), h = 4.6)

# 4. The ridge: the most correlated pair of shape parameters
cr <- stats::cor(b$draws[, c("d", "s", "t")]); diag(cr) <- 0
pair <- which(abs(cr) == max(abs(cr)), arr.ind = TRUE)[1, ]
v1 <- colnames(cr)[pair[1]]; v2 <- colnames(cr)[pair[2]]
cat("most correlated pair:", v1, v2, round(cr[pair[1], pair[2]], 3), "\n")
fig("bayes-ridge.png",
    ggplot(b$draws, aes(.data[[v1]], .data[[v2]])) + geom_point(alpha = 0.15, size = 0.8) +
      labs(subtitle = sprintf("Posterior draws, r = %.2f", cr[pair[1], pair[2]])) + theme_bw(),
    h = 4)
