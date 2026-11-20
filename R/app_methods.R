# Methods paragraph of the app report (#24): the template approved by Jorge
# (2026-11-20), filled with the options of the analysis. English only; only
# sources verified in SPEC 19 (Davison & Hinkley and Gelman at book level,
# Howley only for the VO2max/VO2peak label). The commit and the seed go in
# the report footer, not here.

# Reference list, author-year, by key
.app_references <- c(
  achten2002 = paste("Achten J, Gleeson M, Jeukendrup AE (2002). Determination of the exercise",
                     "intensity that elicits maximal fat oxidation. Med Sci Sports Exerc 34:92-97.",
                     "doi:10.1097/00005768-200201000-00015"),
  amarogahete2018 = paste("Amaro-Gahete FJ, Sanchez-Delgado G, Ruiz JR (2018). Commentary:",
                          "Contextualising maximal fat oxidation during exercise: determinants and",
                          "normative values. Front Physiol 9:1460. doi:10.3389/fphys.2018.01460"),
  amarogahete2019 = paste("Amaro-Gahete FJ, Sanchez-Delgado G, Alcantara JMA, et al. (2019).",
                          "Optimizing maximal fat oxidation assessment by a treadmill-based graded",
                          "exercise protocol: when should the test end? Front Physiol 10:909.",
                          "doi:10.3389/fphys.2019.00909"),
  brooks1994 = paste("Brooks GA, Mercier J (1994). Balance of carbohydrate and lipid utilization",
                     "during exercise: the \"crossover\" concept. J Appl Physiol 76(6):2253-2261.",
                     "doi:10.1152/jappl.1994.76.6.2253"),
  chavezguevara2023 = paste("Ch\u00e1vez-Guevara IA, Amaro-Gahete FJ, Ramos-Jim\u00e9nez A, Brun JF",
                            "(2023). Toward exercise guidelines for optimizing fat oxidation during",
                            "exercise in obesity: a systematic review and meta-regression. Sports",
                            "Med 53(12):2399-2416. doi:10.1007/s40279-023-01897-y"),
  cheneviere2009 = paste("Chenevi\u00e8re X, Malatesta D, Peters EM, Borrani F (2009). A",
                         "mathematical model to describe fat oxidation kinetics during graded",
                         "exercise. Med Sci Sports Exerc 41:1615-1625.",
                         "doi:10.1249/MSS.0b013e31819e2f91"),
  davison1997 = paste("Davison AC, Hinkley DV (1997). Bootstrap Methods and their Application.",
                      "Cambridge University Press."),
  frayn1983 = paste("Frayn KN (1983). Calculation of substrate oxidation rates in vivo from",
                    "gaseous exchange. J Appl Physiol 55:628-634. doi:10.1152/jappl.1983.55.2.628"),
  gelman2013 = paste("Gelman A, Carlin JB, Stern HS, Dunson DB, Vehtari A, Rubin DB (2013).",
                     "Bayesian Data Analysis, 3rd ed. Chapman & Hall/CRC."),
  howley1995 = paste("Howley ET, Bassett DR, Welch HG (1995). Criteria for maximal oxygen uptake:",
                     "review and commentary. Med Sci Sports Exerc 27(9):1292-1301.",
                     "doi:10.1249/00005768-199509000-00009"),
  jeukendrup2005 = paste("Jeukendrup AE, Wallis GA (2005). Measurement of substrate oxidation",
                         "during exercise by means of gas exchange measurements. Int J Sports Med",
                         "26 Suppl 1:S28-S37. doi:10.1055/s-2004-830512"),
  peronnet1991 = paste("P\u00e9ronnet F, Massicotte D (1991). Table of nonprotein respiratory",
                       "quotient: an update. Can J Sport Sci 16(1):23-29."),
  tanaka2001 = paste("Tanaka H, Monahan KD, Seals DR (2001). Age-predicted maximal heart rate",
                     "revisited. J Am Coll Cardiol 37(1):153-156.",
                     "doi:10.1016/s0735-1097(00)01054-8")
)

# Citation of the package from inst/CITATION, with its version
.app_package_citation <- function() {
  cit <- tryCatch(utils::citation("MFO"), error = function(e) NULL)
  if (is.null(cit)) return(sprintf("MFO R package version %s.", utils::packageVersion("MFO")))
  txt <- attr(unclass(cit)[[1]], "textVersion")
  if (is.null(txt)) txt <- format(cit[1], style = "text")
  txt
}

# Commit of the installed package (GitHub installs record it), NA otherwise
.app_commit <- function() {
  d <- utils::packageDescription("MFO")
  sha <- d$RemoteSha
  if (is.null(sha)) sha <- d$GithubSHA1
  if (is.null(sha)) NA_character_ else substr(sha, 1, 7)
}


# The paragraph and its references. a: analysis as stored by the app (res,
# kin, opts, cart, data); ci: settings of the intervals or NULL; zones: the
# zones of the Zones tab (zones, options) or NULL
.app_methods <- function(a, ci = NULL, zones = NULL) {
  o <- a$opts
  res <- a$res
  refs <- character(0)
  cite <- function(key, text) {
    refs <<- c(refs, key)
    text
  }
  s <- character(0)
  add <- function(...) s <<- c(s, paste0(...))

  mode <- if (identical(o$mode, "cycling")) "cycle ergometer" else "treadmill"
  add("[Participants were tested after {...} h of fasting, on a ", mode, ".]")
  add("MFO and Fatmax were computed with the R package MFO (version ",
      as.character(utils::packageVersion("MFO")), ").")

  info <- a$cart$info
  if (!is.null(info$averaging) && grepl("^mean", info$averaging)) {
    add("Breath-by-breath gas exchange data were averaged in consecutive ",
        format(a$cart$step_time), "-s intervals.")
  } else {
    add("Gas exchange data were used as exported by the analyser (",
        format(a$cart$step_time), "-s averages).")
  }
  add("Resting substrate oxidation was computed over the 5-min window with the lowest ",
      "coefficient of variation of ", o$cv_var, ".")

  if (identical(o$type, "ramp")) {
    add("VO2 and VCO2 were averaged in ", o$ramp_window, "-s windows every ", o$ramp_step,
        " s of the ramp.")
  } else {
    steps <- .app_numbers(o$step_duration)
    steady <- .app_numbers(o$steady_state_duration)
    if (length(steps) > 1) {
      add("VO2 and VCO2 were averaged over the last ", paste(steady, collapse = "/"),
          " s of each step (step durations: ", paste(steps, collapse = ", "), " s, the last ",
          "one repeated).")
    } else {
      add("VO2 and VCO2 were averaged over the last ", paste(steady, collapse = "/"),
          " s of each ", steps, "-s step", if (identical(o$type, "relative"))
            " (loads relative to body mass)", ".")
    }
  }

  eq <- switch(o$author,
               Frayn = cite("frayn1983", "Frayn (1983)"),
               Peronnet = cite("peronnet1991", "P\u00e9ronnet and Massicotte (1991)"),
               Jeukendrup = cite("jeukendrup2005", "Jeukendrup and Wallis (2005)"))
  add("Fat and carbohydrate oxidation rates were calculated with the stoichiometric ",
      "equations of ", eq, ", assuming negligible protein oxidation.")
  if (identical(o$energy_equivalents, "source")) {
    add(switch(o$author,
      Peronnet = "Energy expenditure was calculated with the energy equivalents of the same source (3.8683 kcal/g for carbohydrate and 9.7460 kcal/g for fat).",
      Frayn = paste0("Energy expenditure was calculated with the energy equivalents of ",
                     cite("peronnet1991", "P\u00e9ronnet and Massicotte (1991)"),
                     " (3.8683 kcal/g for carbohydrate and 9.7460 kcal/g for fat), as Frayn (1983) gives none."),
      Jeukendrup = "Energy expenditure was calculated with the energy equivalents of the same source (carbohydrate 3.95 kcal/g below and 4.07 kcal/g from 50 % VO2max; fat 9.75 kcal/g)."))
  }

  if (!identical(o$RER_cutoff, "none")) {
    value <- switch(o$RER_cutoff, classic = 1.00, safe = 0.95, sedentary = 0.93,
                    trained = 0.90, custom = o$RER_custom)
    src <- switch(o$RER_cutoff,
                  classic = paste0(" (", cite("cheneviere2009", "Chenevi\u00e8re et al., 2009"), ")"),
                  safe = , sedentary = , trained =
                    paste0(" (", cite("amarogahete2019", "Amaro-Gahete et al., 2019"), ")"),
                  "")
    add("Steps with RER above ", formatC(value, format = "f", digits = 2), " were excluded",
        src, ".")
  }
  add(switch(o$negative_FAT,
             truncate = "Negative fat oxidation values were set to zero.",
             exclude = "Steps with negative fat oxidation were excluded.",
             keep = "Negative fat oxidation values were kept."))

  if (identical(o$vo2max_source, "graded")) {
    add("VO2max was the highest ", format(res$VO2max_window), "-s rolling mean of VO2 in ",
        "the graded test and was labelled ", res$VO2max_type, " according to ",
        cite("howley1995", "Howley et al. (1995)"), ".")
  } else {
    add("VO2max (", format(round(res$VO2max)), " mL/min) was measured in a separate test.")
  }

  basal <- if (isTRUE(o$include_basal)) ", including the resting value as the first point" else ""
  add("MFO and Fatmax were ", switch(o$method,
    P3 = paste0("determined from a third-degree polynomial fitted to fat oxidation against ",
                "% VO2max", basal),
    P2 = paste0("determined from a second-degree polynomial fitted to fat oxidation against ",
                "% VO2max", basal),
    MV = "determined as the step with the highest measured fat oxidation",
    MRER = paste0("determined with the MRER method (",
                  cite("cheneviere2009", "Chenevi\u00e8re et al., 2009"), ")")), ".")
  add("The Fatmax zone comprised the intensities with fat oxidation of at least 90 % of MFO (",
      cite("achten2002", "Achten et al., 2002"), "), and the crossover point was the ",
      "intensity at which carbohydrate exceeded fat oxidation (",
      cite("brooks1994", "Brooks and Mercier, 1994"), ").")

  if (!is.null(ci)) {
    how <- if (identical(ci$type, "parametric")) {
      paste0("a parametric bootstrap with \u03c3 drawn from its posterior under a flat prior (",
             cite("gelman2013", "Gelman et al., 2013"), ")")
    } else {
      paste0("a bootstrap of the modified residuals (",
             cite("davison1997", "Davison and Hinkley, 1997"), ")")
    }
    add(format(100 * ci$conf), " % confidence intervals were obtained by ", how, " with ",
        ci$B, " replicates.")
  }

  if (!is.null(a$kin) && isTRUE(a$kin$convergence)) {
    add("Fat oxidation kinetics were described with the SIN model (",
        cite("cheneviere2009", "Chenevi\u00e8re et al., 2009"), "), fitted to ",
        if (identical(o$sin_fit, "absolute")) "absolute fat oxidation" else
          "fat oxidation normalised to MFO",
        ", which gives dilatation (d), symmetry (s) and translation (t).")
  }

  zn <- zones$zones
  if (!is.null(zn)) {
    if (!is.na(zn$HRmax)) {
      base <- switch(zn$HRmax_source,
                     given = "the maximal heart rate provided",
                     measured = "the measured peak heart rate",
                     paste0("the predicted maximal heart rate (",
                            cite("tanaka2001", "Tanaka et al., 2001"), ")"))
      add("Heart rate at Fatmax and at the zone limits was interpolated between steps and ",
          "expressed as a percentage of ", base, ".")
    }
    if (identical(zones$options$population, "obesity") && !is.na(zn$obesity_range_lower)) {
      add("The zone was compared with the range recommended for obesity by ",
          cite("chavezguevara2023", "Ch\u00e1vez-Guevara et al. (2023)"), ".")
    }
    if (!is.na(zn$MFO_percentile_label) || !is.na(zn$Fatmax_percentile_label)) {
      add("MFO and Fatmax were compared with the percentiles of sedentary adults of ",
          cite("amarogahete2018", "Amaro-Gahete et al. (2018)"), " (",
          sub("^Amaro-Gahete 2018: ", "", zn$reference), ", n = ", zn$reference_n,
          "); that reference differs from this test in ", zn$reference_differences, ".")
    }
  }

  keys <- sort(unique(refs))
  list(text = paste(s, collapse = " "),
       references = c(.app_package_citation(), unname(.app_references[keys])))
}
