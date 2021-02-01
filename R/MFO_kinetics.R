
#' Maximal Fat Oxidation Kinetics
#'
#' @param MFO_data a data frame obtained from MFO function
#'
#' @importFrom minpack.lm nlsLM
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate
#' @import ggplot2
#'
#' @return
#' @export
#'
#' @examples
MFO_kinetics <- function(MFO_data) {

  # Get %MFO
  MFO_kinetics_data <- MFO_data %>%
    mutate(porc_MFO = (FAT * 100) /  max(FAT))

  # Fit cubic polynomial model# mod <- lm(porc_MFO ~ porc_VO2 + I(porc_VO2^2), data = MFO_kinetics_data)
  mod <- lm(porc_MFO ~ poly(porc_VO2, 3), data = MFO_kinetics_data)

  # Create new data
  data_nls <- data.frame(porc_VO2 = seq(from = 0, to = 100, length.out = 100))

  # Get fitted values
  err <- predict(mod, newdata = data_nls, se.fit = F)

  # Get %MFO
  data_nls <- data_nls %>%
    mutate(porc_MFO_kinetics = ((err * 100) /  max(err))/100) %>%
    rename(porc_VO2_kinetics = porc_VO2)

  porc_MFO_basic <- sin((( (pi^(1/1) / pi + 2*0) * ((pi/100)*data_nls$porc_VO2_kinetics + 0 + 0) )^1))

  # %MFO = sin((( (pi^1/s / pi + 2*d) * (K*porc_VO2 + d + t) )^s))

  # Estimate non-linear parameters
  mod_nls = nlsLM(porc_MFO_kinetics ~ sin(( (3.1416^(1/s) / 3.1416 + 2*d) * (0.03141593*porc_VO2_kinetics + d + t) )^s),
                  start=list(d = 0, s = 1, t = 0),
                  data = data_nls)

  d <- data.frame(summary(mod_nls)$coefficients)["d","Estimate"]
  t <- data.frame(summary(mod_nls)$coefficients)["t","Estimate"]
  s <- data.frame(summary(mod_nls)$coefficients)["s","Estimate"]

  d_text <- paste("d == ~",round(d, 2))
  t_text <- paste("t == ~",round(t, 2))
  s_text <- paste("s == ~",round(s, 2))

  data_nls <- cbind(data_nls, porc_MFO_basic)

  data_nls <- data_nls %>%
    pivot_longer(!porc_VO2_kinetics, names_to = "variables", values_to = "values")  %>%
    mutate(variables = factor(variables, levels = c("porc_MFO_basic", "porc_MFO_kinetics")))


  MFO_kinetics_plot <- data_nls %>%
    ggplot(aes(x=porc_VO2_kinetics, y=values, group = variables, colour = variables)) +
    geom_line() +
    ylim(0,1.01) +
    ylab("Fat oxidation (%MFO)") +
    xlab(expression("Exercise " *"intensity " *"("*VO["2max"]*")")) +
    theme_bw() +
    theme(legend.position = "none",
          axis.text.x = element_text(size=11),
          axis.text.y = element_text(size=11),
          axis.title = element_text(size=12)) +
    annotate("label",
             x = 5,
             y = 0.95,
             label = d_text,
             parse = T) +
    annotate("label",
             x = 5,
             y = 0.875,
             label = t_text,
             parse = T) +
    annotate("label",
             x = 5,
             y = 0.80,
             label = s_text,
             parse = T)

  return(list(
    MFO_kinetics_data = MFO_kinetics_data,
    MFO_kinetics_plot = MFO_kinetics_plot,
    d = d,
    t = t,
    s = s
  ))


}
