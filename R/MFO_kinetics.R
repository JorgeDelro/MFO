
#' Maximal Fat Oxidation Kinetics
#'
#' @param MFO_data a data frame obtained from MFO function
#'
#' @importFrom minpack.lm nlsLM
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate pull
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #' # Path to the MFO package sample data
#' path <- system.file("extdata", "sample_data.xlsx", package = "MFO")
#'
#' # Read databases
#' sample_data <- read_MFO_databases(from = "files",
#'                                  path = paste(path),
#'                                  db_basal_name = "M.BASAL",
#'                                  db_MFO_name = "MFO",
#'                                  db_graded_name = "V02máx.",
#'                                  col_name_VO2 = "V'O2",
#'                                  col_name_VCO2 = "V'CO2",
#'                                  col_name_RER = "RER",
#'                                  col_name_HR = "HR",
#'                                  remove_rows = NULL)
#' # Calculate MFO and Fatmax
#' result_MFO <- MFO(step_time = 20,
#'                  db_MFO = sample_data$participant_db_MFO,
#'                  db_basal = sample_data$participant_db_basal,
#'                  db_graded = sample_data$participant_db_graded,
#'                  cv_var = "RER",
#'                  author = "Frayn",
#'                  VO2max = NULL)
#'}
#'
MFO_kinetics <- function(MFO_data) {

  # Get %MFO
  MFO_kinetics_data <- MFO_data %>%
    mutate(porc_MFO = (FAT * 100) /  max(FAT))

  # Fit cubic polynomial model
  mod <- lm(porc_MFO ~ poly(porc_VO2, 3), data = MFO_kinetics_data)

  # Extract the last load step
  last_step <- as.numeric(pull(MFO_data[nrow(MFO_data),"Load"]))

  # Create new data to predict
  data_nls <- data.frame(porc_VO2 = seq(from = 0, to = last_step, length.out = last_step))

  # Create a vector of percentage VO2 for MFO basic
  porc_VO2 = seq(from = 0, to = 100, length.out = 100)

  # Get fitted values
  err <- predict(mod, newdata = data_nls, se.fit = F)

  # Get %MFO
  data_nls <- data_nls %>%
    mutate(porc_MFO_kinetics = ((err * 100) /  max(err))/100) %>%
    rename(porc_VO2_kinetics = porc_VO2)

  # Use porc_VO2 form 0 to 100
  porc_MFO_basic <- sin((( (pi^(1/1) / pi + 2*0) * ((pi/100)*porc_VO2 + 0 + 0) )^1))

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

  # Add NAs at the end of data_nls
  n_NAs <- 100 - nrow(data_nls)
  data_nls <- data.frame(porc_MFO_kinetics = c(data_nls$porc_MFO_kinetics, rep(NA, n_NAs)),
                         porc_VO2_kinetics = c(data_nls$porc_VO2_kinetics, rep(NA, n_NAs)))


  # Dataset for plotting
  data_nls <- cbind(data_nls, porc_MFO_basic)

  #
  data_nls$porc_VO2_kinetics <- seq(from = 0, to = 100, length.out = 100)

  data_nls <- data_nls %>%
    pivot_longer(!porc_VO2_kinetics, names_to = "variables", values_to = "values")  %>%
    mutate(variables = factor(variables, levels = c("porc_MFO_basic", "porc_MFO_kinetics")))


  MFO_kinetics_plot <- data_nls %>%
    ggplot(aes(x=porc_VO2_kinetics, y=values, group = variables, colour = variables)) +
    geom_line() +
    ylim(0,1.01) +
    xlim(0, 100) +
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
