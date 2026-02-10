
#' @title Graded exercise test dataframe
#'
#' @description Synthetic breath-by-breath data from a graded exercise test
#'   to exhaustion (10 stages, 75-300 W). Simulates a moderately trained
#'   male cyclist reaching VO2max ~3200 mL/min and HRmax ~185 bpm.
#'   Used to determine VO2max when this value is not known a priori.
#'
#' @docType data
#'
#' @usage data(VO2max_df)
#'
#' @format A data frame with 30 rows and 9 variables:
#' \describe{
#'   \item{Time}{time test, in seconds (20s intervals)}
#'   \item{HR}{heart rate, in beats/min}
#'   \item{Load}{workload, in watts}
#'   \item{VO2}{volume of oxygen consumption, in mL/min}
#'   \item{VCO2}{volume of exhaled carbon dioxide, in mL/min}
#'   \item{RER}{respiratory exchange ratio}
#'   \item{BF}{breathing frequency, in breaths/min}
#'   \item{VE}{ventilation, in L/min}
#'   \item{PETCO2}{end-tidal carbon dioxide pressure, in mmHg}
#' }
"VO2max_df"
