
#' @title Basal test dataframe
#'
#' @description Synthetic breath-by-breath data from a 15-minute resting
#'   metabolic assessment. Simulates a moderately trained male cyclist
#'   (VO2max ~3200 mL/min) with an initial stabilization period followed
#'   by steady-state resting metabolism.
#'
#' @docType data
#'
#' @usage data(basal_df)
#'
#' @format A data frame with 45 rows and 8 variables:
#' \describe{
#'   \item{Time}{time test, in seconds (20s intervals)}
#'   \item{HR}{heart rate, in beats/min}
#'   \item{VO2}{volume of oxygen consumption, in mL/min}
#'   \item{VCO2}{volume of exhaled carbon dioxide, in mL/min}
#'   \item{RER}{respiratory exchange ratio}
#'   \item{BF}{breathing frequency, in breaths/min}
#'   \item{VE}{ventilation, in L/min}
#'   \item{PETCO2}{end-tidal carbon dioxide pressure, in mmHg}
#' }
"basal_df"
