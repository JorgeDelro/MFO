
#' @title MFO test dataframe
#'
#' @description Synthetic breath-by-breath data from an incremental exercise
#'   test with 8 stages (25-200 W, 3 min per stage). Simulates a moderately
#'   trained male cyclist covering ~20-75\% VO2max. The fat oxidation pattern
#'   shows a clear inverted U-shape with a peak around 35-40\% VO2max,
#'   suitable for demonstrating P3, P2, MRER, and SIN model fitting.
#'
#' @docType data
#'
#' @usage data(MFO_df)
#'
#' @format A data frame with 72 rows and 9 variables:
#' \describe{
#'   \item{Time}{time test, in seconds (20s intervals)}
#'   \item{HR}{heart rate, in beats/min}
#'   \item{Load}{external load, in W (25 W in the first stage, +25 W per stage)}
#'   \item{VO2}{volume of oxygen consumption, in mL/min}
#'   \item{VCO2}{volume of exhaled carbon dioxide, in mL/min}
#'   \item{RER}{respiratory exchange ratio}
#'   \item{BF}{breathing frequency, in breaths/min}
#'   \item{VE}{ventilation, in L/min}
#'   \item{PETCO2}{end-tidal carbon dioxide pressure, in mmHg}
#' }
"MFO_df"
