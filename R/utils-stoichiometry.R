#' Oxidation equations derived from O2/CO2 constants per gram of substrate
#'
#' Solves the 2x2 system
#' VO2 = cho_O2 * CHO + fat_O2 * FAT and VCO2 = cho_CO2 * CHO + fat_CO2 * FAT
#' (L/min and g/min), so that every coefficient in the package can be traced
#' back to the constants published in the source table.
#'
#' @param cho_O2,cho_CO2 litres of O2 consumed / CO2 produced per g of carbohydrate.
#' @param fat_O2,fat_CO2 litres of O2 consumed / CO2 produced per g of fatty acid.
#'
#' @return named numeric vector with the coefficients of
#'   CHO = cho_VCO2 * VCO2 - cho_VO2 * VO2 and FAT = fat_VO2 * VO2 - fat_VCO2 * VCO2.
#'
#' @noRd
.derive_oxidation_coefficients <- function(cho_O2, cho_CO2, fat_O2, fat_CO2) {
  det <- fat_O2 * cho_CO2 - cho_O2 * fat_CO2
  c(cho_VCO2 = fat_O2 / det,
    cho_VO2 = fat_CO2 / det,
    fat_VO2 = cho_CO2 / det,
    fat_VCO2 = cho_O2 / det)
}


#' Jeukendrup & Wallis (2005) constants, Table 4
#'
#' Litres of O2 / CO2 per g of glucose, glycogen and fatty acid (Jeukendrup AE,
#' Wallis GA. Int J Sports Med 2005; 26 Suppl 1: S28-S37, Table 4).
#'
#' @noRd
.jw_constants <- list(
  glucose = c(O2 = 0.7455, CO2 = 0.7426),
  glycogen = c(O2 = 0.8283, CO2 = 0.8251),
  fatty_acid = c(O2 = 2.0092, CO2 = 1.4136)
)


#' Jeukendrup & Wallis (2005) equations for a glucose/glycogen mix
#'
#' Carbohydrate is a mix of `glucose_fraction` glucose and the rest glycogen.
#' Only 100/0 (pure glucose) is used in calculations: it is the package's choice
#' for resting values, because the article proposes no resting equation. For
#' exercise the package uses the coefficients printed in Table 5; 50/50
#' (40-50 \% VO2max) and 20/80 (50-75 \% VO2max) are derived only to cross-check
#' them in the tests.
#'
#' @param glucose_fraction fraction of carbohydrate oxidised as glucose (0-1).
#'
#' @noRd
.jeukendrup_coefficients <- function(glucose_fraction) {
  k <- .jw_constants
  cho <- glucose_fraction * k$glucose + (1 - glucose_fraction) * k$glycogen
  .derive_oxidation_coefficients(cho_O2 = cho[["O2"]], cho_CO2 = cho[["CO2"]],
                                 fat_O2 = k$fatty_acid[["O2"]],
                                 fat_CO2 = k$fatty_acid[["CO2"]])
}
