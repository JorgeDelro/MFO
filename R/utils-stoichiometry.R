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


#' Peronnet & Massicotte (1991) constants, Table 1
#'
#' Litres of O2 consumed / CO2 produced per g of glucose and of fatty acid
#' (Peronnet F, Massicotte D. Can J Sport Sci 1991; 16: 23-29, Table 1).
#' The article prints no oxidation equations, so the package derives them
#' from these constants with [.derive_oxidation_coefficients()] (SPEC 19,
#' finding 6): CHO = 4.58519 VCO2 - 3.22597 VO2, FAT = 1.69469 VO2 - 1.70130 VCO2.
#' The values often quoted as Peronnet's (4.585 / 3.2255 / 1.6946 / 1.7012),
#' used up to v0.2.0, are these numbers rounded by later authors.
#'
#' @noRd
.peronnet_constants <- list(
  glucose = c(O2 = 0.7455, CO2 = 0.7426),
  fatty_acid = c(O2 = 2.0092, CO2 = 1.4136)
)


#' Peronnet & Massicotte (1991) oxidation coefficients
#'
#' Derived from [.peronnet_constants] by solving the 2x2 system (see there).
#'
#' @noRd
.peronnet_coefficients <- function() {
  k <- .peronnet_constants
  .derive_oxidation_coefficients(cho_O2 = k$glucose[["O2"]], cho_CO2 = k$glucose[["CO2"]],
                                 fat_O2 = k$fatty_acid[["O2"]],
                                 fat_CO2 = k$fatty_acid[["CO2"]])
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
