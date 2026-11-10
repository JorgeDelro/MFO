#' SIN model of fat oxidation kinetics
#'
#' Equation \[14\] of Cheneviere et al. (2009, Med Sci Sports Exerc 41: 1615-1625,
#' p. 1618 and Appendix p. 1625, \doi{10.1249/MSS.0b013e31819e2f91}):
#' \deqn{\%MFO(x) = \sin\{[\pi^{1/s} / (\pi + 2d) \cdot (Kx + d + t)]^s\},
#'       \quad K = \pi / 100}
#' with x in \% VO2max, d = dilatation, s = symmetry and t = translation.
#' The denominator is (pi + 2d); v0.1.0/v0.2.0 computed (pi^(1/s) / pi) + 2d,
#' which is the same curve family but a different d (SPEC 6.5, 19 finding 5).
#'
#' @param x intensity (\% VO2max).
#' @param d,s,t SIN parameters.
#'
#' @return \%MFO as a proportion (1 at Fatmax). Below the first zero the
#'   base is negative and the result is NaN for non-integer `s`.
#'
#' @noRd
.sin_model <- function(x, d, s, t) {
  K <- pi / 100
  sin((pi^(1 / s) / (pi + 2 * d) * (K * x + d + t))^s)
}

#' Closed-form characteristic points of the SIN model
#'
#' Derived from equation \[14\] of Cheneviere et al. (2009); not printed in the
#' article (SPEC 6.5). With the argument u = pi^(1/s)/(pi + 2d) (Kx + d + t):
#' \itemize{
#' \item Fatmax: u^s = pi/2, so x = \[(pi + 2d) 2^(-1/s) - d - t\] / K;
#' \item Fatmin, second zero (Cheneviere's definition): u^s = pi, so
#'   x = (pi + d - t) / K;
#' \item Fatmin at a fraction `threshold` of MFO above Fatmax:
#'   u^s = pi - asin(threshold), so
#'   x = \[(pi + 2d) ((pi - asin(threshold)) / pi)^(1/s) - d - t\] / K.
#' }
#'
#' @param d,s,t SIN parameters.
#' @param threshold `NULL` (second zero) or a proportion of MFO in (0, 1).
#'
#' @return intensity in \% VO2max.
#'
#' @noRd
.sin_fatmax <- function(d, s, t) {
  ((pi + 2 * d) * 2^(-1 / s) - d - t) / (pi / 100)
}

#' @rdname dot-sin_fatmax
#' @noRd
.sin_fatmin <- function(d, s, t, threshold = NULL) {
  level <- if (is.null(threshold)) pi else pi - asin(threshold)
  ((pi + 2 * d) * (level / pi)^(1 / s) - d - t) / (pi / 100)
}
