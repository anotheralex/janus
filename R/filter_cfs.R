#' Feature selection using CFS algorithm.
#'
#' Uses the correlation-based feature selection algorithm as implemented in the
#' FSelector package to select important features.
#'
#' @param formula A formula object.
#' @param data A dataframe containing predictor and response variables.
#'
#' @return A vector of predictor identifiers.
#'
#' @author Alex Wollenschlaeger, \email{alexw@@panix.com}
#'
#' @export
filter_cfs <- function(formula, data) {
  FSelector::cfs(formula, data)
}
