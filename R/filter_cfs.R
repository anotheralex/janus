#' Feature selection using CFS algorithm
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
