#' Feature selection using the CFS algorithm
#' @param formula, a formula object
#' @param data, a data frame
filter_cfs <- function(formula, data) {
  FSelector::cfs(formula, data)
}
