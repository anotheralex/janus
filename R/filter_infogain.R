#' Univariate filter using information gain.
#'
#' Uses information gain for feature selection, as implemented in the FSelector
#' package, to select important features.
#'
#' @param formula A formula object.
#' @param data A dataframe containing predictor and response variables.
#' @param limit Optional integer specifying the number of features to retain.
#'
#' @return A vector of feature importance values.
#'
#' @author Alex Wollenschlaeger, \email{alexw@@panix.com}
#'
#' @export
filter_infogain <- function(formula, data, limit) {
  result <- FSelector::information.gain(formula, data)
  result <- result[order(result, decreasing = TRUE), , drop = FALSE]

  if(is.null(limit)) {
    result
  } else {
    result[FSelector::cutoff.k(result, limit), , drop = FALSE]
  }
}
