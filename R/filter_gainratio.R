#' Univariate filter using the information gain ratio.
#'
#' Uses the information gain ratio for feature selection, as implemented in the
#' FSelector package, to select important features.
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
filter_gainratio <- function(formula, data, limit) {
  result <- FSelector::gain.ratio(formula, data)
  result <- result[order(result, decreasing = TRUE), , drop = FALSE]

  if(is.null(limit)) {
    result
  } else {
    result[FSelector::cutoff.k(result, limit), , drop = FALSE]
  }
}
