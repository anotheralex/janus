#' Univariate filter using the Information Gain Ratio
#' @param formula, a formula object
#' @param data, a data frame
filter_gainratio <- function(formula, data, limit) {
  result <- FSelector::gain.ratio(formula, data)
  result <- result[order(result, decreasing = TRUE), , drop = FALSE]

  if(is.null(limit)) {
    result
  } else {
    result[FSelector::cutoff.k(result, limit), , drop = FALSE]
  }
}

