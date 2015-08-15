#' Univariate filter using Pearson's correlation with output variable
#' @param formula, a formula object
#' @param data, a data frame
filter_pearson <- function(formula, data, limit) {
  result <- FSelector::linear.correlation(formula, data)
  result <- result[order(result, decreasing = TRUE), , drop = FALSE]

  if(is.null(limit)) {
    result
  } else {
    result[FSelector::cutoff.k(result, limit), , drop = FALSE]
  }
}
