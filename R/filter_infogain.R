#' Univariate filter using Information Gain
#' @param formula, a formula object
#' @param data, a data frame
filter_infogain <- function(formula, data, limit) {
  result <- FSelector::information.gain(formula, data)
  result <- result[order(result, decreasing = TRUE), , drop = FALSE]

  if(is.null(limit)) {
    result
  } else {
    result[FSelector::cutoff.k(result, limit), , drop = FALSE]
  }
}
