#' Univariate filter using Chi-squared
#' @param formula, a formula object
#' @param data, a data frame
filter_chisq <- function(formula, data, limit) {
  result <- FSelector::chi.squared(formula, data)
  result <- result[order(result, decreasing = TRUE), , drop = FALSE]

  if(is.null(limit)) {
    result
  } else {
    result[FSelector::cutoff.k(result, limit), , drop = FALSE]
  }
}
