#' Feature selection through filtering
#'
#' @param formula, a formula object
#' @param data, a data frame
#' @param method, a string indicating the filter method to use
#' @param ..., arguments to be passed on to called functions
#' @param limit, an optional integer specifying the number of features to retain
filter <- function(formula,
                   data,
                   method = c("pearson", "spearman", "chisq", "cfs"),
                   ...,
                   limit = NULL
                   ) {

  # test all arguments are supplied
  if(missing(formula)) stop(sQuote("formula"), " is missing")
  if(missing(data)) stop(sQuote("data"), " is missing")
  if(missing(method)) stop(sQuote("method"), " is missing")
  #if(!is.null(limit) & (limit < 1)) stop(sQuote("limit"), "must be greater than 0")

  # get full argument
  method <- match.arg(method)

  # determine which filter method to use
  switch(EXPR = method,
    "pearson" = .filter_pearson(formula, data, limit),
    "spearman" = .filter_spearman(formula, data, limit),
    "chisq" = .filter_chisq(formula, data, limit),
    "cfs" = .filter_cfs(formula, data),
    stop("Unknown method")
  )
}

#' Univariate filter using Pearson's correlation with output variable
#' @param formula, a formula object
#' @param data, a data frame
.filter_pearson <- function(formula, data, limit) {
  result <- FSelector::linear.correlation(formula, data)
  result <- result[order(result, decreasing = TRUE), , drop = FALSE]

  if(is.null(limit)) {
    result
  } else {
    result[FSelector::cutoff.k(result, limit), , drop = FALSE]
  }
}

#' Univariate filter using rank correlation with output variable
#' @param formula, a formula object
#' @param data, a data frame
.filter_spearman <- function(formula, data, limit) {
  result <- FSelector::rank.correlation(formula, data)
  result <- result[order(result, decreasing = TRUE), , drop = FALSE]

  if(limit >= nrow(result)) {
    result
  } else {
    result[FSelector::cutoff.k(result, limit), , drop = FALSE]
  }
}

#' Univariate filter using Chi-squared
#' @param formula, a formula object
#' @param data, a data frame
.filter_chisq <- function(formula, data, limit) {
  result <- FSelector::chi.squared(formula, data)
  result <- result[order(result, decreasing = TRUE), , drop = FALSE]

  if(limit >= nrow(result)) {
    result
  } else {
    result[FSelector::cutoff.k(result, limit), , drop = FALSE]
  }
}

#' Feature selection using the CFS algorithm
#' @param formula, a formula object
#' @param data, a data frame
.filter_cfs <- function(formula, data) {
  FSelector::cfs(formula, data)
}
