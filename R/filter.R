#' Feature selection through filtering
#'
#' @param formula, a formula object
#' @param data, a data frame
#' @param method, a string indicating the filter method to use
filter <- function(formula, data, method = c("pearson", "spearman", "chisq", "cfs")) {

  # test all arguments are supplied
  if(missing(formula)) stop(sQuote("formula"), " is missing. Usage: filter(formula, data, method)")
  if(missing(data)) stop(sQuote("data"), " is missing. Usage: filter(formula, data, method)")
  if(missing(method)) stop(sQuote("method"), " is missing. Usage: filter(formula, data, method)")

  # get full argument
  method <- match.arg(method)

  # determine which filter method to use
  switch(EXPR = method,
    "pearson" = .filter_pearson(formula, data),
    "spearman" = .filter_spearman(formula, data),
    "chisq" = .filter_chisq(formula, data),
    "cfs" = .filter_cfs(formula, data),
    stop("Unknown method")
  )
}

#' Univariate filter using Pearson's correlation with output variable
#' @param formula, a formula object
#' @param data, a data frame
.filter_pearson <- function(formula, data) {
  #loadNamespace("FSelector")
  result <- FSelector::linear.correlation(formula, data)
  result[order(result, decreasing = TRUE), , drop = FALSE]
}

#' Univariate filter using rank correlation with output variable
#' @param formula, a formula object
#' @param data, a data frame
.filter_spearman <- function(formula, data) {
  loadNamespace("FSelector")
  rank.correlation(formula, data)
}

#' Univariate filter using Chi-squared
#' @param formula, a formula object
#' @param data, a data frame
.filter_chisq <- function(formula, data) {
  loadNamespace("FSelector")
  chi.squared(formula, data)
}

#' Feature selection using the CFS algorithm
#' @param formula, a formula object
#' @param data, a data frame
.filter_cfs <- function(formula, data) {
  loadNamespace("FSelector")
  cfs(formula, data)
}
