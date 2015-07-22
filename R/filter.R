#' Feature selection through filtering
#'
#' @param formula, a formula object
#' @param data, a data frame
#' @param method, a string indicating the filter method to use.
#'  Currently supported options: pearson, rank, cfs, chisq

filter <- function(formula, data, method) {

  # test all arguments are supplied
  if(missing(formula)) stop(sQuote("formula"), " is missing. Usage: filter(formula, data, method)")
  if(missing(data)) stop(sQuote("data"), " is missing. Usage: filter(formula, data, method)")
  if(missing(method)) stop(sQuote("method"), " is missing. Usage: filter(formula, data, method)")

  switch(EXPR = method,
    "pearson" = .filter_pearson(formula, data),
    "rank"    = .filter_rank(formula, data),
    "chisq"   = .filter_chisq(formula, data),
    "cfs"     = .filter_cfs(formula, data)
  )
}

.filter_pearson <- function(formula, data) {
  print("filtering with pearson...")
}

.filter_rank <- function(formula, data) {
  print("filtering with rank...")
}

.filter_chisq <- function(formula, data) {
  print("filtering with chisq...")
}

.filter_cfs <- function(formula, data) {
  print("filtering with cfs...")
}
