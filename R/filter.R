#' Feature selection through filtering
#'
#' @param formula, a formula object
#' @param data, a data frame
#' @param method, a string indicating the filter method to use.
#'  Currently supported options: pearson, rank, cfs, chi2

filter <- function(formula, data, method) {

  # test all arguments are supplied
  if(missing(formula)) stop("Missing formula. Usage filter(formula, data, method)")
  if(missing(data)) stop("Missing data. Usage filter(formula, data, method)")
  if(missing(method)) stop("Missing data. Usage filter(formula, data, method)")

  print("filtering...")
}
