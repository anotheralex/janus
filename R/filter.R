#' Feature selection through filtering
#'
#'

filter <- function(formula, data, method) {

  # test all arguments are supplied
  if(missing(formula)) stop("Missing formula. Usage filter(formula, data, method")
  if(missing(data)) stop("Missing data. Usage filter(formula, data, method")
  if(missing(method)) stop("Missing data. Usage filter(formula, data, method")

  print("filtered features")
}
