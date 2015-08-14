#' fit a binomial or multinomial logistic regression classifier using
#'    glmnet::glmnet
#'
#' @param x a dataframe or matrix containing predictors
#' @param y a factorn vector containing response variables with two or more
#'    levels
#' @param ... additional parameters to pass to glmnet
#'
#' @return fitted model in object of class janus
#'
#' @author Alex Wollenschlaeger, \email{alexw@@panix.com}
#'
#' @export
fit_glmnet <- function(x, y, ...) {

  # check that necessary data has been supplied and in correct form
  if(missing(x)) stop(sQuote("x"), " missing")
  if(missing(y)) stop(sQuote("y"), " missing")
  if(is.null(x)) stop(sQuote("x"), " cannot be null")
  if(is.null(y)) stop(sQuote("y"), " cannot be null")

  # check if y is a factor variable and if not warn that it will be coerced
  if(!is.factor(y)) {
    message(sQuote("y"), " is not a factor and will be coerced")
  }

  # check if y is a matrix and if not warn that it will be coerced
  if(!is.matrix(x)) {
    message(sQuote("x"), " is not a matrix and will be coerced")
  }

  # check how many levels the factor response variable has
  if(is.factor(y)) {
    levels <- nlevels(y)
  } else {
    num_levels <- length(unique(y))
    if (num_levels == length(y)) {
      stop(sQuote("y"), " is not a valid factor variable")
    } else {
      levels <- length(unique(y))
    }
  }

  # fit a bi- or multinomial model based on the number of levels in the
  # response variable
  # cast x and y into matrix and factor variable, respectively
  if(levels == 2) {
    model <- glmnet::glmnet(as.matrix(x), as.factor(y), family = "binomial")
  } else if (levels > 2) {
    model <- glmnet::glmnet(as.matrix(x), as.factor(y), family = "multinomial",
                            type.multinomial = "grouped")
  } else {
    stop("Unsupported number of levels in factor variable ", sQuote("y"))
  }

  res <- janus(model, package = "glmnet", classifier = "glmnet",
               interface = "default")
  res
}
