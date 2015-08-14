#' Fit a binomial or multinomial logistic regression classifier using
#'    glmnet::glmnet.
#'
#' @param x A dataframe or matrix containing predictors.
#' @param y A vector containing response variables with two or more
#'    levels.
#' @param ... Additional parameters to pass to glmnet.
#' @param cvfit Logical indicating whether to use cross validation to find best
#'    value for model hyperparameters. Defaults to TRUE.
#' @param type_measure String indicating the measure to use during cross
#'    validation. Default measure is "deviance". Alternatives include:
#'    \enumerate{
#'      \item "class": misclassification error rate
#'      \item "auc": area under the curve, for two-class models only
#'      \item "mse": mean squared error
#'      \item "mae": mean absolute error
#'    }
#' @param folds The number of folds to use during cross validation. Default
#'    number of folds is 10.
#'
#' @return A fitted model in object of class janus.
#'
#' @author Alex Wollenschlaeger, \email{alexw@@panix.com}
#'
#' @export
fit_glmnet <- function(x, y, ..., cvfit = TRUE, type_measure = "deviance",
                       folds = 10) {

  # check that necessary data has been supplied and is in correct form
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
    if(cvfit) {
      model <- glmnet::cv.glmnet(as.matrix(x),
                                 as.factor(y),
                                 family = "binomial",
                                 type.measure = type_measure,
                                 nfolds = folds)
      res <- janus(model, package = "glmnet", classifier = "cv.glmnet",
                   interface = "default")
    } else {
      model <- glmnet::glmnet(as.matrix(x),
                              as.factor(y),
                              family = "binomial")
      res <- janus(model, package = "glmnet", classifier = "glmnet",
                   interface = "default")
    }
  } else if (levels > 2) {
    if(cvfit) {
      model <- glmnet::cv.glmnet(as.matrix(x),
                                 as.factor(y),
                                 family = "multinomial",
                                 type.multinomial = "grouped",
                                 type.measure = type_measure,
                                 nfolds = folds)
      res <- janus(model, package = "glmnet", classifier = "cv.glmnet",
                   interface = "default")
    } else {
      model <- glmnet::glmnet(as.matrix(x),
                              as.factor(y),
                              family = "multinomial",
                              type.multinomial = "grouped")
      res <- janus(model, package = "glmnet", classifier = "glmnet",
                   interface = "default")
    }
  } else {
    stop("Unsupported number of levels in factor variable ", sQuote("y"))
  }
  res
}
