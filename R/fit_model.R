fit <- function(x, ...) {
  UseMethod("fit")
}

#' fit a model of the specified type
#'
#' @param formula a formula object
#' @param data a data frame
#' @param classifier a string indicating the classifier to use
#' @param subset vector of indices for the subset to be used in fitting
#' @param ... additional arguments to be passed to classifiers

#' @return fitted model in object of class janus
#'
#' @author Alex Wollenschlaeger, \email{alexw@@panix.com}
#'
#' @export
fit.formula <- function(formula,
                data,
                classifier = c("e1071",
                               "glm",
                               "randomforest",
                               "glmnet"),
                subset,
                ...) {

  # check that required parameters are present and of correct class
  if(missing(formula)) {stop(sQuote("formula"), " argument missing")}
  if(!inherits(formula, "formula")) {
    stop(sQuote("formula"), " is not a formula object")
  }

  if(missing(data)) {stop(sQuote("data"), " argument missing")}
  if(!inherits(data, "data.frame")) {
    stop(sQuote("data"), " must be a data.frame")
  }

  # create the data subset if required
  if(!missing(subset)) {
    data <- data[subset, ]
  }

  # get the full-length classifier name
  classifier = match.arg(classifier)

  # identify correct classifier to fit and call it
  switch(EXPR = classifier,
         "e1071" = fit_e1071(formula, data, ...),
         "glm" = fit_glm(formula, data),
         "randomforest" = fit_randomforest(formula, data),
         "glmnet" = fit_glmnet(formula, data, ...)
  )
}

#' fit a model of the specified type
#'
#' Uses a dataframe or matrix of predictor features to train a model using
#' a vector (for binomial) or matrix or dataframe (for multinomial) regression.
#' Currently only supports glmnet. Remaining classifiers use the formula method.
#'
#' @param x a dataframe or matrix containing the predictor variables
#' @param y a vector or matrix of output variables
#' @param classifier a string indicating the classifier to use
#' @param subset vector of indices for the subset to be used in fitting
#' @param ... additional arguments to be passed to classifiers

#' @return fitted model in object of class janus
#'
#' @author Alex Wollenschlaeger, \email{alexw@@panix.com}
#'
#' @export
fit.default <- function(x,
                        y,
                        classifier = c("glmnet"),
                        subset,
                        ...) {

  # check that required parameters are present and of correct class
  if(missing(x)) {stop(sQuote("x"), " argument containing predictors missing")}
  if(missing(y)) {stop(sQuote("y"), " argument containing response missing")}

  # create the data subset if required
  if(!missing(subset)) {
    data <- data[subset, ]
  }

  # get the full-length classifier name
  classifier = match.arg(classifier)

  # identify correct classifier to fit and call it
  switch(EXPR = classifier,
         "glmnet" = fit_glmnet(x, y, ...),
         stop("classifier not supported; retry using formula interface")
  )
}
