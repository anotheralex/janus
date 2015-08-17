#' Fit a classification model.
#'
#' Generic method for fitting a classification model.
#'
#' @param x Either a formula object or a database or matrix object.
#' @param ... Additional parameters to be passed on.
#'
#' @return A fitted model in object of class janus.
#'
#' @examples
#' # create a dataframe containing no missing values
#' mt_complete <- mtcars[complete.cases(mtcars), ]
#'
#' # train a logistic regression model using glm
#' fit(am ~ mpg, data = mt_complete, classifier = "glm")
#'
#' # train a random forest classifier using randomForest
#' fit(as.factor(am) ~ ., data = mt_complete, classifier = "randomforest")
#'
#' # train a support vector machine with linear kernel using e1071::svm
#' fit(am ~ ., data = mt_complete, classifier = "e1071")
#'
#' @author Alex Wollenschlaeger, \email{alexw@@panix.com}
#'
#' @export
fit <- function(x, ...) {
  UseMethod("fit")
}

#' Fit a classification model using a formula object.
#'
#' @param formula A formula object.
#' @param data A dataframe containing the predictor and response variables.
#' @param classifier A string indicating the classifier to use. Available
#'    options (and source package) include:
#' \enumerate{
#'   \item "glm": logistic regression (glm)
#'   \item "randomForest": random forest (randomForest)
#'   \item "e1071": support vector machine (e1071)
#' }
#'
#' @param subset Vector of indices for the subset to be used in fitting.
#' @param ... Additional arguments to be passed to classifiers.

#' @return A fitted model in an object of class janus.
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

#' Fit a classification model using separate predictor and
#'    response variables.
#'
#' Uses a dataframe or matrix of predictor features to train a model using
#' a vector (for binomial) or matrix or dataframe (for multinomial) regression.
#'
#' @param x A dataframe or matrix containing the predictor variables.
#' @param y A vector or matrix of output variables.
#' @param classifier A string indicating the classifier to use. Available
#'    options (and source package) include:
#' \enumerate{
#'   \item "glm": logistic regression (glm)
#'   \item "glmnet": binomial or multinomial logistic regression (glmnet)
#'   \item "randomForest": random forest (randomForest)
#'   \item "e1071": support vector machine (e1071)
#' }
#'
#' @param subset Vector of indices for the subset to be used in fitting.
#' @param ... Additional arguments to be passed to classifiers.

#' @return A fitted model in an object of class janus.
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
