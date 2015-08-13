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
         "e1071" = .fit_e1071(formula, data, ...),
         "glm" = .fit_glm(formula, data),
         "randomforest" = .fit_randomforest(formula, data),
         "glmnet" = .fit_glmnet(formula, data, ...)
  )

}

#' fit a support vector machine classifier
#'
#' Uses e1071::svm to fit a support vector machine. Note that the type
#' argument to svm is set to force classification. Also, the function uses
#' linear as the default to the kernel argument rather than radial as in native
#' svm. The cost parameter retains its default (i.e. cost = 1) and can be
#' modified by passing through a different value.
#'
#' @param formula, a model formula
#' @param data, a data frame with a categorical output variable
#' @param ... arguments to pass to called functions
#'
#' @return fitted model in object of class janus
#'
.fit_e1071 <- function(formula, data, ...) {
  # TODO: check documentation for requirements to default to classification
  # response variable needs to be a factor to give classification
  # note that default kernel is linear not radial as in e1071::svm
  model <- e1071::svm(formula = formula,
                      data = data,
                      kernel = "linear",
                      type = "C-classification",
                      probability = TRUE)
  class(model) <- c("janus", class(model))
  model
}

#' fit a logistic regression model using stats::glm
#' @param formula, a model formula
#' @param data, a data frame with a categorial output variable
#' @return fitted model in object of class janus
#' #'
.fit_glm <- function(formula, data) {
  model <- stats::glm(formula = formula,
             data = data,
             family = "binomial")
  class(model) <- c("janus", class(model))
  model
}

#' fit a support vector machine classifier using randomForest::randomForest
#' @param formula, a model formula
#' @param data, a data frame with a categorical output variable
#' @return fitted model in object of class janus
#'
.fit_randomforest <- function(formula, data) {
  model <- randomForest::randomForest(formula = formula, data = data)
  class(model) <- c("janus", class(model))
  model
}

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
.fit_glmnet <- function(x, y, ...) {

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
         "glmnet" = .fit_glmnet(x, y, ...),
         stop("classifier not supported; retry using formula interface")
  )
}
