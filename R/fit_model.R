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
#' @param formula a model formula
#' @param data a data frame with a categorical output variable
#' @param ... additional parameters to pass to glmnet
#'
#' @return fitted model in object of class janus
#'
#' @author Alex Wollenschlaeger, \email{alexw@@panix.com}
#'
#' @export
.fit_glmnet <- function(formula, data, ...) {
  model <- glmnet::glmnet(formula = formula, data = data, ...)
  class(model) <- c("janus", class(model))
  model
}
