#' fit a model of the specified type
#' currently only supports passing a formula object and data in a data.frame
#'
#' @param formula, a formula object
#' @param data, a data frame
#' @param classifier, a string indicating the classifier to use
#' @param subset, vector of indices for the subset to be used in fitting
#' @param ..., additional arguments to be passed to classifiers
#' @return fitted model in object of class janus
#'
fit <- function(formula, data, classifier = c("e1071", "glm", "randomforest"),
                subset, ...) {

  # check that required parameters are present and of correct class
  if(missing(formula)) {stop("missing formula", call. = FALSE)}
  if(!inherits(formula, "formula")) {
    stop("object must be a formula", call. = FALSE)
  }

  if(missing(data)) {stop("missing data", call. = FALSE)}
  if(!inherits(data, "data.frame")) {
    stop("data must be in a data.frame", call. = FALSE)
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
         "randomforest" = .fit_randomforest(formula, data)
  )

}

#' fit a support vector machine classifier using e1071::svm
#' @param formula, a model formula
#' @param data, a data frame with a categorical output variable
#' @param kernel, kernel to be used for training model and making predictions
#' @return fitted model in object of class janus
#'
.fit_e1071 <- function(formula, data, kernel = "linear") {
  # TODO: check documentation for requirements to default to classification
  # response variable needs to be a factor to give classification
  model <- e1071::svm(formula = formula, data = data, kernel = kernel)
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
