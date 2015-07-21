#' fit a model of the specified type
#' currently only supports passing a formula object and data in a data.frame
#'
#' @param formula, a formula object
#' @param data, a data frame
#' @param classifier, a string indicating the classifier to use.
#'    one of: glm, e1071
#' @param subset, vector of indices for the subset to be used in fitting
#' @param ..., additional arguments to be passed to stats::glm
#'
fit <- function(formula, data, classifier, subset, ...) {

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

  # identify correct classifier to fit and call it
  switch(EXPR = classifier,
         "glm" = .fit_glm(formula, data, subset),
         "e1071" = .fit_e1071(formula, data, subset))
}

#' fit a logistic regression model using stats::glm
#' @param formula, a model formula
#' @param data, a data frame with a categorial output variable
#'
.fit_glm <- function(formula, data) {
  stats::glm(formula = formula,
             data = data,
             family = "binomial")
}

#' fit a support vector machine classifier using e1071::svm
#' @param formula, a model formula
#' @param data, a data frame with a categorical output variable
#'
.fit_e1071 <- function(formula, data) {
  print("starting e1071...")
}
