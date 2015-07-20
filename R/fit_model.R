#' fit a model of the required type
#' @param formula, a formula object
#' @param data, a data frame
#' @param classifier, a string indicating the classifier to use.
#'    one of: glm, e1071
#'
fit <- function(formula, data, classifier) {
  if(missing(formula)) {stop("missing formula", call. = FALSE)}
  if(missing(data)) {stop("missing data", call. = FALSE)}

  switch(EXPR = classifier,
         "glm" = fit_glm(formula, data),
         "e1071" = fit_e1071(formula, data))
}

#' fit a logistic regression model using stats::glm
#' @param formula, a model formula
#' @param data, a data frame with a categorial output variable
#'
fit_glm <- function(formula, data) {
  stats::glm(formula = formula,
             data = data,
             family = "binomial")
}


#' fit a support vector machine classifier using e1071::svm
#' @param formula, a model formula
#' @param data, a data frame with a categorical output variable
#'
fit_e1071 <- function(formula, data) {
  print("starting e1071...")
}
