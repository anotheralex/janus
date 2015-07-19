#' fit a model of the required type
#' @param algorithm, one of: glm, svc
#'
fit <- function(formula, data, classifier) {
  switch(EXPR = classifier,
         "glm" = fit_glm(formula, data),
         "svm" = print("starting e1071..."))
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
