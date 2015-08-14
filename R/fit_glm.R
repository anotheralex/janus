#' fit a logistic regression model using stats::glm
#' @param formula, a model formula
#' @param data, a data frame with a categorial output variable
#' @return fitted model in object of class janus
#' #'
fit_glm <- function(formula, data) {
  model <- stats::glm(formula = formula,
                      data = data,
                      family = "binomial")
  class(model) <- c("janus", class(model))
  model
}
