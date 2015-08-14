#' Fit a two-class logistic regression model using stats::glm
#'
#' @param formula A formula object.
#' @param data A dataframe containing the predictor variables and a categorial
#'    output variable.

#' @return A fitted model in object of class janus.
#'
#' @author Alex Wollenschlaeger, \email{alexw@@panix.com}
#'
#' @export
fit_glm <- function(formula, data) {
  model <- stats::glm(formula = formula,
                      data = data,
                      family = "binomial")
  class(model) <- c("janus", class(model))
  model
}
