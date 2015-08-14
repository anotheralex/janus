#' Fit a support vector machine classifier using randomForest::randomForest
#'
#' @param formula A formula object.
#' @param data A data frame containing the predictor variables and a categorical
#'    response variable.
#'
#' @return A fitted model in object of class janus.
#'
#' @author Alex Wollenschlaeger, \email{alexw@@panix.com}
#'
#' @export
fit_randomforest <- function(formula, data) {
  model <- randomForest::randomForest(formula = formula, data = data)
  class(model) <- c("janus", class(model))
  model
}
