#' fit a support vector machine classifier using randomForest::randomForest
#' @param formula, a model formula
#' @param data, a data frame with a categorical output variable
#' @return fitted model in object of class janus
#'
fit_randomforest <- function(formula, data) {
  model <- randomForest::randomForest(formula = formula, data = data)
  class(model) <- c("janus", class(model))
  model
}
