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
fit_e1071 <- function(formula, data, ...) {
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
