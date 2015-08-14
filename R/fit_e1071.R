#' Fit a support vector machine classifier using e1071::svm.
#'
#' Uses svm from the e1071 package to fit a support vector machine. The type
#' argument to svm is set to force classification. Also, the function uses
#' linear as the default to the kernel argument rather than radial as in native
#' svm. The cost parameter retains its default (i.e. cost = 1) and can be
#' modified by passing through a different value.
#'
#' @param formula A formula object.
#' @param data A dataframe containing the predictor variables and a categorical
#'    response variable.
#' @param ... Arguments to pass to called functions.
#'
#' @return A fitted model in object of class janus.
#'
#' @author Alex Wollenschlaeger, \email{alexw@@panix.com}
#'
#' @export
fit_e1071 <- function(formula, data, ...) {
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
