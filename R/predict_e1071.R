#' Predict response for svm model in janus object.
#'
#' Makes predictions for models trained using the svm classifier in the package
#' e1071.
#'
#' @param object A janus object containing a fitted glmnet or cv.glmnet model.
#' @param newdata A dataframe that will be used to predict either probabilities
#'   for each class or class labels.
#' @param type A string indicating the desired output: either "class" for class
#'   labels or "probabilities" for probabilities.
#' @param ... Arguments to be passed on to underlying functions.
#' @param threshold Probability threshold for binary classification. Th default
#'   value is 0.5.
#'
#' @return A vector or dataframe containining class labels or probabilities, as
#'   determined by the type argument.
#'
#' @author Alex Wollenschlaeger, \email{alexw@@panix.com}
#'
#' @export
predict_e1071 <- function(object, newdata, type, ...) {

  # set the class of the object to that of e1071 svm object
  # required to gain access to the correct predict function
  class(object) <- c("svm.formula", "svm")

  # use e1071::predict.svm to predict the probabilities associated with
  # either the training data or new data
  # note that there is a fair amount of code duplication here but it is
  # necessary to prevent the copying of potentially large data objects
  # since R does not use pointers
  if(is.null(newdata)) {

    if(type == "probability") {
      # predict probabilities for the training data
      # need to identify the data from the fitted model object
      pred_probs <- predict(object,
                            object$call$data,
                            decision.values = TRUE,
                            probability = TRUE)
      attr(pred_probs, "probalities")
    } else if(type == "class") {
      pred_labels <- predict(object, quote(object$call$data))
      pred_labels
    } else {
      stop("Unknown ", sQuote("type"))
    }
  } else {
    if(type == "probability") {
      pred_probs <- predict(object,
                            newdata,
                            decision.values = TRUE,
                            probability = TRUE)
      attr(pred_probs, "probabilities")
    } else if(type == "class") {
      pred_labels <- predict(object, newdata)
      pred_labels
    } else {
      stop("Unknown ", sQuote("type"))
    }
  }
}
