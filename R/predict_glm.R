#' Predict response for glm model in janus object.
#'
#' Makes predictions for models trained using the glm classifier in the package
#' stats.
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
predict_glm <- function(object,
                        newdata,
                        type,
                        threshold) {

  # use glm::predict.glm to predict the probabilities associated with
  # either the training data or new data
  if(is.null(newdata)) {
    # predict probabilities for the training data using glm::predict.glm
    pred_probs <- predict.glm(object, type = "response")
  } else {
    pred_probs <- predict.glm(object, newdata = newdata, type = "response")
  }

  if(type == "probability") {
    pred_probs
  } else if(type == "class") {
    pred_labels <- rep(0, length(pred_probs))
    pred_labels[pred_probs > threshold] <- 1
    pred_labels
  }
}
