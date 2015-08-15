#' Predict response for randomForest model in janus object.
#'
#' Makes predictions for models trained using the randomForest classifier.
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
predict_randomforest <- function(object,
                                 newdata,
                                 type,
                                 threshold) {

  # use randomForest::randomForest and randomForest::predict.randomForest
  # to predict the probabilities associated with either the training data or
  # new data

  # randomForest does not allow direct access to its predict method and it does
  # not work with a janus object directly. Re-assign the same class that a
  # randomForest object has to this copy of the object
  class(object) <- c("randomForest.formula", "randomForest")

  if(is.null(newdata)) {
    if(type == "probability") {
      pred_probs <- predict(object, type = "prob")
      pred_probs
    } else if(type == "class") {
      pred_labels <- predict(object, type = "response")
      pred_labels
    }
  } else {
    if(type == "probability") {
      pred_probs <- predict(object, newdata, type = "prob")
      pred_probs
    } else if(type == "class") {
      pred_labels <- predict(object, newdata, type = "response")
      pred_labels
    }
  }
}
