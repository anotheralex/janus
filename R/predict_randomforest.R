# predict class labels or probabilities for a randomForest model
predict_randomforest <- function(object,
                                 newdata,
                                 type,
                                 threshold) {

  # use randomForest::randomForest and randomForest::predcict.randomForest
  # to predict the probabilities associated with either the training data or
  # new data

  # randomForest does not allow direct access to its predict method
  # and it does not work with a janus object directly. Therefore, as a
  # temporary workaround, we will change the class of the janus
  # object. Since R does makes a copy of the passed object, this does not affect
  # the model object created by fit()
  # NOTE: THIS IS A KLUDGE AND A REAL FIX IS NEEDED

  # assign the same class that a randomForest object has
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
