# predict class labels or probabilities for a glm model
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
