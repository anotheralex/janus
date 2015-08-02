#' Predict probabilities or class labels for a fitted janus model
#'
#' Uses a janus model object to predict class labels. If new data is supplied,
#' the class labels or probabilities for each option are returned. Otherwise
#' the label or probabilities for the training data are predicted.
#'
#' @param object a janus object containing a fitted model, as produced by fit()
#' @param newdata a dataframe that will be used to predict class labels
#' @param type a string indicating the desired output, either class labels or
#'    probabilities
#' @param ... arguments to be passed on to underlying functions
#' @param threshold probability threshold for binary classification,
#'    defaults to 0.5
#'
#' @return a data structure containining class labels or probabilities, as
#'    determined by the type argument
#'
#' @author Alex Wollenschlaeger, \email{alexw@@panix.com}
#'
#' @export
predict.janus <- function(object,
                          newdata,
                          type = c("class", "probability"),
                          ...,
                          threshold = 0.5) {
  if(!inherits(object, "janus")) stop(sQuote("object"), "is not a janus object")

  type <- match.arg(type)

  if(inherits(object, "glm")) {
    res <- .predict_glm(object, newdata, type, threshold)
  } else if(inherits(object, "svm")) {
    res <- .predict_e1071(object, newdata, type)
  } else if(inherits(object, "randomForest")) {
    res <- .predict_randomforest(object, newdata, type)
  }
}

# predict class labels or probabilities for a glm model
.predict_glm <- function(object, newdata, type, threshold) {
  if(missing(newdata)) {
    # predict probabilities for the training data using glm::predict.glm
    pred_probs <- predict.glm(object, type = "response")

    if(type == "probability") {
      pred_probs
    } else if(type == "class") {
      pred_labels <- rep(0, length(pred_probs))
      pred_labels[pred_probs > threshold] <- 1
      pred_labels
    }
  }
}

.predict_e1071 <- function(object, newdata, type) {
  #TODO
}

.predict_randomforest <- function(object, newdata, type) {
  #TODO
}
