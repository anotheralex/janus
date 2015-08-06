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
#' @return a vector containining class labels or probabilities, as
#'    determined by the type argument
#'
#' @author Alex Wollenschlaeger, \email{alexw@@panix.com}
#'
#' @export
predict.janus <- function(object,
                          newdata = NULL,
                          type = c("class", "probability"),
                          ...,
                          threshold = 0.5) {
  if(!inherits(object, "janus")) {
    stop(sQuote("object"), "is not a janus object")
  }

  type <- match.arg(type)

  if(inherits(object, "glm")) {
    res <- .predict_glm(object, newdata, type, threshold)
  } else if(inherits(object, "svm")) {
    res <- .predict_e1071(object, newdata, type)
  } else if(inherits(object, "randomForest")) {
    res <- .predict_randomforest(object, newdata, type, threshold)
  }
  res
}

# predict class labels or probabilities for a glm model
.predict_glm <- function(object,
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

.predict_e1071 <- function(object, newdata, type, ...) {

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
                            quote(object$call$data),
                            decision.values = TRUE,
                            probability = TRUE)
      attr(pred_probs, "probalities")
    } else if(type == "class") {
      pred_labels <- predict(object, quote(object$call$data))
      pred_labels
    }
  } else {
    if(type == "probability") {
      pred_probs <- predict(object,
                            newdata,
                            decision.values = TRUE,
                            probability = TRUE)
      attr(pred_prob, "probabilities")
    } else if(type == "class") {
      pred_labels <- predict(object, newdata)
      pred_labels
    }
  }
}

# predict class labels or probabilities for a randomForest model
.predict_randomforest <- function(object,
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
