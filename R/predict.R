#' Predict probabilities or class labels for a fitted janus model
#'
#' Uses a janus model object to predict class labels. If new data is supplied,
#' the class labels or probabilities for each option are returned. Otherwise
#' the label or probabilities for the training data are predicted.
#'
#' Note that for some models, the new data must be explicitly provided. These
#' are indicated by warning messages when run without a new data object.
#'
#' @param object a janus object containing a fitted model, as produced by fit
#' @param newdata a dataframe that will be used to predict class labels
#'    required for prediction of training samples using support vector machine
#'    models implemented by the e1071 package
#' @param type a string indicating the desired output, either class labels or
#'    probabilities
#' @param ... arguments to be passed on to underlying functions
#' @param threshold probability threshold for binary classification. Th default
#'    value is 0.5
#'
#' @return a vector or dataframe containining class labels or probabilities, as
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
    res <- predict_glm(object, newdata, type, threshold)
  } else if(inherits(object, "svm")) {
    res <- predict_e1071(object, newdata, type)
  } else if(inherits(object, "randomForest")) {
    res <- predict_randomforest(object, newdata, type, threshold)
  } else if(inherits(object, "glmnet") || inherits(object, "cv.glmnet")) {
    res <- predict_glmnet(object, newdata, type, threshold)
  }
  res
}
