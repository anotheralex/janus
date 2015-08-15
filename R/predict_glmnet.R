#' Predict response for glmnet model in janus object.
#'
#' Makes predictions for models trained using the glmnet logistic regression
#' classifier. This function handles both two-class and multinomial logistic
#' regression. By default, the predictions are made using the model with the
#' optimal regularization by setting the glmnet "s" parameter to the minimum
#' value of lambda obtained by cross validation using the cv.glmnet classifier.
#'
#' @param object A janus object containing a fitted glmnet or cv.glmnet model.
#' @param newdata A dataframe or matrix that will be used to predict either
#'    probabilities for each class or class labels. If supplied as a dataframe,
#'    it will be coerced to a matrix. This is required for obtained predictions
#'    for either testing or training data.
#' @param type A string indicating the desired output: either "class" for class
#'    labels or "probabilities" for probabilities.
#' @param ... Arguments to be passed on to underlying functions.
#' @param threshold Probability threshold for binary classification. Th default
#'    value is 0.5.
#'
#' @return A vector or dataframe containining class labels or probabilities, as
#'    determined by the type argument.
#'
#' @author Alex Wollenschlaeger, \email{alexw@@panix.com}
#'
#' @export
predict_glmnet <- function(object, newdata, type,  ..., threshold) {

  # for glmnet, we need to ensure that the data is passed in explicitly
  if(is.null(newdata) || missing(newdata)) {
    stop(sQuote("newdata"), " missing. Retry with new data object.")
  }

  # require new data to be in a matrix
  # if it is not, coerce into one
  if(!is.matrix(newdata)) {
    message(sQuote("newdata"), " is not a matrix. Being coerced.")
  }

  # use glmnet::glmnet, glmnet::cvfit and glmnet::predict
  # to predict probabilities or classes for trained model
  if(inherits(object, "cv.glmnet")) {
    if(type == "probability") {
      pred_prob <- predict(object$model,
                           newx = as.matrix(newdata),
                           s = "lambda.min",
                           type = "response")
      pred_prob
    } else if(type == "class") {
      pred_class <- predict(object$model,
                            newx = as.matrix(newdata),
                            s = "lambda.min",
                            type = "class")
      pred_class
    }
  } else if(inherits(object, "glmnet")) {
    if(type == "probability") {
      pred_prob <- predict(object$model,
                           newx = as.matrix(newdata),
                           type = "response")
      pred_prob
    } else if(type == "class") {
      pred_class <- predict(object$model,
                            newx = as.matrix(newdata),
                            type = "class")
      pred_class
    }
  } else {
    stop("Object is from an unsupported class.")
  }
}
