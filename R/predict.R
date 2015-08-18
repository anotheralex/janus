#' Predict probabilities or class labels for a fitted janus model.
#'
#' Uses a janus model object to predict class labels. If new data is supplied,
#' the class labels or probabilities for each option are returned. Otherwise the
#' label or probabilities for the training data are predicted.
#'
#' Note that for some models, the new data must be explicitly provided. These
#' are indicated by warning messages when run without a new data object.
#'
#' @param object janus object containing a fitted model, as produced by fit
#' @param newdata Dataframe that will be used to predict class labels required
#'   for prediction of training samples using support vector machine models
#'   implemented by the e1071 package.
#' @param type String indicating the desired output, either class labels or
#'   probabilities.
#' @param ... Arguments to be passed on to underlying functions
#' @param threshold Probability threshold for binary classification. Th default
#'   value is 0.5.
#'
#' @return Vector or dataframe containining class labels or probabilities, as
#'   determined by the type argument.
#'
#' @seealso Helper functions: \code{\link{predict_e1071}},
#'   \code{\link{predict_glm}}, \code{\link{predict_glmnet}},
#'   \code{\link{predict_randomforest}}
#'
#' @examples
#' # create a dataframe containing no missing values
#' mt_complete <- mtcars[complete.cases(mtcars), ]
#'
#' # train a logistic regression model using glm
#' mod <- fit(am ~ mpg, data = mt_complete, classifier = "glm")
#'
#' # predict the class labels for the training data
#' predict(mod, type = "class")
#'
#' # create train and test datasets
#' train_index <- sample(nrow(mt_complete), size = nrow(mt_complete)/2)
#' train_df <- mt_complete[train_index, ]
#' test_df <- mt_complete[-train_index, ]
#'
#' # train a randomforest classifier on the training data
#' mod <- fit(as.factor(am) ~ ., data = train_df, classifier = "randomforest")
#'
#' # predict the class probabilities for the test data
#' predict(mod, newdata = test_df, type = "probability")
#'
#' # predict the classes for the test data
#' predict(mod, newdata = test_df, type = "class")
#'
#' # create predictor data matrix and response data vector
#' x <- mtcars[, -9]
#' y <- mtcars[, 9]
#'
#' # fit a regularlized cv.glmnet classifier, using cross-validation for
#' # determining the optimal value for model hyperparameters
#' mod <- fit(x, y, classifier = "glmnet")
#'
#' # predict the class probabilities for the training data
#' predict(mod, newdata = x, type = "probability")
#'
#' # predict the class labels for the training data
#' predict(mod, newdata = x, type = "class")
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
