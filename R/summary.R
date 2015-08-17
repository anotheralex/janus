#' Summarize the output of a janus model
#'
#' Provides a text summary of a fitted janus model, with appropriate output
#'    based on the underlying classifier.
#'
#' @param object An object of class janus
#' @param ... Additional parameters to be passed on.
#'
#' @return None
#'
#' @examples
#' # train a logistic regression model using glm
#' mod <- fit(am ~ mpg, data = mtcars, classifier = "glm")
#'
#' # show a summary for the fitted model
#' summary(mod)
#'
#' @author Alex Wollenschlaeger, \email{alexw@@panix.com}
#'
#' @export
summary.janus <- function(object, ...) {
  if(!is(object, "janus")) stop(sQuote("object"), "is not of class janus.")

  if(is(object, "glm")) {
    sum_obj <- NextMethod("summary", object)
    .summary_glm(sum_obj)
  } else if(is(object, "svm")) {
    .summary_e1071(object)
  } else if(is(object, "randomForest")) {
    .summary_randomforest(object)
  } else if(is(object, "glmnet") | is(object, "cv.glmnet")) {
    .summary_glmnet(object)
  }
}

# Show a summary of the model fitting
# Note: current version uses glm output object directly
.summary_glm <- function(object) {
  cat("\nJanus Model Fitting Summary\n")
  cat("============================\n\n")
  cat("Classifier: glm\n")
  cat("Package: stats\n")
  object
}

.summary_e1071 <- function(object) {
  cat("\nJanus Model Fitting Summary\n")
  cat("============================\n\n")
  cat("Classifier: svm\n")
  cat("Package: e1071\n")
  object
}

# TODO Caputure calling environment to access formula and data argument
.summary_randomforest <- function(object) {
  cat("\nJanus Model Fitting Summary\n")
  cat("============================\n\n")
  cat("Classifier: randomForest\n")
  cat("Package: randomForest\n")
  object
}

.summary_glmnet <- function(object) {
  cat("\nJanus Model Fitting Summary\n")
  cat("============================\n\n")
  cat("Package:", object$package, "\n")
  cat("Classifier:", object$classifier, "\n")
  if(inherits(object, "cv.glmnet")) {
    object$model$glmnet.fit
  } else {
    object$model
  }
}
