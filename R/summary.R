#' Summarize the output of a janus model
#'
#' Provides a text summary of a fitted janus model, with appropriate output
#'    based on the underlying classifier.
#'
#' @param object an object of class janus
#'
#' @return None
#'
#' @author Alex Wollenschlaeger, \email{alexw@@panix.com}
#'
#' @examples
#' summary(object)
#'
#' @export
summary.janus <- function(object) {
  if(!is(object, "janus")) stop(sQuote("object"), "is not of class janus.")

  if(is(object, "glm")) {
    .summary_glm(object)
  } else if(is(object, "svm")) {
    .summary_e1071(object)
  } else if(is(object, "randomForest")) {
    .summary_randomforest(object)
  }
}

.summary_glm <- function(object) {
  print("janus-glm object")
}

.summary_e1071 <- function(object) {
  print("janus-e1071 object")
}

.summary_randomforest <- function(object) {
  print("janus-randomforest object")
}
