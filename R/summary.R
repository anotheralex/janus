#' Summarize the output of a janus model
#'
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
