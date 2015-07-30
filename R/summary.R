#' Summarize the output of a janus model
#'
summary.janus <- function(object) {
  if(!is(object, "janus")) stop(sQuote("object"), "is not of class janus.")

  if(is(object, "glm")) {
    print("janus-glm object")
  } else if(is(object, "e1071")) {
    print("janus-e1071 object")
  } else if(is(object, "randomForest")) {
    print("janus-randomforest object")
  }
}
