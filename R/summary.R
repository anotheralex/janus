#' Summarize the output of janus model
#'
summary.janus <- function(object) {
  if(!is(object, "janus")) stop(sQuote("object"), "is not of class janus.")

  print("janus object extends unknown class.")
}
