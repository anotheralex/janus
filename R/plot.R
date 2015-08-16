#' Plot a janus object.
#'
#' @param object A janus object containing a fitted model.
#' @param ... Additional plotting parameters.
#'
#' @return Nothing
#'
#' @author Alex Wollenschlaeger, \email{alexw@@panix.com}
#'
#' @export
plot.janus <- function(object, ...) {
  if(!inherits(object, "janus")) stop(sQuote("object"), " is not compatible.")

  if(inherits(object, "glmnet") || inherits(object, "cv.glmnet")) {
    plot_glmnet(object$model)
  }
}

