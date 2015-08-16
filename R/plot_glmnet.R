#' Plot a janus object derived from a glmnet or cv.glmnet object
#'
#' @param object A janus object containing a fitted model.
#' @param ... Additional plotting parameters.
#'
#' @return Nothing
#'
#' @author Alex Wollenschlaeger, \email{alexw@@panix.com}
#'
#' @export
plot_glmnet <- function(object, ...) {
  if(!inherits(object, "glmnet") && !inherits(object, "cv.glmnet")) {
    stop(sQuote("object"), " is not compatible with this method.")
  }
  plot(object)
}
