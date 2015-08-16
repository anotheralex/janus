#' Plot a janus object derived from a glmnet or cv.glmnet object
#'
#' @param x A janus object containing a fitted model.
#' @param ... Additional plotting parameters.
#'
#' @return Nothing
#'
#' @author Alex Wollenschlaeger, \email{alexw@@panix.com}
#'
#' @export
plot_glmnet <- function(x, ...) {
  if(!inherits(x, "glmnet") && !inherits(x, "cv.glmnet")) {
    stop(sQuote("x"), " is not compatible with this method.")
  }
  plot(x)
}
