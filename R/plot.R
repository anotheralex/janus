#' Plot a janus object.
#'
#' @param x A janus object containing a fitted model.
#' @param ... Additional plotting parameters.
#'
#' @return Nothing
#'
#' @author Alex Wollenschlaeger, \email{alexw@@panix.com}
#'
#' @export
plot.janus <- function(x, ...) {
  if(!inherits(x, "janus")) stop(sQuote("x"), " is not compatible.")

  if(inherits(x, "glmnet") || inherits(x, "cv.glmnet")) {
    plot_glmnet(x$model)
  }
}

