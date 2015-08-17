#' Plot a janus object.
#'
#' @param x A janus object containing a fitted model.
#' @param ... Additional plotting parameters.
#'
#' @return Nothing
#'
#' @examples
#' # create predictor data matrix and response data vector
#' x <- mtcars[, -9]
#' y <- mtcars[, 9]
#'
#' # fit a regularlized cv.glmnet classifier, using cross-validation for
#' # determining the optimal value for model hyperparameters
#' mod <- fit(x, y, classifier = "glmnet")
#'
#' # plot the model error measure against values for the penalty glmnet penalty
#' # parameter
#' plot(mod)
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

