#' constructor for janus object
#'
#' @param obj a trained model object
#'
#' @return a janus object containing the trained model object with additional
#'    metadata extracted from the object
#'
#' @author Alex Wollenschlaeger, \email{alexw@@panix.com}
#'
#' @export
janus <- function(obj) {
  structure(list(model = obj,
                 formula = obj$call$formula,
                 data = obj$call$data),
            class = c("janus", class(obj))
  )
}
