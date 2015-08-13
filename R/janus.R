#' constructor for janus object
#'
#' @param object a trained model object
#' @param package character string indicating package origin of classifier
#' @param classifier character string indicating the classifier used to train
#'    the model in object
#' @param constructed logical indicating whether this object was created using
#'    the janus constructor
#'
#' @return a janus object containing the trained model object with additional
#'    metadata
#'
#' @author Alex Wollenschlaeger, \email{alexw@@panix.com}
#'
#' @export
janus <- function(object, package, classifier,
                  interface = c("formula", "default"), constructed = TRUE) {
  structure(list(model = object,
                 package = package,
                 classifier = classifier,
                 interface = interface,
                 constructed = constructed),
            class = c("janus", class(object))
  )
}
