#' Constructor for janus object
#'
#' A constructor for creating a janus object. The principle argument is the
#' trained model object, which is packaged inside a janus object along with
#' metadata derived from the fitting process.
#'
#' @param object A trained model object.
#' @param package Character string indicating package origin of classifier.
#' @param classifier Character string indicating the classifier used to train
#'   the model in object.
#' @param interface String indicating whether the object was created using the
#'   formula method interface or the default interface.
#' @param constructed Logical indicating whether this object was created using
#'   the janus constructor.
#'
#' @return A janus object containing the trained model object with additional
#'   metadata.
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
