#' Generic method for expanding the formula associated with a model object
expand_formula <- function(model) {
  UseMethod("expand_formula")
}

#' Expand the formula associated with a janus fitted model object
#'
#' If a formula is passed to fit in a variable, the variable will be stored
#'    in the resulting fitted model object's call variable. This method is a
#'    convenient utility for extracting and expanding the formula from a fitted
#'    janus model object.
#'
#' @param model a janus fitted model
#'
#' @return a formula object containing the expanded formula
#'
#' @author Alex Wollenschlaeger, \email{alexw@@panix.com}
#'
#' @export
expand_formula.janus <- function(model) {
  expanded <- eval(model$call$formula)
  expanded
}
