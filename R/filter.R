#' Feature selection through filtering
#'
#' @param formula A formula object.
#' @param data A dataframe containing predictor and response variables.
#' @param method A string indicating the filter method to use. One of:
#' \enumerate{
#'   \item "cfs": correlation-based feature selector (from FSelector package)
#'   \item "chisq": chi-square (FSelector)
#'   \item "gainratio": information gain ratio (FSelector)
#'   \item "infogain": information gain (FSelector)
#'   \item "pearson": Pearson correlation (FSelector)
#'   \item "spearman": Spearman rank-based correlation (FSelector)
#' }
#' @param ... Arguments to be passed on to called functions.
#' @param limit Optional integer specifying the number of features to retain.
#'
#' @return A vector of feature importance values or predictor identifiers,
#'   depending on the chosen method.
#'
#' @author Alex Wollenschlaeger, \email{alexw@@panix.com}
#'
#' @export
filter <- function(formula,
                   data,
                   method = c("pearson", "spearman", "chisq", "infogain",
                              "gainratio", "cfs"),
                   ...,
                   limit = NULL
                   ) {

  # test all arguments are supplied
  if(missing(formula)) stop(sQuote("formula"), " is missing")
  if(missing(data)) stop(sQuote("data"), " is missing")
  if(missing(method)) stop(sQuote("method"), " is missing")
  if(!is.null(limit) && (limit < 1)) stop(sQuote("limit"),
                                          " must be greater than 0")

  # get full argument
  method <- match.arg(method)

  # determine which filter method to use
  switch(EXPR = method,
    "pearson" = filter_pearson(formula, data, limit),
    "spearman" = filter_spearman(formula, data, limit),
    "chisq" = filter_chisq(formula, data, limit),
    "infogain" = filter_infogain(formula, data, limit),
    "gainratio" = filter_gainratio(formula, data, limit),
    "cfs" = filter_cfs(formula, data),
    stop("Unknown method")
  )
}
