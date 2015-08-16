#' Feature selection through filtering.
#'
#' Allows choosing from a range of algorithms for univariate feature selection.
#' Also allows use of the CFS algorithm for choosing the most important feature
#' in a set of predictor variables.
#'
#' @param formula A formula object.
#' @param data A dataframe containing predictor and response variables.
#' @param method A string indicating the filter method to use. One of:
#' \itemize{
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
#' @examples
#' # set up an example dataframe
#' set.seed(1)
#' y <- sample(0:1, 100, replace = TRUE)
#' x1 <- rnorm(100)
#' x2 <- sample(0:1, 100, replace = TRUE)
#' df <- data.frame(y, x1, x2)
#'
#' # use Pearson's correlation for filtering
#' filter(y ~ ., df, method = "pearson")
#'
#' # use Spearman's rank correlation and limit the number of returned variables
#' filter(y ~ ., df, method = "spearman", limit = 1)
#'
#' # use the CFS algorithm to return the most important predictor
#' filter(y ~ ., df, method = "cfs")
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
