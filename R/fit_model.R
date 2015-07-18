#' fit a model of the required type
#' @param algorithm, one of: glm, svc
#'
fit <- function(formula, data, algorithm) {
  switch(EXPR = algorithm,
         "logistic" = stats::glm(formula = formula,
                                 data = data,
                                 family = "binomial"),
         "svm" = print("running support vector machine in e1071..."))
}
