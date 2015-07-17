#' fit a model of the required type
#' @param algorithm, one of: glm, svc
#'
fit <- function(algorithm) {
  switch(EXPR = algorithm,
         "logistic" = print("logistic regression in glm"),
         "svm" = print("support vector machine in e1071"))
}
fit("logistic")
fit("svm")
