library(janus)
context("fit classifier")

test_that("error is thrown if formula parameter is missing", {
  expect_error(fit(data = mtcars, classifier = "glm"))
})

test_that("error is thrown if formula parameter not a formula object", {
  y <- matrix(rnorm(n = 10 * 5, mean = 0, sd = 1), nrow = 10, ncol = 5)
  expect_error(fit(formula = y, data = mtcars, classifier = "glm"))
})

test_that("error is thrown if data parameter is missing", {
  expect_error(fit(formula = am ~ mpg, classifier = "glm"))
})

test_that("object of correct type returned by helper functions", {
  model <- .fit_glm(formula = am ~ mpg, data = mtcars)
  expect_is(model, "glm")
  expect_is(model, "janus")

  model <- .fit_randomforest(formula = am ~ mpg, data = mtcars)
  expect_is(model, "randomForest")
  expect_is(model, "janus")

  model <- .fit_e1071(formula = am ~ mpg, data = mtcars)
  expect_is(model, "svm")
  expect_is(model, "janus")
})

test_that("object of correct type returned by fit function", {
  model <- fit(formula = am ~ mpg, data = mtcars, classifier = "glm")
  expect_is(model, "glm")
  expect_is(model, "janus")

  model <- fit(formula = am ~ mpg, data = mtcars, classifier = "randomforest")
  expect_is(model, "randomForest")
  expect_is(model, "janus")

  model <- fit(formula = am ~ mpg, data = mtcars, classifier = "e1071")
  expect_is(model, "svm")
  expect_is(model, "janus")
})

test_that("abbreviated classifier names work", {
  model <- fit(formula = am ~ mpg, data = mtcars, classifier = "g")
  expect_is(model, "glm")
})
