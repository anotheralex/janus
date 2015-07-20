library(janus)
context("fitting classifier")

test_that("formula parameter is not missing", {
  expect_error(fit(data = mtcars, classifier = "glm"), "missing formula")
})

test_that("data parameter is not missing", {
  expect_error(fit(formula = am ~ mpg, classifier = "glm"), "missing data")
})
