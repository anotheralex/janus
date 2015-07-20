library(janus)
context("fitting classifier")

test_that("formula and data parameters are not empty", {
  expect_error(fit(classifier = "glm", "missing formula"))
})
