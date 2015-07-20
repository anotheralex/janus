library(janus)
context("fitting classifier")

test_that("formula parameter is not missing", {
  expect_error(fit(classifier = "glm", "missing formula"))
})
