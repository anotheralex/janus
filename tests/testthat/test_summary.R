library(janus)
context("summarize fitted model")

test_that("error is thrown if non-janus object is given as parameter", {
  mod <- lm(mpg ~ ., data = mtcars)
  expect_error(summary.janus(mod))
})
