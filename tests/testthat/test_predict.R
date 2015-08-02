library(janus)
context("make predictions based on fitted model")

test_that("error thrown object of incompatible class passed as argument", {
  # create an object of class lm
  obj <- lm(mpg ~ ., data = mtcars)
  expect_error(predict.janus(obj))
})
