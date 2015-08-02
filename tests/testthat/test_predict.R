library(janus)
context("make predictions based on fitted model")

test_that("error thrown object of incompatible class passed as argument", {
  # create an object of class lm
  obj <- lm(mpg ~ ., data = mtcars)
  expect_error(predict.janus(obj))
})

test_that("higher threshold leads to fewer 1 labels in 2-class problem", {
  mod <- fit(am ~ mpg, data = mtcars[complete.cases(mtcars), ],
             classifier = "glm")
  low_thresh <- predict(mod, type = "class", threshold = 0.2)
  high_thresh <- predict(mod, type = "class", threshold = 0.8)
  expect_true(sum(low_thresh) > sum(high_thresh))
})
