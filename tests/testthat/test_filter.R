# tests for filter functions
library("janus")
context("filter features")

# set up example data
y = sample(0:1, 100, replace = TRUE)
x1 = rnorm(100)
x2 = sample(0:1, 100, replace = TRUE)
df <- data.frame(y, x1, x2)

test_that("error is thrown if argument is missing", {
  expect_error(filter(data = df, method = "pearson"))
  expect_error(filter(formula = y ~ ., method = "pearson"))
  expect_error(filter(formula = y ~ ., data = df))
})

test_that("setting the limit parameter shows a reduced set (if possible)", {
  expect_equal(filter(y ~ ., data = df, method = "pearson"), 2)
  expect_equal(filter(y ~ ., data = df, method = "pearson", limit = 1), 1)
  expect_equal(filter(y ~ ., data = df, method = "pearson", limit = 3), 2)
})
