test_that("coefficients works for blblm", {
  set.seed(1)
  f = blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, num_workers = 4, parallelize = FALSE, fast = TRUE)
  coefficient = as.vector(c(54.36739774, -9.48935809, -0.18379485, 0.04578581))
  names(coefficient) = c("(Intercept)", "wt", "hp", "wt:hp")
  expect_equal(coef(f), coefficient)
})

test_that("coefficients works for blbglm", {
  set.seed(1)
  f = suppressWarnings(blbglm(am ~ wt * hp, data = mtcars, m = 3, B = 100))
  coefficient = as.vector(c(331.1727301, -179.4165808, -0.7906700, 0.8730134))
  names(coefficient) = c("(Intercept)", "wt", "hp", "wt:hp")
  expect_equal(coef(f), coefficient)
})
