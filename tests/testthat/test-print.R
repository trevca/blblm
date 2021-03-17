test_that("blblm print works", {
  f = blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, num_workers = 4, parallelize = FALSE, fast = TRUE)
  expect_output(print(f), "blblm model: mpg ~ wt * hp", fixed = TRUE)
})

test_that("blbglm print works", {
  f = suppressWarnings(blbglm(am ~ wt * hp, data = mtcars, m = 3, B = 100))
  expect_output(print(f), "blbglm model: am ~ wt * hp", fixed = TRUE)
})