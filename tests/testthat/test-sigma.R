test_that("sigma works", {
  set.seed(1)
  f = blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, num_workers = 4, parallelize = FALSE, fast = TRUE)
  expect_equal(round(sigma(f), 6), 1.651795)
})

test_that("sigma works", {
  set.seed(1)
  f = suppressWarnings(blbglm(am ~ wt * hp, data = mtcars, m = 3, B = 100))
  expect_equal(round(sigma(f), 6), 2.380669)
})
