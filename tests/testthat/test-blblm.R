test_that("test blblm", {
  f = blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, num_workers = 4, parallelize = FALSE, fast = TRUE)
  expect_equal(length(f$estimates), 3)
  expect_equal(length(f$estimates$`1`), 100)
  expect_equal(length(f$estimates$`2`), 100)
  expect_equal(length(f$estimates$`3`), 100)
})

test_that("test blbglm", {
  f = suppressWarnings(blbglm(am ~ wt * hp, data = mtcars, m = 3, B = 100))
  expect_equal(length(f$estimates), 3)
  expect_equal(length(f$estimates$`1`), 100)
  expect_equal(length(f$estimates$`2`), 100)
  expect_equal(length(f$estimates$`3`), 100)
})
