test_that("predict works for blblm", {
  set.seed(1)
  f = blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, num_workers = 4, parallelize = FALSE, fast = TRUE)
  expect_equal(round(unname(predict(f, data.frame(wt = c(2.5, 3), hp = c(150, 170)))),5), c(20.24445, 18.00496))
})

test_that("predict works for blbglm", {
  set.seed(1)
  f = suppressWarnings(blbglm(am ~ wt * hp, data = mtcars, m = 3, B = 100))
  expect_equal(round(unname(predict(f, data.frame(wt = c(2.5, 3), hp = c(150, 170)))),5), c(91.41081, 103.74593))
})