test_that("confint works for blblm", {
  set.seed(1)
  f = blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, num_workers = 4, parallelize = FALSE, fast = TRUE)
  newsmthing = matrix(c(c(-13.34869743, -0.26538230, 0.01409981), c(-5.79080007, -0.07330356, 0.06864059)), nrow = 3, ncol = 2, dimnames = list(c("wt", "hp", "wt:hp"), c("2.5%", "97.5%")))
  expect_equal(round(confint(f), 8), newsmthing)
})

test_that("confint works for blbglm", {
  set.seed(1)
  f = suppressWarnings(blbglm(am ~ wt * hp, data = mtcars, m = 3, B = 100))
  newsmthing = matrix(c(c(-82.9774612, -1.5444788, -0.2000994), c(-0.08805053, 0.76388879, 0.43768425)), nrow = 3, ncol = 2, dimnames = list(c("wt", "hp", "wt:hp"), c("2.5%", "97.5%")))
  expect_equal(round(confint(f), 8), newsmthing)
})
