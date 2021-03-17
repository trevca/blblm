#' @import purrr
#' @import furrr
#' @import parallel
#' @import stats
#' @importFrom magrittr %>%
#' @details
#' Linear and Logistic Regression models with Little Bag of Bootstraps
"_PACKAGE"

## quiets concerns of R CMD check re: the .'s that appear in pipelines
# from https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
utils::globalVariables(c("."))

#' BLBLM
#'
#' @param formula, the formula form of the desired linear regression.
#' @param data, the data to pull the formula variables from.
#' @param m, the number of subsets of "little bootstraps" to run, defaults to `10`.
#' @param B, the number of times to bootstrap the data, defaults to `5000`.
#' @param seed_value, an initial seed value, defaults to `100`, for reproduceability.
#' @param num_workers, the number of workers if parallelized, defaults to the number of computer cores. This value doesn't matter if parallelize is set to `FALSE`.
#' @param parallelize, whether or not to parallelize the function, defaults to `TRUE`.
#' @param fast, whether or not to use the faster rcpp version of the weighted linear regression function. Defaults to `TRUE`.
#' @examples
#' blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, num_workers = 4, parallelize = FALSE, fast = FALSE)
#' @export
blblm <- function(formula, data, m = 10, B = 5000, seed_value = 100, num_workers = detectCores(), parallelize = TRUE, fast = TRUE) {
  data_list <- split_data(data, m)
  set.seed(seed_value)
  if (parallelize) {
    suppressWarnings(plan(multiprocess, workers = num_workers))
    options(future.rng.onMisuse = "ignore")
    estimates <- future_map(
      data_list,
      ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B, fast = fast),
      .options = furrr_options(seed = seed_value)
    )
  } else {
    estimates <- map(
      data_list,
      ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B, fast = fast)
    )
  }
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"

  invisible(res)
}

# split data into m parts of approximated equal sizes
split_data <- function(data, m) {
  idx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(idx)
}


# compute the estimates
lm_each_subsample <- function(formula, data, n, B, fast = FALSE) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wrong variable from the global scope.
  environment(formula) <- environment()
  m <- model.frame(formula, data)
  X <- model.matrix(formula, m)
  y <- model.response(m)
  if (fast) {
    replicate(B, lmFast(X, y, n), simplify = FALSE)
  } else {
    replicate(B, lm1(X, y, n), simplify = FALSE)
  }
}

# computes the regression estimates for a blb dataset faster using cpp
lmFast <- function(X, y, n) {
  freqs <- as.vector(rmultinom(1, n, rep(1, nrow(X))))
  fit <- fastLm_impl(X, y, freqs)
  coefficient <- as.vector(fit$coefficients)
  names(coefficient) <- colnames(X)
  sig <- sqrt(sigmaHelper(X, y, fit))
  list(coef = coefficient, sigma = sig)
}

# compute the regression estimates for a blb dataset
lm1 <- function(X, y, n) {
  freqs <- as.vector(rmultinom(1, n, rep(1, nrow(X))))
  fit <- lm.wfit(X, y, freqs)
  list(coef = blbcoef(fit), sigma = blbsigma(fit))
}


# compute the coefficients from fit
blbcoef <- function(fit) {
  coef(fit)
}


# compute sigma from fit
blbsigma <- function(fit) {
  p <- fit$rank
  e <- na.omit(fit$residuals)
  w <- na.omit(fit$weights)
  s <- sum(w * (e^2)) / (sum(w) - p)
  sqrt(abs(s))
}


#' Prints BLBLM
#'
#' @param x the model to be printed
#' @param ... extra arguments
#' @examples
#' blblm_model <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, num_workers = 4, parallelize = FALSE, fast = FALSE)
#' print(blblm_model)
#' @export
#' @method print blblm
print.blblm <- function(x, ...) {
  cat("blblm model:", capture.output(x$formula))
  cat("\n")
}


#' Gets sigma for BLBLM
#'
#' @param object the blblm model to use
#' @param confidence whether or not to include a confidence interval
#' @param level the confidence level to be used if confidence is `TRUE`, defaults to `0.95`
#' @param ... extra arguments
#' @examples
#' blblm_model <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, num_workers = 4, parallelize = FALSE, fast = FALSE)
#' sigma(blblm_model)
#' sigma(blblm_model, confidence = TRUE)
#' sigma(blblm_model, confidence = TRUE, level = 0.99)
#' @export
#' @method sigma blblm
sigma.blblm <- function(object, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  sigma <- mean(map_dbl(est, ~ mean(map_dbl(., "sigma"))))
  if (confidence) {
    alpha <- 1 - level
    limits <- est %>%
      map_mean(~ quantile(map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2))) %>%
      set_names(NULL)
    return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
  } else {
    return(sigma)
  }
}

#' Gets coefficients for BLBLM
#'
#' @param object the blblm model to use
#' @param ... extra arguments
#'
#' @examples
#' blblm_model <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, num_workers = 4, parallelize = FALSE, fast = FALSE)
#' coef(blblm_model)
#' @export
#' @method coef blblm
coef.blblm <- function(object, ...) {
  est <- object$estimates
  return(map_mean(est, ~ map_cbind(., "coef") %>% rowMeans()))
}


#' Gets confident interval for BLBLM variables
#'
#' @param object the blblm model to use
#' @param parm the parameters we want to find the confidence intervals of
#' @param level the confidence level for each confidence interval
#' @param ... extra arguments
#'
#' @examples
#' blblm_model <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, num_workers = 4, parallelize = FALSE, fast = FALSE)
#' confint(blblm_model, c("wt", "hp"))
#' @export
#' @method confint blblm
confint.blblm <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(parm)) {
    parm <- attr(terms(object$formula), "term.labels")
  }
  alpha <- 1 - level
  est <- object$estimates
  out <- map_rbind(parm, function(p) {
    map_mean(est, ~ map_dbl(., list("coef", p)) %>% quantile(c(alpha / 2, 1 - alpha / 2)))
  })
  if (is.vector(out)) {
    out <- as.matrix(t(out))
  }
  dimnames(out)[[1]] <- parm
  out
}

#' Predicts new dataset for BLBLM
#'
#' @param object the blblm model to use
#' @param new_data the dataset representing `X` to predict `y` for
#' @param confidence whether or not to create a confident interval, defaults to `FALSE`
#' @param level the confidence level to run the confidence interval on, defaults to `0.95`
#' @param ... extra arguments
#'
#' @examples
#' blblm_model <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, num_workers = 4, parallelize = FALSE, fast = FALSE)
#' predict(blblm_model, data.frame(wt = c(2.5, 3), hp = c(150, 170)))
#' predict(blblm_model, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = TRUE)
#' @export
#' @method predict blblm
predict.blblm <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
  if (confidence) {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>%
      apply(1, mean_lwr_upr, level = level) %>%
      t())
  } else {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>% rowMeans())
  }
}


mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(fit = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>% set_names(c("lwr", "upr")))
}

map_mean <- function(.x, .f, ...) {
  (map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}

map_cbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(cbind)
}

map_rbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(rbind)
}

sigmaHelper <- function(X, y, fit) {
  sse <- 0
  for (j in 1:nrow(X)) {
    y_pred <- 0
    for (i in 1:ncol(X)) {
      y_pred <- y_pred + (X[, i][[j]] * fit$coefficients[[i]])
    }
    sse <- sse + (y_pred - y[[j]])^2
  }
  sse / nrow(X)
}
