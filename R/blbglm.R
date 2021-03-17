#' BLBGLM
#' @description  blbglm is a model that performs "bag of little bootstraps" method of regression on a logistic regression model. This is intended for when the dependent variable of a particular model is discrete for classification purposes.
#'
#' @param formula, the formula form of the desired Logistic regression.
#' @param data, the data to pull the formula variables from.
#' @param m, the number of subsets of "little bootstraps" to run, defaults to `10`.
#' @param B, the number of times to bootstrap the data, defaults to `5000`.
#' @param seed_value, an initial seed value, defaults to `100`, for reproduceability.
#' @examples
#' blbglm(am ~ wt * hp, data = mtcars, m = 3, B = 100)
#' @export
blbglm <- function(formula, data, m = 3, B = 100, seed_value = 100) {
  data_list <- split_data(data, m)
  set.seed(seed_value)
  estimates <- map(
    data_list,
    ~ glm_each_subsample(formula = formula, data = ., n = nrow(data), B = B)
  )
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blbglm"

  invisible(res)
}

# compute the estimates
glm_each_subsample <- function(formula, data, n, B) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wrong variable from the global scope.
  environment(formula) <- environment()
  m <- model.frame(formula, data)
  X <- model.matrix(formula, m)
  y <- model.response(m)
  replicate(B, glm1(formula, data, n), simplify = FALSE)
}

# compute the regression estimates for a blb dataset
glm1 <- function(formula, data, n) {
  fit <- glm(formula, data, weights = as.vector(rmultinom(1, n, rep(1, nrow(data)))), family = binomial)
  list(coef = blbcoef(fit), sigma = blbsigma(fit))
}

#' Prints blbglm
#'
#' @description overloads the `print` functionality to show the formula used when printing a `blbglm` model.
#'
#' @param x the model to be printed
#' @param ... extra arguments
#' @examples
#' blbglm_model <- blbglm(am ~ wt * hp, data = mtcars, m = 3, B = 100)
#' print(blbglm_model)
#' @export
#' @method print blbglm
print.blbglm <- function(x, ...) {
  cat("blbglm model:", capture.output(x$formula))
  cat("\n")
}


#' Gets sigma for blbglm
#' @description overloads the standard `sigma` functionality to get the error for a `blbglm` model.
#' @param object the blbglm model to use
#' @param confidence whether or not to include a confidence interval
#' @param level the confidence level to be used if confidence is `TRUE`, defaults to `0.95`
#' @param ... extra arguments
#' @examples
#' blbglm_model <- blbglm(am ~ wt * hp, data = mtcars, m = 3, B = 100)
#' sigma(blbglm_model)
#' sigma(blbglm_model, confidence = TRUE)
#' sigma(blbglm_model, confidence = TRUE, level = 0.99)
#' @export
#' @method sigma blbglm
sigma.blbglm <- function(object, confidence = FALSE, level = 0.95, ...) {
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

#' Gets coefficients for blbglm
#' @description overloads the standard `coef` functionality to get the coefficients for a `blbglm` model.
#' @param object the blbglm model to use
#' @param ... extra arguments
#'
#' @examples
#' blbglm_model <- blbglm(am ~ wt * hp, data = mtcars, m = 3, B = 100)
#' coef(blbglm_model)
#' @export
#' @method coef blbglm
coef.blbglm <- function(object, ...) {
  est <- object$estimates
  return(map_mean(est, ~ map_cbind(., "coef") %>% rowMeans()))
}


#' Gets confident interval for blbglm variables
#' @description overloads the standard `confint` functionality to calculate the confidence intervals for a `blbglm` model's coefficients.
#' @param object the blbglm model to use
#' @param parm the parameters we want to find the confidence intervals of
#' @param level the confidence level for each confidence interval
#' @param ... extra arguments
#'
#' @examples
#' blbglm_model <- blbglm(am ~ wt * hp, data = mtcars, m = 3, B = 100)
#' confint(blbglm_model, c("wt", "hp"))
#' @export
#' @method confint blbglm
confint.blbglm <- function(object, parm = NULL, level = 0.95, ...) {
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

#' Predicts new dataset for blbglm
#' @description overloads the standard `predict` functionality to get predictions for a `blbglm` model given an X value.
#' @param object the blbglm model to use
#' @param new_data the dataset representing `X` to predict `y` for
#' @param confidence whether or not to create a confident interval, defaults to `FALSE`
#' @param level the confidence level to run the confidence interval on, defaults to `0.95`
#' @param ... extra arguments
#'
#' @examples
#' blbglm_model <- blbglm(am ~ wt * hp, data = mtcars, m = 3, B = 100)
#' predict(blbglm_model, data.frame(wt = c(2.5, 3), hp = c(150, 170)))
#' predict(blbglm_model, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = TRUE)
#' @export
#' @method predict blbglm
predict.blbglm <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {
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
