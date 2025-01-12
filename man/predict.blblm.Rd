% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blblm.R
\name{predict.blblm}
\alias{predict.blblm}
\title{Predicts new dataset for BLBLM}
\usage{
\method{predict}{blblm}(object, new_data, confidence = FALSE, level = 0.95, ...)
}
\arguments{
\item{object}{the blblm model to use}

\item{new_data}{the dataset representing \code{X} to predict \code{y} for}

\item{confidence}{whether or not to create a confident interval, defaults to \code{FALSE}}

\item{level}{the confidence level to run the confidence interval on, defaults to \code{0.95}}

\item{...}{extra arguments}
}
\description{
overloads the standard \code{predict} functionality to get predictions for a \code{blglm} model given an X value.
}
\examples{
blblm_model <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, num_workers = 4, parallelize = FALSE, fast = FALSE)
predict(blblm_model, data.frame(wt = c(2.5, 3), hp = c(150, 170)))
predict(blblm_model, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = TRUE)
}
