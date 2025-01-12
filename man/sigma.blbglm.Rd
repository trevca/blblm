% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blbglm.R
\name{sigma.blbglm}
\alias{sigma.blbglm}
\title{Gets sigma for blbglm}
\usage{
\method{sigma}{blbglm}(object, confidence = FALSE, level = 0.95, ...)
}
\arguments{
\item{object}{the blbglm model to use}

\item{confidence}{whether or not to include a confidence interval}

\item{level}{the confidence level to be used if confidence is \code{TRUE}, defaults to \code{0.95}}

\item{...}{extra arguments}
}
\description{
overloads the standard \code{sigma} functionality to get the error for a \code{blbglm} model.
}
\examples{
blbglm_model <- blbglm(am ~ wt * hp, data = mtcars, m = 3, B = 100)
sigma(blbglm_model)
sigma(blbglm_model, confidence = TRUE)
sigma(blbglm_model, confidence = TRUE, level = 0.99)
}
