% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blblm.R
\name{print.blblm}
\alias{print.blblm}
\title{Prints BLBLM}
\usage{
\method{print}{blblm}(x, ...)
}
\arguments{
\item{x}{the model to be printed}

\item{...}{extra arguments}
}
\description{
overloads the \code{print} functionality to show the formula used when printing a \code{blblm} model.
}
\examples{
blblm_model <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, num_workers = 4, parallelize = FALSE, fast = FALSE)
print(blblm_model)
}
