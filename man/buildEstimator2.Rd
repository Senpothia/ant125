% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/buildEstimator2.R
\name{buildEstimator2}
\alias{buildEstimator2}
\title{Builds an estimator based on a vector of regression coefficients}
\usage{
buildEstimator2(coefs, variable)
}
\arguments{
\item{coefs}{vector of coefficients form a linear regression}

\item{variable}{name of the variable given in string}
}
\value{
a string representing an estimator based on the coefficients of regression
}
\description{
Builds an estimator based on a vector of regression coefficients
}
\examples{
COEFSR<-extModels(TAB, 125, FALSE, FALSE)
est<-buildEstimator2(COEFSR, "N")

}
