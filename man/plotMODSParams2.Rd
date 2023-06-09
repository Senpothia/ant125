% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotMODSParams2.R
\name{plotMODSParams2}
\alias{plotMODSParams2}
\title{Plots the regression curves for the model coefficients: RN, LN, RF, or LF
Version ggplot2::ggplot. Joint representations of all curves.}
\usage{
plotMODSParams2(matrice, intervalle)
}
\arguments{
\item{matrice}{matrix list of regression coefficients on the coefficients extracted from the measurement data}

\item{intervalle}{vectors of the measurement interval. Related to measurement points: frequencies or nTypes.
e.g:intervalle=c(60,120) or intervalle=c(10, 100)}
}
\description{
Plots the regression curves for the model coefficients: RN, LN, RF, or LF
Version ggplot2::ggplot. Joint representations of all curves.
}
\examples{
CS<-regMods("data")
matrice= CS[1]

}
