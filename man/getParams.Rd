% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getParams.R
\name{getParams}
\alias{getParams}
\title{Sorting of parameters}
\usage{
getParams(models)
}
\arguments{
\item{models}{list of models: RN, LN, LF or RF provides for the whole list of models
the coefficients sorted by degree}
}
\description{
Sorting of parameters
}
\examples{
TAB<-getMeasures("ant046")
MODS<-getModels(TAB)
models<-getModparams(MODS, "RF")

}
