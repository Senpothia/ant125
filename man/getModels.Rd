% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getModels.R
\name{getModels}
\alias{getModels}
\title{Provides a list of all RN, LN, RF, LF model lists}
\usage{
getModels(TAB)
}
\arguments{
\item{TAB}{Data frame representing the measurements provided by getMeasures function}
}
\value{
A list of lists of models: RN, LN, LF, RF
}
\description{
Provides a list of all RN, LN, RF, LF model lists
}
\examples{
TAB<-getMeasures("data")
MODS<-getModels(TAB)

}
