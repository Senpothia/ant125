
#' Provides the set of cumulative graphs with insertion of the simulated curve
#'
#' @param params list of parameter values
#' @param file name of the file to be processed given as a string
#'
#' @return plots of quantities of interest
#' @export
#'
#' @examples
#' getAllGraphs("data", c(110, 110, 125, 90))
#' getAllGraphs("data", c(N1, N2, F1, F2))
getAllGraphs<-function(file, params){

  TAB<-getMeasures(file)

  frequencies<-sort(unique(TAB$F))     # Liste des fréquences
  echs<-sort(unique(TAB$ech))
  nTypes<-sort(unique(TAB$N))

  CS<-regMods(file)

  YY<-evalEstimator2(CS[1], params[1], frequencies) #LF; param: N
  plotGroups(TAB, "LF", YY, params[1]) #LF


  YY<-evalEstimator2(CS[2], params[2], frequencies) #RF; param: N
  plotGroups(TAB, "RF", YY, params[2]) #LF


  YY<-evalEstimator2(CS[3],params[3], nTypes) #RN; param: F
  plotGroups(TAB, "RN", YY, params[3]) #RN


  YY<-evalEstimator2(CS[4],params[4], nTypes) #LN; param: F
  plotGroups(TAB, "LN", YY, params[4]) #LN

}
