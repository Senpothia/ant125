
#' Provides two sets of cumulative graphs with insertion of the simulated curve
#'
#' @param data data frame representing the measurements to analyse provided by the function getMeasures based on the data file with csv extention
#' @param param the frequency value in kHz for extrapolations.
#'
#' @return plots of quantities of interest
#' @export
#'
#' @examples
#' TAB<-getMeasures("ant046")
#' getAllGraphs(TAB, 125)

getAllGraphs<-function(data, param){

  COEFSL<-extModels(data, param, TRUE, FALSE)
  COEFSR<-extModels(data, param, FALSE, FALSE)

  newEch<-max(env$echs) + 1
  new_row<-c()

  estimationsL<-evalEstimator2(COEFSL, env$turns)
  estimationsR<-evalEstimator2(COEFSR, env$turns)

  i<-1

  for(n in env$turns){

    new_row = c(echs=newEch, N=n, F=param, L=estimationsL[i], R=estimationsR[i])
    data<-rbind(data, new_row)
    i<-i+1
  }

  print(data)


  p1 <- ggplot2::ggplot(data, ggplot2::aes(x=N, y=L, colour=as.factor(F), group=F)) + ggplot2::geom_point() + ggplot2::geom_smooth(method=lm, formula = y ~ poly(x, 2), se=FALSE) + ggplot2::ggtitle(paste("Inductance vs N - pamamètre simulé: F=", as.character(param))) + ggplot2::labs(x = "N - Tours", y = "Inductance - mH", color="F")
  p2 <- ggplot2::ggplot(data, ggplot2::aes(x=N, y=R, colour=as.factor(F), group=F)) + ggplot2::geom_point() + ggplot2::geom_smooth(method=lm, formula = y ~ poly(x, 2), se=FALSE) + ggplot2::ggtitle(paste("Résistance vs N - Paramètre simulé: F=", as.character(param) )) + ggplot2::labs(x = "N - Tours", y = "Résistance - Ohms", color="F")

  print(p1)
  print(p2)


}
