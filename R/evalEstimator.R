

#' Provides the quantity of interest values based on an estimator
#' @param estimator estimator given as a string
#' @param param parameter value. e.g: F=125. frequencies are given in kHz
#' @param interval range of the calculation interval. e.g: 60:120 ou seq(60:120, by=0.1) for the number of turns, or 10:100 if the parameter is the frequency
#' @param titre title of the graph produced - optional
#' @param labX label axis x - optional
#' @param labY label axis y - optional

#'
#' @return a numeric
#' @export
#'
#' @examples
evalEstimator<-function(estimator, param, interval, titre="", labX="", labY=""){

  x<-param
  y<-interval
  fonction<-parse(text=estimator)
  Y<-eval(fonction)
  p<-ggplot2:ggplot(data.frame(y,Y), aes(x=y, y=Y)) + geom_line(col="blue") + ggtitle(titre) + labs(x = labX, y = labY)
  print(p)
  return (Y)

}
