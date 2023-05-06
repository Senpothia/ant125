
#' Plots the regression curves for the model coefficients: RN, LN, RF, or LF
#'
#' @param matrice matrix list of regression coefficients on the coefficients extracted from the measurement data
#' @param intervalle vectors of the measurement interval. Related to measurement points: frequencies or nTypes.
#' e.g:intervalle=c(60,120) or intervalle=c(10, 100)
#' e.g:matrice= CS[1]
#' matrice= CS[1]
#'
#' @return
#' @export
#'
#' @examples
#' CS<-regMods("data")
plotMODSParams<-function(matrice, intervalle){

  MATRICE <- matrix(unlist(matrice), ncol = 3, byrow = FALSE)

  m0<-MATRICE[,1]
  m1<-MATRICE[,2]
  m2<-MATRICE[,3]

  x<-seq(intervalle[1], intervalle[2], by=1)
  y0<-m0[1] + m0[2]*x + m0[2]*x^2
  y1<-m1[1] + m1[2]*x + m1[2]*x^2
  y2<-m2[1] + m2[2]*x + m2[2]*x^2
  plot(x,y0, type="l", col="red")
  lines(x,y1, col="blue")
  lines(x,y2, col="green")

}
