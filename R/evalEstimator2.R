
#' Executes the calculation of the values of the quantity of interest according to the estimator
#'
#' @param matrice  matrix of regression coefficients
#' @param param value of the parameter. ex: F=125. The frequencies are indicated in kHz
#' @param interval range of the calculation interval. ex: 60:120 or seq(60:120, by=0.1) for the number of turns if the parameter is F
#'
#' @return a list of values according the estimator on the whole range of interval calculation
#' @export
#'
#' @examples evalEstimator2(CS[3], 125, c(60, 120))
#'
evalEstimator2<-function(matrice, param, interval){

  MATRICE <- matrix(unlist(matrice), ncol = 3, byrow = FALSE)

  x<- param

  c<-MATRICE[,1]
  b<-MATRICE[,2]
  a<-MATRICE[,3]

  C<-c[1] + c[2]* x + c[3]* x^2
  B<-b[1] + b[2]* x + b[3]* x^2
  A<-a[1] + a[2]* x + a[3]* x^2


  Y<-function(y) { C + B * y + A * y^2}

  return(Y(interval))

}
