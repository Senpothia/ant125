
#' Builds an estimator based on the matrix of regression coefficients depending on a parameter value
#'
#' @param matrice  matrix of regression coefficients
#' @param param value of the parameter. ex: F=125. The frequencies are indicated in kHz
#'
#'
#' @return a string representing the estimator for the parameter value given as parameter
#' @export
#'
#' @examples buildEstimator(CS[3], 125)
#'
buildEstimator<-function(matrice, param){

  MATRICE <- matrix(unlist(matrice), ncol = 3, byrow = FALSE)

  x<- param

  c<-MATRICE[,1]
  b<-MATRICE[,2]
  a<-MATRICE[,3]

  C<-c[1] + c[2]* x + c[3]* x^2
  B<-b[1] + b[2]* x + b[3]* x^2
  A<-a[1] + a[2]* x + a[3]* x^2

  Y<-paste(C, "+",  B ,"* y +", A , "* y^2")

  return(Y)

}
