

#' Generates the estimator from the regression coefficients of the model parameters
#'
#' @param matrice matrix list of regression coefficients on the coefficients extracted from the measurement data
#'
#' @return
#'  Returns the equation Y(x) representing the quantity of interest L or R as a function of x (N or F)
#' Y(y) <-C(x) + B(x) * y + A(x) * y^2
#' A, B, C factor in the form of polymorphs of degree 2, result of the regression on the parameters
#' of the models
#' y: variable of interest: N or F
#' x: parameter: N for the xF group; F for the xN group
#' the coefficients of Y depend on x. Each coefficient is a polynomial of degree 2
#'
#' @export
#'
#' @examples
#' CS<-regMods("data")
#' matrice= CS[1]
#'
getEstimator<-function(matrice){

  MATRICE <- matrix(unlist(matrice), ncol = 3, byrow = FALSE)

  c<-MATRICE[,1]
  b<-MATRICE[,2]
  a<-MATRICE[,3]

  C<-paste(as.character(c[1]), "+", as.character(c[2]), "* x +", as.character(c[3]), "* x^2")
  B<-paste(as.character(b[1]), "+", as.character(b[2]), "* x +", as.character(b[3]), "* x^2")
  A<-paste(as.character(a[1]), "+", as.character(a[2]), "* x +", as.character(a[3]), "* x^2")

  Y<-paste(C, "+", "(", B, ")","* y +","(",  A, ")" , "* y^2")
  print(Y)

}
