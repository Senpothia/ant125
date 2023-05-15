
#' Builds an estimator based on a vector of regression coefficients
#'
#' @param coefs vector of coefficients form a linear regression
#' @param variable name of the variable given in string
#'
#' @return a string representing an estimator based on the coefficients of regression
#' @export
#'
#' @examples
#' COEFSR<-extModels(TAB, 125, FALSE, FALSE)
#' est<-buildEstimator2(COEFSR, "N")
#'
buildEstimator2<-function(coefs, variable){

  Y<-paste(coefs[1], "+",  coefs[2] ,"*", variable , "+", coefs[3] , "*",  variable, "^2")

  return(Y)

}
