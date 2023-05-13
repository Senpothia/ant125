
#' Executes the calculation of the values of the quantity of interest according to the estimator coefficients
#
#' @param coefs estimated coefficients of the model
#'
#' @param parameter the value to provide the quantity of interest value
#'
#' @return a numeric
#' @export
#'
#' @examples
#' evalEstimator2(COEFSL, 524)
#'
evalEstimator2<-function(coefs, parameter){

  Y<-function(y) { coefs[1] + coefs[2] * y + coefs[3] * y^2}

  return(Y(parameter))

}
