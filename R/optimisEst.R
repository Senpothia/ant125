
#' Returns the value of the abcissa of the N or F estimator which provides the value of the quantity
#' of interest: L or R
#'
#'
#' @param value expected value of the quantity of interest
#' @param interval
#' @param coefs estimated parameters of the model from extModels function
#'
#' @return
#' @export
#'
#' @examples
#'
#' TAB<-getMeasures("ant046")
#' COEFS<-extModels(TAB, 125, FALSE, TRUE)
#' m<-optimisEst(COEFS, 25, c(60, 120)
#'
optimisEst<-function(coefs, value, interval){

  Y<-function(y) { abs(coefs[1] + coefs[2] * y + coefs[3] * y^2 - value) }
  minus<-optimize(Y, interval)
  return(minus)

}
