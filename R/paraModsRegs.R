
#' Deduce models on the parameters from a matrix of parameters
#'
#' @param matrice matrix from a group RN, RL, LN, or LF via getMatParams()
#' @param abscisse vector of the values of frequencies F or number of turns N which were used for the measurements
#' abscisse is the parameter of the studied curve network: F for RN, LN; N for RF, LF
#'
#' @return
#' @export
#'
#' @examples
paraModsRegs<-function(matrice, abscisse){

  l<-list()

  for(i in 1:3){

    v<-vector()
    coefs<-matrice[,i]
    m <- lm(coefs~abscisse+I(abscisse^2), data=data.frame(coefs,abscisse))
    v[1]<-m$coefficients[1]
    v[2]<-m$coefficients[2]
    v[3]<-m$coefficients[3]
    l[[i]]<-v
  }
  MAT <- matrix(unlist(l), ncol = 3, byrow = TRUE)
  dimnames(MAT) <- list(c("Dgr0", "Dgr1", "Dgr2"), c("D0", "D1", "D2"))
  return(MAT)


}
