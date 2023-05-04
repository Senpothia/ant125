# Retourne la valeur de l'abcisse de l'estimateur N ou F qui fournit la
# la valeur recherchée de la grandeur d'intérêt: L ou R
# matrice: ex: CS<-regMods("data")
#  m<-optimisEst(CS[4], 2000, 125, c(60, 120))
# param: paramètre des coefficients du modèle. ex: 125kHz
# value: valeur attendue de la grandeur d'intérêt


optimisEst<-function(matrice, value, param, interval){
  
  MATRICE <- matrix(unlist(matrice), ncol = 3, byrow = FALSE)
  
  x<- param
  
  c<-MATRICE[,1]
  b<-MATRICE[,2]
  a<-MATRICE[,3]
  
  C<-c[1] + c[2]* x + c[3]* x^2 
  B<-b[1] + b[2]* x + b[3]* x^2 
  A<-a[1] + a[2]* x + a[3]* x^2  
 
  
  Y<-function(y) { abs(C + B * y + A * y^2 - value) }
  minus<-optimize(Y, interval)
  return(minus)

}