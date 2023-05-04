# Exécute le calcul des valeurs de la grandeur d'interêt suivant l'estimateur
# estimator: estimateur
# param: valeur du paramètre. ex: F=125. Les fréquences sont indiquées en kHz
# interval: étendue de l'interval de calcul. ex: 60:120 ou seq(60:120, by=0.1) pour le nbre de spires si le paramètre est F
# matrice: matrice des coefficients de régression

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