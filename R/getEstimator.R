# Génère l'estimateur d'après les coefficient de regressions des paramètres du modèle
# matrice: liste en matrice des coefficients de regression sur les coefficients extraits des données de mesures
# ex: 
# CS<-regMods("data")
# matrice= CS[1]
# Retourne l'équation Y(x) représentant la grandeur d'intérêt L ou N fonction de x (N ou F)
# Y(y) <-C(x) + B(x) * y + A(x) * y^2
# A, B, C facteur sous forme de polymônes de dégrés 2, résultat des régression sur les paramètres
# des modèles
# y: variable d'intérêt: N ou F
# x: paramètre: N pour le groupe xF; F pour le groupe xN
# les coefficients de Y dépendent de x. Chaque coefficient est un polynôme de degré 2

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
