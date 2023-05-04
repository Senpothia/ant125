# Conversion de la liste de paramètres d'un groupe en une matrice de paramètres listés en colonnes
# coef: liste issue de getParams()pour un des groupes: RN, LN, RF, LF

getMatParams<-function(coef){
  
  
  MAT <- matrix(unlist(coef), ncol = 3, byrow = TRUE)
  
  if(dim(MAT)[1] == 4){
    
    dimnames(MAT) <- list(c("N=60", "N=80", "N=100", "N=120"), c("D0", "D1", "D2"))
  }
  
  if(dim(MAT)[1] == 7){
    
    dimnames(MAT) <- list(c("F=10", "F=20", "F=28.5", "F=40", "F=50", "F=66.6", "F=100"), c("D0", "D1", "D2"))
  }
  
  return(MAT)
  
}
