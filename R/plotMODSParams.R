# Trace les courbes de régression pour les coefficients du modèle: RN, LN, RF, ou LF
# matrice: liste en matrice des coefficients de regression sur les coefficients extraits des données de mesures
# ex: 
# CS<-regMods("data")
# matrice= CS[1]
# intervalel: vecteurs de l'intervalle de mesures. En rapport avec points de mesures: frequencies ou nTypes
# ex:intervalle=c(60,120) ou intervalle=c(10, 100)

plotMODSParams<-function(matrice, intervalle){
  
  MATRICE <- matrix(unlist(matrice), ncol = 3, byrow = FALSE)
  
  m0<-MATRICE[,1]
  m1<-MATRICE[,2]
  m2<-MATRICE[,3]
  
  x<-seq(intervalle[1], intervalle[2], by=1)
  y0<-m0[1] + m0[2]*x + m0[2]*x^2
  y1<-m1[1] + m1[2]*x + m1[2]*x^2
  y2<-m2[1] + m2[2]*x + m2[2]*x^2
  plot(x,y0, type="l", col="red")
  lines(x,y1, col="blue")
  lines(x,y2, col="green")
  
}