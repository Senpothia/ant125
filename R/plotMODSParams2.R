# Trace les courbes de régression pour les coefficients du modèle: RN, LN, RF, ou LF
# Version ggplot. Représentations conjointes de toutes les courbes.
# matrice: liste en matrice des coefficients de regression sur les coefficients extraits des données de mesures
# ex: 
# CS<-regMods("data")
# matrice= CS[1]
# intervalel: vecteurs de l'intervalle de mesures. En rapport avec points de mesures: frequencies ou nTypes
# ex:intervalle=c(60,120) pour groupes xN ou intervalle=c(10, 100) pour les groupes xF
# Supprimer les graphes existants si les nouveaux ne sont pas enregsitrés dans le repertoire de sauvegarde.
# Sinon fermer Rstudio et relancer

plotMODSParams2<-function(matrice, intervalle){
  
  lab<-names(matrice)
  
  if(lab[1] == "LF"){labX<-"N tours"
                     titre<-"Inductance vs Fréquence"}
  if(lab[1] == "RF"){labX<-"N tours"
                     titre<-"Résistance vs Fréquence"}
  if(lab[1] == "LN"){labX<-"Fréquence"
                     titre<-"Inductance vs N tours"}
  if(lab[1] == "RN"){labX<-"Fréquence"
                     titre<-"Résistance vs N tours"}
  
  # saveFile<-paste("./plots/", lab[1],"_coefs_params.png",sep="")
  # print(saveFile)
  MATRICE <- matrix(unlist(matrice), ncol = 3, byrow = FALSE)
  
  m0<-MATRICE[,1]
  m1<-MATRICE[,2]
  m2<-MATRICE[,3]
  
  x<-intervalle[1]:intervalle[2]
  y0<-function(x){m0[1] + m0[2]*x + m0[2]*x^2}
  y1<-function(x){m1[1] + m1[2]*x + m1[2]*x^2}
  y2<-function(x){m2[1] + m2[2]*x + m2[2]*x^2}
  df<-data.frame(x, y0(x), y1(x), y2(x))
  p0<-ggplot(df,aes(x))+ stat_function(fun=y0, col="red") + ggtitle("Coefficient degré 0") + labs(x = labX, y = "d0") 
  #print(p0)
  p1<-ggplot(df,aes(x))+ stat_function(fun=y1, col="blue")+ ggtitle("Coefficient degré 1") + labs(x = labX, y = "d1") 
  #print(p1)
  p2<-ggplot(df,aes(x))+ stat_function(fun=y2, col="green")+ ggtitle("Coefficient degré 2") + labs(x = labX, y = "d2") 
  #print(p2)
 
  figure<-ggarrange(p0,p1,p2, heights = c(2, 2, 2),
            ncol = 1, nrow = 3, align = "v")
  
  figure<-annotate_figure(figure,
                  top = text_grob(titre, color = "black", face = "bold", size = 14)
  )
  
  print(figure)
 

  png(paste("./plots/", lab[1] ,"_coefs_params.png",sep=""))
  print(figure)
  dev.off()
  
}