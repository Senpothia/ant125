# Trace les courbes de régression pour les coefficients du modèle: RN, LN, RF, ou LF
# Version ggplot2::ggplot. Représentations conjointes de toutes les courbes.
# matrice: liste en matrice des coefficients de regression sur les coefficients extraits des données de mesures
# ex:
# CS<-regMods("data")
# matrice= CS[1]
# intervalle: vecteurs de l'intervalle de mesures. En rapport avec points de mesures: frequencies ou nTypes
# ex:intervalle=c(60,120) pour groupes xN ou intervalle=c(10, 100) pour les groupes xF
# Supprimer les graphes existants si les nouveaux ne sont pas enregistrés dans le repertoire de sauvegarde.
# Sinon fermer Rstudio et relancer

#' Plots the regression curves for the model coefficients: RN, LN, RF, or LF
#' Version ggplot2::ggplot. Joint representations of all curves.
#'
#' @param matrice matrix list of regression coefficients on the coefficients extracted from the measurement data
#' @param intervalle vectors of the measurement interval. Related to measurement points: frequencies or nTypes.
#' e.g:intervalle=c(60,120) or intervalle=c(10, 100)
#'
#' @return
#' @export
#'
#' @examples
#' CS<-regMods("data")
#' matrice= CS[1]
#'
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
  p0<-ggplot2::ggplot(df,ggplot2::aes(x))+ ggplot2::stat_function(fun=y0, col="red") + ggplot2::ggtitle("Coefficient degré 0") + ggplot2::labs(x = labX, y = "d0")
  #print(p0)
  p1<-ggplot2::ggplot(df,ggplot2::aes(x))+ ggplot2::stat_function(fun=y1, col="blue")+ ggplot2::ggtitle("Coefficient degré 1") + ggplot2::labs(x = labX, y = "d1")
  #print(p1)
  p2<-ggplot2::ggplot(df,ggplot2::aes(x))+ ggplot2::stat_function(fun=y2, col="green")+ ggplot2::ggtitle("Coefficient degré 2") + ggplot2::labs(x = labX, y = "d2")
  #print(p2)

  figure<-ggpubr::ggarrange(p0,p1,p2, heights = c(2, 2, 2),
            ncol = 1, nrow = 3, align = "v")

  figure<-ggpubr::annotate_figure(figure,
                  top = ggpubr::text_grob(titre, color = "black", face = "bold", size = 14)
  )

  print(figure)


  png(paste("./plots/", lab[1] ,"_coefs_params.png",sep=""))
  print(figure)
  dev.off()

}
