# Exécute le calcul des valeurs de la grandeur d'interêt suivant l'estimateur
# estimator: estimateur
# param: valeur du paramètre. ex: F=125. Les fréquences sont indiquées en kHz
# interval: étendue de l'interval de calcul. ex: 60:120 ou seq(60:120, by=0.1) pour le nbre de spires si le paramètre est F
# titre: titre du graphe produit - optionnel
# labX: label axe x - optionnel
# labY: label axe y - optionnel

evalEstimator<-function(estimator, param, interval, titre="", labX="", labY=""){
  
  x<-param
  y<-interval
  fonction<-parse(text=estimator)
  Y<-eval(fonction)
  p<-ggplot(data.frame(y,Y), aes(x=y, y=Y)) + geom_line(col="blue") + ggtitle(titre) + labs(x = labX, y = labY) 
  print(p)
  return (Y)
  
}
