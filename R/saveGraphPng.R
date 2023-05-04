# Enregistrement d'un graphe

saveGraphPng<-function(fileName, p){
  
  fileName<-paste("./plots/", fileName)
  fileName<-paste(fileName, ".png")
  png(fileName)
  print(p)
  dev.off()
}