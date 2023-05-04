# Tri des paramètres

# étapes pour l'appel:
# > TAB<-getMeasures("data", ",", ".")
# > MODS<-getModels(TAB)
# > models<-getModparams(MODS, "RF")

# models: liste de modèles: RN, LN, LF ou RF
# fourni pour l'ensemble d'une liste de modèles les coefficients triés par dégré
# dégré: 0, 1, 2 du polynôme de dégré 2

getParams<-function(models){  #mod
  
  PARAMS<-list()
  D0<-list()
  D1<-list()
  D2<-list()
  
  i<-1
  for(m in models){ 
  
      D0[[i]]<-m$coefficients[1]
      D1[[i]]<-m$coefficients[2]
      D2[[i]]<-m$coefficients[3]
      i<-i+1

    
  }
  PARAMS[[1]]<-D0
  PARAMS[[2]]<-D1
  PARAMS[[3]]<-D2
  
  names(PARAMS)<-c("D0", "D1", "D2")
  return(PARAMS)
  
  
}
