# graphe des représentations conjointes paramètrées en F ou N
# data: jeu de données issu de getMeasures()
# groupe: "RN", "LN", "RF", "LF"
# estimation: données issues de l'estimateur (à 125kHz par ex.), vecteur de longueur nTypes ou frequencies
# estimation: données issue de evalEstimator2
# param: valeur du paramètre dans les modèles. Ex: 125kHz


plotGroups<-function(data, groupe, estimation, param){
  
  #TODO: ajouter traitement de l'extension du data.frame aux valeurs de evalEstimator2
  
  echs<-sort(unique(data$ech))
  frequencies<-sort(unique(data$F))     # Liste des fréquences
  nTypes<-sort(unique(data$N))
  
  newEch<-max(echs) + 1
  new_row<-c()
 
  
  if(groupe == "RN"){
    
  
    i<-1
    
    for(n in nTypes){
      
      new_row = c(echs=newEch, N=n, F=param, R=estimation[i], L=0)
      data<-rbind(data, new_row)
      i<-i+1
    }
  
    
    p1 <- ggplot(data, aes(x=N, y=R, colour=as.factor(F), group=F)) + geom_point() + geom_smooth(method=lm, formula = y ~ poly(x, 2), se=FALSE) + ggtitle(paste("Résistance vs N - Paramètre simulé: F=", as.character(param) )) + labs(x = "N - Tours", y = "Résistance - Ohms", color="F")
    
   
  }
  
  if(groupe == "LN"){
    
   
    i<-1
    
    for(n in nTypes){
      
      new_row = c(echs=newEch, N=n, F=param, R=0, L=estimation[i])
      data<-rbind(data, new_row)
      i<-i+1
    }
  
    
    p1 <- ggplot(data, aes(x=N, y=L, colour=as.factor(F), group=F)) + geom_point() + geom_smooth(method=lm, formula = y ~ poly(x, 2), se=FALSE) + ggtitle(paste("Inductance vs N - pamamètre simulé: F=", as.character(param))) + labs(x = "N - Tours", y = "Inductance - mH", color="F")
 

  }
  
  if(groupe == "RF"){
    
    i<-1
    
    for(f in frequencies){
      
      new_row = c(echs=newEch, N=param, F=f, R=estimation[i], L=0)
      data<-rbind(data, new_row)
      i<-i+1
    }
    
    p1 <- ggplot(data, aes(x=F, y=R, colour=as.factor(N), group=N)) + geom_point() + geom_smooth(method=lm, formula = y ~ poly(x, 2), se=FALSE) + ggtitle(paste("Résistance vs Fréquence - pamamètre simulé: N=", as.character(param))) + labs(x = "F - kHz", y = "Résistance - Ohms", color="N")
  
   
    
  }
  
  if(groupe == "LF"){
    
    i<-1
    
    for(f in frequencies){
      
      new_row = c(echs=newEch, N=param, F=f, R=0, L=estimation[i])
      data<-rbind(data, new_row)
      i<-i+1
    }
  
    p1 <- ggplot(data, aes(x=F, y=L, colour=as.factor(N), group=N)) + geom_point() + geom_smooth(method=lm, formula = y ~ poly(x, 2), se=FALSE) + ggtitle(paste("Inductance vs Fréquence - paramètre simulé: N=", as.character(param))) + labs(x = "N - Tours", y = "Inductance - mH", color="N")
   

  }
  
  print(p1)
 
 
}