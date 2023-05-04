# Obtention listes des paramètres de modèles pour un groupe: RN, LN, LF, RF 
# contenu dans un ensemble désigné par models

getModparams<-function(models, groupe){  # ex appel:
                                      #           M<-getModels(TAB)
                                      #           getModparams(M, "LN")
  
  PAR<-list()
  
  # groupes: LN, RN, LF, RF
  # models: liste de tous les models d'après le jeu de données traité
  
  if(groupe == "LN"){
    
    gr<-"MODSLN"
  }
  
  if(groupe == "RN"){
    
    gr<-"MODSRN"
  }
  
  if(groupe == "LF"){
    
    gr<-"MODSLF"
  }
  
  
  if(groupe == "RF"){
    
    gr<-"MODSRF"
    
  }
  
  #MODS[["MODSLF"]][1]
  # |       |       |
  # liste   liste   |
  #         interne |
  #                 indice dans la liste interne        
  
  i<-1
  for(m in models[[gr]]){
    mods<-list()
    for(j in 1:3){
      
      mods[j]<-m$coefficients[j]
      
      
    }
  
    PAR[[i]]<-mods
    i<-i+1
   
  }
  
 
  return(PAR)
  
  
}
