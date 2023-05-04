# Fournit la liste de toutes les listes de modèles RN, LN, RF, LF
# liste de liste
# RN, LN... sont des listes
# Accès à une liste: MODS["MODSLF"] avec MODS la liste des modèles issue de getModels()

getModels<-function(TAB){
  
  MODSLN<-list()
  MODSRN<-list()
  
  MODSLF<-list()
  MODSRF<-list()
  
  nameList<-c("MODSLN", "MODSRN", "MODSLF", "MODSRF")
  
  frequencies<-sort(unique(TAB$F))     # Liste des fréquences 
  echs<-sort(unique(TAB$ech))
  nTypes<-sort(unique(TAB$N))
  
  
  # pour F=f0 
  # 1- L=f(N)
  # 2- R=f(N)
  
  
  #pour N=no 
  # 1- L=g(F)
  # 2- R=g(F)
  
  
  #---------------------    Courbes paramétrées par F      ------------------------------
  
  # LN: Inductance vs N paramétrée en F
  j<-1
  
  for(i in frequencies){
    
    
    VAL<-TAB[TAB$F == i,]
    modL <- lm(VAL$L~VAL$N+I(VAL$N^2), data=VAL)
    MODSLN[[j]]<-modL
    j<-j+1
    
  }
  names(MODSLN)<-c("F=10", "F=20", "F=28.5", "F=40", "F=50", "F=66.6", "F=100")
  
  # RN: Résistance vs N paramétrée en F
  
  j<-1
  
  for(i in frequencies){
    
    
    VAL<-TAB[TAB$F == i,]
    modR <- lm(VAL$R~VAL$N+I(VAL$N^2), data=VAL)
    MODSRN[[j]]<-modR
    j<-j+1
    
  }
  
  names(MODSRN)<-c("F=10", "F=20", "F=28.5", "F=40", "F=50", "F=66.6", "F=100")
  # ---------------    Courbes paramétrées par N   -------------------------------
  # LF: Inductance vs F paramétrée en N
  
  j<-1
  
  for(i in nTypes){
    
    
    VAL<-TAB[TAB$N == i,]
    modL <- lm(VAL$L~VAL$F+I(VAL$F^2), data=VAL)
    MODSLF[[j]]<-modL
    j<-j+1
    
  }
  
  names(MODSLF)<-c("N=60", "N=80", "N=100", "N=120")
  
  # RF: Résistance vs F paramétrée en N
  
  j<-1
  
  for(i in nTypes){
    
    
    VAL<-TAB[TAB$N == i,]
    modR <- lm(VAL$R~VAL$F+I(VAL$F^2), data=VAL)
    MODSRF[[j]]<-modR
    j<-j+1
    
  }
  names(MODSRF)<-c("N=60", "N=80", "N=100", "N=120")
  
  MODS<-list(MODSLF, MODSRF, MODSLN, MODSRN)
  names(MODS)<-c("MODSLF", "MODSRF", "MODSLN", "MODSRN")
  return(MODS)
  
}