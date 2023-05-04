#---------------------------------------------------------------------------------------------------------
# Constantes

env<-new.env(parent = emptyenv())

env$F0<-100000   # fréquence de référence
env$dataSource="./data/mesures.csv"
env$vdd=5
env$Vss=0
env$Rser=27
env$Rad=9
env$z=1e-2 # distance de référence 1cm
env$r=0.38 # rayon de l'antenne
env$Cdv1=39e-12
env$Cdv2=1.5e-09
env$c2<-(39e-12*1.5e-9)/(39e-12+1.5e-9)  # Influence pont capacitif

# ---------  GETTERS et SETTERS

# Change reference frequency

setDataSource<-function(source){
  
  env$dataSource<-source
}

setRefFrequency<-function(freq){
  
  env$F0<-freq
}

setCdv1<-function(c){
  
  env$Cdv1<-c
  updateC2()
  
}

setCdv2<-function(c){
  
  env$Cdv2<-c
  updateC2()
  
}

setVdd<-function(v){
  
  env$Vdd<-v
  
}

setVss<-function(v){
  
  env$Vss<-v
  
}

setRser<-function(r){
  
  env$Rser<-r
  
}

setRad<-function(r){
  
  env$Rad<-r
  
}

setZ<-function(z){
  
  env$z<-z
  
}

setR<-function(r){
  
  env$r<-r
  
}

updateC2<-function(){
  
  env$c2=env$Cdv1*env$Cdv2/(env$Cdv1+env$Cdv2)    
}

getC2<-function(){
  
  env$c2=env$Cdv1*env$Cdv2/(env$Cdv1+env$Cdv2) 
  return(env$c2)
}

summaryEnv<-function(){
  
  print("--------------- CONSTANTES DE REFERENCES --------------------------------------------")
  print(paste("Fréquence de référence:", env$F0))  # fréquence de référence
  print(paste("Localisation des  données:", env$dataSource))
  print(paste("Tension d'alimentation Vdd:", env$vdd))
  print(paste("Référence de tension Vss:", env$Vss))
  print(paste("Résistance série Rser:",env$Rser))
  print(paste("Résistance de sortie driver antenne Rad:", env$Rad))
  print(paste("Distance de référence:", env$z)) # distance de référence 1cm
  print(paste("Rayon d'antenne:", env$r)) # rayon de l'antenne
  print(paste("Pont capacitif, Cdv1:",env$Cdv1))
  print(paste("Pont capacitif, Cdv2:",env$Cdv2))
  print(paste("Pont capacitif, capacité équivalente:",getC2())) # Influence pont capacitif
  print("-------------------------------------------------------------------------------------")
}
