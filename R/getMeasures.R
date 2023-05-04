#Chargement des données
#TAB<-read.table("./data/mesures.csv",header=TRUE,sep=";",dec=",")
#TAB<-read.table("./data/data.csv",header=TRUE,sep=",",dec=".")

# Load data file for measurements and factors corrections

getMeasures<-function(file, s=";", d=","){
  
  file<-paste("./data/",file, sep="")
  file<-paste(file,".csv", sep="")
  TAB<-read.table(file,header=TRUE,sep=s,dec=d)
  return(TAB)
  
}