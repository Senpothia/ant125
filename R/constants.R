
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


#' Set source folder for loading data
#'
#' @param name of the source folder as string
#'
#' @return nothing
#' @export
#'
#' @examples
#' setDataSource("folder")
#'
setDataSource<-function(source){

  env$dataSource<-source
}

#' Set the reference frequency for estimations. Usely this parameter is set as 125
#' as 125kHz
#'
#' @param freq The reference frequency in kHz
#'
#' @return nothing
#' @export
#'
#' @examples setRefFrequency(125)
#'
setRefFrequency<-function(freq){

  env$F0<-freq
}

#' Cdv1 setter
#'
#' @param c: capacitance of antenna voltage divider. In farad
#'
#' @return nothing
#' @export
#'
#' @examples
setCdv1<-function(c){

  env$Cdv1<-c
  updateC2()

}

#' Cdv2 setter
#'
#' @param c: capacitance of antenna voltage divider. In farad
#'
#' @return nothing
#' @export
#'
#' @examples
setCdv2<-function(c){

  env$Cdv2<-c
  updateC2()

}

#' Voltage of the antenna driver setter
#'
#' @param v
#'
#' @return nothing
#' @export
#'
#' @examples
#' setVdd(10)
setVdd<-function(v){

  env$Vdd<-v

}

#' Antenna reference voltage setter
#'
#' @param v
#'
#' @return nothing
#' @export
#'
#' @examples
#' setVss(0)
setVss<-function(v){

  env$Vss<-v

}

#' Serial antenna resistance setter
#'
#' @param r
#'
#' @return nothing
#' @export
#'
#' @examples
#' setRser(18)
#'
setRser<-function(r){

  env$Rser<-r

}

#' Serial antenna resistance
#'
#' @param r
#'
#' @return nothing
#' @export
#'
#' @examples
#' setRad(9)
#'
setRad<-function(r){

  env$Rad<-r

}

#' Setter for distance reference for magnetic field estimation
#'
#' @param z
#'
#' @return nothig
#' @export
#'
#' @examples
setZ<-function(z){

  env$z<-z

}

#' Radius of antenna setter. Use for magnetic field estimation
#'
#' @param r
#'
#' @return nothing
#' @export
#'
#' @examples
setR<-function(r){

  env$r<-r

}

#' Equivalent capacitance value of the antenna divider voltage
#'
#' @return a numeric
#' @export
#'
#' @examples
updateC2<-function(){

  env$c2=env$Cdv1*env$Cdv2/(env$Cdv1+env$Cdv2)
}

#' Equivalent capacitance value of the antenna divider voltage getter
#'
#' @return a numeric
#' @export
#'
#' @examples
getC2<-function(){

  env$c2=env$Cdv1*env$Cdv2/(env$Cdv1+env$Cdv2)
  return(env$c2)
}

#' Summary of the constants use for estimation in the package
#'
#' @return
#' @export
#'
#' @examples
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
