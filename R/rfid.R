# Calcul du courant d'antenne
# x: résistance d'antenne


Iant<-function(x){  # En milliampères
  
  I<-(4/pi)*((env$vdd-env$Vss)/(x+env$Rser+2*env$Rad))*1000
  return(I)
  
}


RforI<-function(I){
  rx<-function(x){abs(Iant(x) - I)}
  r<-optimize(rx, c(0, 10000))
  return (r)
}

# Calcul du courant d'antenne
# R: résistance d'antenne

IantA<-function(R){  # En Ampères
  
  I<-(4/pi)*((env$vdd-env$Vss)/(R+env$Rser+2*env$Rad))
  return(I)
  
}

# Evalue la courant d'antenne d'après l'estimateur de la résistance d'antenne
# N: nombre de spires
# Restimator: estimateur de résistance d'antenne. Expression sous forme de string
# obtenu à partir des coefficients de régression

In<-function(N, Restimator){
  
  RN<-parse(text=Restimator)
  R<-eval(RN)
  I<-Iant(R)
  return(I)
  
}

# Etimation du champ magnétique
# Evalue le champ magnétique en fonction du courant d'antenne
# I: courant estimé d'après In

Best<-function(I){
  
  b<-I*env$r^2/env$z^3
  return(b)
  
  
}

# Estimation de la capacité de résonnance

Cres<-function(N, L){
  
  L<-eval(L)
  c<-1/((2*pi*en$F0)^2*L(N))^-1
  
}

Cresonnance<-function(L, F){  # Fréquence de résonnance en kHz pour L en Henry 
  
  return(1/((2*pi*F*1000)^2*L*1e-03) - env$c2)
}

# Estimation tension d'antenne 

Vant<-function(N, R, L){
  R<-eval(R)
  L<-eval(L)
  v<-Iant(R(N))/(2*pi*env$Fo*Cres) 
  
}


# Estimation fréquence d'accord en foction des paramètres du circuit

Fres<-function(L,C){
  
  f<-1/(2*pi*sqrt(L*1e-03*C*1e-12))
  return(f)
  
}

FresN<-function(N,C){
  
  f<-1/(2*pi*sqrt(LEst(N)*1e-03*C*1e-12))
  return(f)
  
}

Fmin<-function(N, C, F){
  
  return(abs(FresN(N, C)- (F*1000)))
  
}

# Evalue l'inductance en mH pour la résonance à la fréquence F donnée en kHz et
# la capacité C donnée en pF

Lattendue<- function(C, F) {
  
  return(1e3/ (2*pi*F*1000)^2/C)
  
} 