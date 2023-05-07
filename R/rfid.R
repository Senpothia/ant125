


#' Calculation of the antenna current in mA
#'
#' @param x antenna resistance
#'
#' @return
#' @export
#'
#' @examples
#'
Iant<-function(x){  # En milliampères

  I<-(4/pi)*((env$vdd-env$Vss)/(x+env$Rser+2*env$Rad))*1000
  return(I)

}


#' Optimise the value of the antenna resistance given a current antenna value
#'
#' @param I
#'
#' @return
#' @export
#'
#' @examples
#'
RforI<-function(I){
  rx<-function(x){abs(Iant(x) - I)}
  r<-optimize(rx, c(0, 10000))
  return (r)
}



#' Calculation of the antenna current in mA
#'
#' @param R antenna resistance
#'
#' @return
#' @export
#'
#' @examples
#'
IantA<-function(R){  # En Ampères

  I<-(4/pi)*((env$vdd-env$Vss)/(R+env$Rser+2*env$Rad))
  return(I)

}


#' Evaluates the antenna current from the antenna resistance estimator
#'
#' @param N number of turns of the antenna
#' @param Restimator antenna resistance estimator obtained from the regression coefficients and given in string form
#'
#' @return
#' @export
#'
#' @examples
In<-function(N, Restimator){

  RN<-parse(text=Restimator)
  R<-eval(RN)
  I<-Iant(R)
  return(I)

}



#' Evaluate the antenna magnetic field based on the antenna resistance estimator and the number of turns
#'
#' @param N number of turns
#' @param Restimator an estiamtor of the antenna resistance obtained from the regression coefficients and given in string form
#'
#' @return
#' @export
#'
#' @examples
Best<-function(N, Restimator){

  RN<-parse(text=Restimator)
  R<-eval(RN)
  I<-Iant(R)
  b<-I*N*env$r^2/env$z^3
  return(b)


}


#'  Estimation of the resonance capacity
#'
#' @param N number of turns of the antenna
#' @param L inductance of the antenna
#'
#' @return
#' @export
#'
#' @examples
#'
Cres<-function(N, L){

  L<-eval(L)
  c<-1/((2*pi*en$F0)^2*L(N))^-1

}

#' Resonance frequency in kHz for L in Henry
#'
#' @param L inductance of the antenna
#'
#' @return
#' @export
#'
#' @examples
Cresonnance<-function(L, F){  # Fréquence de résonnance en kHz pour L en Henry

  return(1/((2*pi*F*1000)^2*L*1e-03) - env$c2)
}


#' Antenna voltage estimation
#'
#' @param N number of turns of the antenna
#' @param R resistance antenna
#' @param L inductance antenna
#'
#' @return
#' @export
#'
#' @examples
Vant<-function(N, R, L){
  R<-eval(R)
  L<-eval(L)
  v<-Iant(R(N))/(2*pi*env$Fo*Cres)

}


#' Estimation of tuning frequency as a function of circuit parameters
#'
#' @param L Inductance of the antenna
#' @param C Capacitance in pf
#'
#' @return
#' @export
#'
#' @examples
#'
Fres<-function(L,C){

  f<-1/(2*pi*sqrt(L*1e-03*C*1e-12))
  return(f)

}


#'Evaluate the inductance in mH for the resonance at the frequency F given in kHz and the capacitance C given in pF
#'
#' @param C capacitance value in pF
#'
#' @return
#' @export
#'
#' @examples
#'
Lattendue<- function(C, F) {

  return(1e3/ (2*pi*F*1000)^2/C)

}
