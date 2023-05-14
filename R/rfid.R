
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


#' Optimises the value of the antenna resistance given a current antenna value
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



#' Evaluates the antenna magnetic field based on the antenna resistance estimator and the number of turns
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


#' Evaluates the number of turns for peak value of the antenna magnetic field
#'
#' @param matrice Matrix of coefficents for antenna resistance estimator
#' @param param the value of the frequency for evaluation of the resistance antenna estimator
#' @param interval The range of value to estimate the peak value of the antenna magnetic field
#'
#' @return the number of turn to achieve to peak value of the antenna magnetic field
#' @export
#'
#' @examples
#'
getMaxBest<-function(matrice, param, interval){

  MATRICE <- matrix(unlist(matrice), ncol = 3, byrow = FALSE)

  x<- param

  c<-MATRICE[,1]
  b<-MATRICE[,2]
  a<-MATRICE[,3]

  C<-c[1] + c[2]* x + c[3]* x^2
  B<-b[1] + b[2]* x + b[3]* x^2
  A<-a[1] + a[2]* x + a[3]* x^2


  Rest<-function(y) { C + B * y + A * y^2 }
  Iantenna<-function(y){(4/pi)*((env$vdd-env$Vss)/(Rest(y)+env$Rser+2*env$Rad))}
  Best<-function(y){Iantenna(y)*y*env$r^2/env$z^3}

  major<-optimize(Best, interval, maximum = TRUE)
  return(major)

}


#' Evaluates the number of turns for peak value of the antenna current
#'
#' @param matrice Matrix of coefficents for antenna resistance estimator
#' @param param the value of the frequency for evaluation of the resistance antenna estimator
#' @param interval The range of value to estimate the peak value of the antenna current
#'
#' @return the number of turn to achieve to peak value of the antenna current
#' @export
#'
getMaxIest<-function(matrice, param, interval){

  MATRICE <- matrix(unlist(matrice), ncol = 3, byrow = FALSE)

  x<- param

  c<-MATRICE[,1]
  b<-MATRICE[,2]
  a<-MATRICE[,3]

  C<-c[1] + c[2]* x + c[3]* x^2
  B<-b[1] + b[2]* x + b[3]* x^2
  A<-a[1] + a[2]* x + a[3]* x^2


  Rest<-function(y) { C + B * y + A * y^2 }
  Iantenna<-function(y){(4/pi)*((env$vdd-env$Vss)/(Rest(y)+env$Rser+2*env$Rad))}


  major<-optimize(Iantenna, interval, maximum = TRUE)
  return(major)

}

#'  Estimation of the resonance capacity
#'
#' @param N number of turns of the antenna
#' @param L inductance of the antenna estimator given as string. N must be the variable. e.g: 2*N+3
#'
#' @return
#' @export
#'
#' @examples
#' c<-Cres(60, "2*N+6")
#' [1] 4.974281e+13
#'
Cres<-function(N, L){

  L<-parse(text=L)
  Lant<-eval(L)
  c<-1/((2*pi*env$F0)^2*Lant)^-1

}

#' Resonnance capacitance calculation
#'
#' @param L inductance of the antenna in mH
#' @param F frequency in kHz
#' @return
#' @export
#'
#' @examples
#' Cresonnance(1.65, 125)
#' [1] 9.444968e-10
#'
Cresonnance<-function(L, F){  # Fréquence de résonnance en kHz pour L en Henry

  return(1/((2*pi*F*1000)^2*L*1e-03) - env$c2)
}


#' Antenna voltage estimation
#'
#' @param N number of turns of the antenna
#' @param R resistance antenna estimator given as a string, N must be the variable. e.g: 2*N+2
#' @param L inductance antenna estimator given as a string, N must be the variable. e.g: 3*N+6
#'
#'
#' @return
#' @export
#'
#' @examples
#' Vant(100, "2*N+2", "3*N+6")
#' print(Vant(100, "2*N+2", "3*N+6"))
#' v<-Vant(100, "2*N+2", "3*N+6")
#' v
#' [1] 3.395644e-19
#'
Vant<-function(N, R, L){

  R<-parse(text=R)
  L<-parse(text=L)
  Rant<-eval(R)
  Lant<-eval(L)
  v<-Iant(Rant)/(2*pi*env$F0*Cres(N, Lant))

}


#' Provides antenna voltage given the number of turns, the resistance and inductance value of the antenna
#'
#' @param N number of turns
#' @param R antenna resistance in Ohm
#' @param L antenna inductance in Henry
#'
#' @return
#' @export
#'
#' @examples
#' v<-getVant(70, 200, 1.6e-3)
#'
getVant<-function(N, R, L){

  C<-1/((2*pi*env$F0)^2*L)^-1
  v<-Iant(R)/(2*pi*env$F0*C)

}


#' Estimation of tuning frequency as a function of circuit parameters
#'
#' @param L Inductance of the antenna in mH
#' @param C Capacitance in pf
#'
#' @return
#' @export
#'
#' @examples
#'Fres(1.65, 980)
#'[1] 125159.9
#'
Fres<-function(L,C){

  f<-1/(2*pi*sqrt(L*1e-03*C*1e-12))
  return(f)

}


#'Evaluates the inductance in mH for the resonance at the frequency F given in kHz and the capacitance C given in pF
#'
#' @param C capacitance value in pF
#'
#' @return
#' @export
#'
#' @examples
#'Lattendue(980e-12, 125)
#'[1] 1.654223
#'
Lattendue<- function(C, F) {

  return(1e3/ (2*pi*F*1000)^2/C)

}
