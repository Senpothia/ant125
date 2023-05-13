#' Extrapole the value of the inductance or resistance for a given frequency based on the measurements recorded
#'
#' @param parameter the frequency in kHz used for extrapole the models of inductance or resistance antenna
#' @param inductance boolean. If TRUE the inductance for the parameter value is estimated, if FALSE, the resistance is evaluated
#' @param data data.frame representing data
#' @param plots boolean. Enable plots
#'
#' @return the coefficients of the extrapoled model for the antenna inductance or the resistance
#' @export
#'
#' @examples
#'
#' TAB<-getMeasures("ant046")
#' CO<-extModels(TAB, 125, FALSE, TRUE)
#' CO<-extModels(TAB, 125, TRUE, TRUE)
#'
extModels<-function(data, parameter, inductance, plots){

  if(inductance){

    LforParamter<-c()
    i<-1
    for( n in env$turns){

      VALS<-data[data$N==n,]
      m <- lm(VALS$L~VALS$F+I(VALS$F^2), data=VALS)

      est<-function(F){m$coefficients[1] + m$coefficients[2]*F + m$coefficients[3]*F^2}
      LforParamter[i]<-est(parameter)
      if(plots){

        plot(VALS$F, VALS$L, main=paste("Inductance vs Frequency  - "," N = ", n, sep="") , xlab="Frequency - kHz", ylab="Inductance - mH")
        curve(est, 10, 100, col="red", add=TRUE)

      }

      i<-i+1

    }

    TOURS<-env$turns
    D<-data.frame(TOURS, LforParamter)

    m <- lm(LforParamter~D$TOURS+I(D$TOURS^2), data=D)
    est<-function(F){m$coefficients[1] + m$coefficients[2]*F + m$coefficients[3]*F^2}
    if(plots){

      plot(D$TOURS, D$LforParamter, main=paste("Inductance vs Turns  - "," F = ", parameter, sep="") , xlab="Turns", ylab="Inductance - mH")
      curve(est, 10, 120, col="red", add=TRUE)

    }

    return(coef(m))

  }


  if(!inductance){

    RforParamter<-c()
    i<-1
    for( n in env$turns){

      VALS<-data[data$N==n,]
      m <- lm(VALS$R~VALS$F+I(VALS$F^2), data=VALS)

      est<-function(F){m$coefficients[1] + m$coefficients[2]*F + m$coefficients[3]*F^2}
      RforParamter[i]<-est(parameter)
      if(plots){

        plot(VALS$F, VALS$R, main=paste("Resistance vs Frequency  - "," N = ", n, sep="") , xlab="Frequency - kHz", ylab="Resistance - Ohms")
        curve(est, 10, 100, col="blue", add=TRUE)

      }

      i<-i+1

    }

    TOURS<-env$turns
    D<-data.frame(TOURS, RforParamter)

    m <- lm(RforParamter~D$TOURS+I(D$TOURS^2), data=D)
    est<-function(F){m$coefficients[1] + m$coefficients[2]*F + m$coefficients[3]*F^2}
    if(plots){

      plot(D$TOURS, D$RforParamter, main=paste("Resistance vs Turns  - "," F = ",parameter, sep="") , xlab="Turns", ylab="Resistance - Ohms")
      curve(est, 10, 120, col="blue", add=TRUE)

    }

    return(coef(m))

  }


}
