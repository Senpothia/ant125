#' Extrapole the value of the inductance or resistance for a given frequency based on the measurements recorded
#'
#' @param parameter the frequency in kHz used for extrapole the models of inductance or resistance antenna
#' @param inductance boolean. If TRUE the inductance for the parameter value is estimated, if FALSE, the resistance is evaluated
#'
#' @return the coefficients of the extrapoled model for the antenna inductance or the resistance
#' @export
#'
#' @examples
#'
#' TAB<-getMeasures("ant046")
#' CO<-extModels(TAB, 125, FALSE)
#' CO<-extModels(TAB, 125, TRUE)
#'
extModels<-function(data, parameter, inductance){

  if(inductance){

    LforParamter<-c()
    i<-1
    for( n in env$turns){

      VALS<-data[data$N==n,]
      m <- lm(VALS$L~VALS$F+I(VALS$F^2), data=VALS)

      est<-function(F){m$coefficients[1] + m$coefficients[2]*F + m$coefficients[3]*F^2}
      LforParamter[i]<-est(parameter)
      plot(VALS$F, VALS$L)
      curve(est, 10, 100, col="red", add=TRUE)
      i<-i+1

    }

    TOURS<-env$turns
    D<-data.frame(TOURS, LforParamter)
    print(D)
    m <- lm(LforParamter~D$TOURS+I(D$TOURS^2), data=D)
    est<-function(F){m$coefficients[1] + m$coefficients[2]*F + m$coefficients[3]*F^2}
    plot(D$TOURS, D$LforParamter)
    curve(est, 10, 120, col="red", add=TRUE)
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
      plot(VALS$F, VALS$R)
      curve(est, 10, 100, col="blue", add=TRUE)
      i<-i+1

    }

    TOURS<-env$turns
    D<-data.frame(TOURS, RforParamter)
    print(D)
    m <- lm(RforParamter~D$TOURS+I(D$TOURS^2), data=D)
    est<-function(F){m$coefficients[1] + m$coefficients[2]*F + m$coefficients[3]*F^2}
    plot(D$TOURS, D$RforParamter)
    curve(est, 10, 120, col="blue", add=TRUE)
    return(coef(m))

  }


}
