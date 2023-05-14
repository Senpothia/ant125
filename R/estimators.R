

#' Provides a list of linear models for a group of data: LN, RN, LF, RF
#' To be call based on a group
#'
#' @param models made of 4 groups:LN, RN, LF, RF
#' @param groupe 4 groups:LN, RN, LF, RF
#'
#' @return a list of models: LN, RN, LF, LF
#' @export
#'
#' @examples
#'M<-getModels(TAB)
#'est<-estimateurs(M, "LN")

estimators<-function(models, groupe){
  EST<-list()

  noms<-c()

  if(groupe == "LN"){

    gr<-"MODSLN"
    noms<-env$namesFrequencies
  }

  if(groupe == "RN"){

    gr<-"MODSRN"
    noms<-env$namesFrequencies
  }

  if(groupe == "LF"){

    gr<-"MODSLF"
    noms<-env$namesTurns
  }


  if(groupe == "RF"){

    gr<-"MODSRF"
    noms<-env$namesTurns

  }

  i<-1
  for(m in models[[gr]]){

    est<-paste(as.character(m$coefficients[1]), "+", as.character(m$coefficients[2]), "*x +", as.character(m$coefficients[3]),  "*x^2")
    EST[[i]]<-est
    i<-i+1

  }
  names(EST)<-noms
  return(EST)

}
