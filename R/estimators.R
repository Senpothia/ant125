

#' Provide a list of linear models for a group of data: LN, RN, LF, RF
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

  # groupes: LN, RN, LF, RF
  # models: liste de tous les models d'après le jeu de données traité
  noms<-c()
  noms1<-c("F=10", "F=20", "F=28.5", "F=40", "F=50", "F=66.6", "F=100")
  noms2<-c("N=60", "N=80", "N=100", "N=120")

  if(groupe == "LN"){

    gr<-"MODSLN"
    noms<-noms1
  }

  if(groupe == "RN"){

    gr<-"MODSRN"
    noms<-noms1
  }

  if(groupe == "LF"){

    gr<-"MODSLF"
    noms<-noms2
  }


  if(groupe == "RF"){

    gr<-"MODSRF"
    noms<-noms2

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
