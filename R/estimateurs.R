
#' Fourni la liste des fonctions de régression d'un groupe pour un ensemble de modèles
#'l'appel se fait sur un groupe: LN, RN, LF, RF
#'
#' @param models models possède 4 groupes:LN, RN, LF, RF
#' @param groupe 4 groupes:LN, RN, LF, RF
#'
#' @return a liste of models
#' @export
#'
#' @examples
#'M<-getModels(TAB)
#'estimateurs(M, "LN")

estimateurs<-function(models, groupe){
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

  # MDS<-parse(text=gr)
  # MDS<-eval(MDS)

  i<-1
  for(m in models[[gr]]){

    est<-paste(as.character(m$coefficients[1]), "+", as.character(m$coefficients[2]), "*x +", as.character(m$coefficients[3]),  "*x^2")
    EST[[i]]<-est
    i<-i+1

  }
  names(EST)<-noms
  return(EST)

}
