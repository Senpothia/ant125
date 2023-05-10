

#' lists of model parameters for a group: RN, LN, LF, RF  contained in a set designated by models
#'
#' @param models List of models provided by getmModels()
#' @param groupe groupe of models: LN, RN, LF, RF
#'
#' @return
#' @export
#'
#' @examples
#'
#' M<-getModels(TAB)
#' getModparams(M, "LN")
#'
getModparams<-function(models, groupe){

  PAR<-list()

  # groupes: LN, RN, LF, RF
  # models: liste de tous les models d'après le jeu de données traité

  if(groupe == "LN"){

    gr<-"MODSLN"
  }

  if(groupe == "RN"){

    gr<-"MODSRN"
  }

  if(groupe == "LF"){

    gr<-"MODSLF"
  }


  if(groupe == "RF"){

    gr<-"MODSRF"

  }

  #MODS[["MODSLF"]][1]
  # |       |       |
  # liste   liste   |
  #         interne |
  #                 indice dans la liste interne

  i<-1
  for(m in models[[gr]]){
    mods<-list()
    for(j in 1:3){

      mods[j]<-m$coefficients[j]


    }

    names(mods)<-rep(gr, 3)
    PAR[[i]]<-mods
    i<-i+1

  }

  names(PAR)<-rep(groupe, length(PAR))
  return(PAR)


}
