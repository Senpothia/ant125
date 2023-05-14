

#' graph of joint representations parametrized in F or N
#'
#' @param data dataset from getMeasures()
#' @param groupe  group of models: "RN", "LN", "RF", "LF"
#' @param estimation data from evalEstimator2
#' @param param value of the parameter in the models. Ex: 125kHz
#'
#' @return
#' @export
#'
#' @examples
#'
plotGroups<-function(data, groupe, estimation, param){

  echs<-sort(unique(data$ech))
  frequencies<-sort(unique(data$F))     # Liste des fréquences
  nTypes<-sort(unique(data$N))

  newEch<-max(echs) + 1
  new_row<-c()


  if(groupe == "RN"){


    i<-1

    for(n in nTypes){

      new_row = c(echs=newEch, N=n, F=param, R=estimation[i], L=0)
      data<-rbind(data, new_row)
      i<-i+1
    }


    p1 <- ggplot2::ggplot(data, ggplot2::aes(x=N, y=R, colour=as.factor(F), group=F)) + ggplot2::geom_point() + ggplot2::geom_smooth(method=lm, formula = y ~ poly(x, 2), se=FALSE) + ggplot2::ggtitle(paste("Résistance vs N - Paramètre simulé: F=", as.character(param) )) + ggplot2::labs(x = "N - Tours", y = "Résistance - Ohms", color="F")


  }

  if(groupe == "LN"){


    i<-1

    for(n in nTypes){

      new_row = c(echs=newEch, N=n, F=param, R=0, L=estimation[i])
      data<-rbind(data, new_row)
      i<-i+1
    }


    p1 <- ggplot2::ggplot(data, ggplot2::aes(x=N, y=L, colour=as.factor(F), group=F)) + ggplot2::geom_point() + ggplot2::geom_smooth(method=lm, formula = y ~ poly(x, 2), se=FALSE) + ggplot2::ggtitle(paste("Inductance vs N - pamamètre simulé: F=", as.character(param))) + ggplot2::labs(x = "N - Tours", y = "Inductance - mH", color="F")


  }

  if(groupe == "RF"){

    i<-1

    for(f in frequencies){

      new_row = c(echs=newEch, N=param, F=f, R=estimation[i], L=0)
      data<-rbind(data, new_row)
      i<-i+1
    }

    p1 <- ggplot2::ggplot(data, ggplot2::aes(x=F, y=R, colour=as.factor(N), group=N)) + ggplot2::geom_point() + ggplot2::geom_smooth(method=lm, formula = y ~ poly(x, 2), se=FALSE) + ggplot2::ggtitle(paste("Résistance vs Fréquence - pamamètre simulé: N=", as.character(param))) + ggplot2::labs(x = "F - kHz", y = "Résistance - Ohms", color="N")



  }

  if(groupe == "LF"){

    i<-1

    for(f in frequencies){

      new_row = c(echs=newEch, N=param, F=f, R=0, L=estimation[i])
      data<-rbind(data, new_row)
      i<-i+1
    }

    p1 <- ggplot2::ggplot(data, ggplot2::aes(x=F, y=L, colour=as.factor(N), group=N)) + ggplot2::geom_point() + ggplot2::geom_smooth(method=lm, formula = y ~ poly(x, 2), se=FALSE) + ggplot2::ggtitle(paste("Inductance vs Fréquence - paramètre simulé: N=", as.character(param))) + ggplot2::labs(x = "N - Tours", y = "Inductance - mH", color="N")


  }

  print(p1)


}
