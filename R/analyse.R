
#' Data analyse and antenna parameters estimation.Provide N tours according an inductance value or a antenna current selected.
#'Provide resonant capacitance according an inductance value estimated.
#'
#' @param data The data file to be processed
#'
#' @return parameters like N, L, C for antenna design
#' @export
#'
#' @examples
#' analyse()
#'
analyse<-function(data){

  TAB<-getMeasures("data")
  CS<-regMods("data")

  cap<-""
  Iantenne<-""

  repeat{

    cap <- readline(prompt="Entrez la valeur de la capacité d'accord en pF: ")

    if(cap == "0"){

      break

    }else{


      # Estimation en fonction de la capacité d'accord


      # convert character into integer
      cap2 <- as.integer(cap)*1e-12

      print("Valeur de capacité entrée: ")
      print(cap2)

      # c2<-(39e-12*1.5e-9)/(39e-12+1.5e-9)  # Influence pont capacitif

      Cacc<-(cap2)+env$c2

      print("Capacité d'accord réelle: ")
      print(Lattendue(Cacc, 125))

      print("Inductance attendue en mH: ")
      Latt<-Lattendue(Cacc, 125)
      print(Latt)

      n<-optimisEst(CS[4], Latt, 125, c(60,120))

      #--------------------------------------------------------------------------------

      # Compte rendu

      insight::print_color("-------   RESULTAT D'OPTIMISATION    -----------\n", "red")
      insight::print_color("\n","red")
      insight::print_color(paste("Optimisation pour F=125kHz et C=", cap), "red")
      insight::print_color("pF\n","red")
      insight::print_color( paste("Jeu de données utilisé: ", env$dataSource), "red")
      insight::print_color("\n","red")
      insight::print_color("\n","red")
      insight::print_color( paste("Nombre de spires estimés: ", as.character(n[1])), "red")
      insight::print_color("\n","red")
      insight::print_color("------------------------------------------------\n", "red")


      # Estimation en fonction du courant d'antenne

      Iantenne <- readline(prompt="Entrez la valeur du courant d'antenne en mA: ")

      if(Iantenne == "0"){

        break

      }

      # convert character into integer
      Iantenne2 <- as.integer(Iantenne)*1e-3
      message<-cat("Valeur du courant d'antenne choisie: ", Iantenne, "mA", "\n")

      insight::print_color(message, "red")

      r<-RforI(Iantenne2)

      message<-cat("Résistance estimée: ", as.character(r[1]), "\n")
      insight::print_color(message, "red")
      insight::print_color("------------------------------------------------\n", "red")

      Rattendue<-as.numeric(r[1])

      n<-optimisEst(CS[3], Rattendue, 125, c(60,120))

      insight::print_color( paste("Nombre de spires estimés: ", as.character(n[1])), "red")
      insight::print_color("\n","red")
      insight::print_color("------------------------------------------------\n", "red")

      Lcal<-evalEstimator2(CS[3], 125, as.numeric(n[1]))

      insight::print_color(paste("Inductance antenne: ", Lcal), "red")
      insight::print_color("\n","red")
      insight::print_color(paste("Capacité d'accord: ", Cresonnance(Lcal, 125)), "red")
      insight::print_color("\n","red")
      insight::print_color("-----------------  FIN DE RAPPORT  ------------\n", "red")

    }

  }

  insight::print_color("FIN", "red")


}
