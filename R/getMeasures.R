

# Load data file for measurements and factors corrections

#' Loads data from file. The file must be located on the appropriate folder. The folder must be named data and placed at the root
#' of the project folder
#'
#' @param file Name of the data file. The file must be saved with .csv extention and given as a string
#'
#' @return A data frame made of the data recorded
#' @export
#'
#' @examples
#' TAB<-getMeasures("data")

getMeasures<-function(file){

  file<-paste(env$dataSource,file, sep="")
  file<-paste(file,".csv", sep="")

  tryCatch(                       # Applying tryCatch

    expr = {
      TAB<-read.table(file, header=TRUE, sep=";", dec=",")
    },

    error = function(e){          # Specifying error message

      TAB<-read.table(file, header=TRUE, sep=",", dec=".")
    },

    warning = function(w){        # Specifying warning message

    },

    finally = {                   # Specifying final message

    }
  )

  env$frequencies<-sort(unique(TAB$F))     # Liste des fréquences
  env$echs<-sort(unique(TAB$ech))
  env$turns<-sort(unique(TAB$N))
  namesFreq<-c()
  namesTurns<-c()

  i<-1
  for(f in env$frequencies){

      namesFreq[i]<-paste("F=", f, sep="")
      i<-i+1
  }

  i<-1
  for(n in env$turns){

    namesTurns[i]<-paste("N=", n, sep="")
    i<-i+1
  }

   env$namesFrequencies<-namesFreq
   env$namesTurns<-namesTurns

  return(TAB)

}
