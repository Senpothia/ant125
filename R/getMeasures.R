

# Load data file for measurements and factors corrections

#' Loads data from file. The file must be located on the appropriate folder. The folder must be named data and placed at the root
#' of the project folder
#'
#' @param file Name of the data file. The file must be saved with .csv extention
#' @param s The separator of data in the data file: "," or ";"
#' @param d The decimal point used in the data file: "," or "."
#'
#' @return A data frame made of the data recorded
#' @export
#'
#' @examples
#' TAB<-getMeasures("data", ",", ".")

getMeasures<-function(file, s=";", d=","){

  file<-paste("./data/",file, sep="")
  file<-paste(file,".csv", sep="")
  TAB<-read.table(file,header=TRUE,sep=s,dec=d)
  return(TAB)

}
