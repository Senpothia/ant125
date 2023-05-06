

#' Recording a graph
#'
#' @param fileName Nom of the plot file
#' @param p plot to save
#'
#' @return
#' @export
#'
#' @examples
saveGraphPng<-function(fileName, p){

  fileName<-paste("./plots/", fileName)
  fileName<-paste(fileName, ".png")
  png(fileName)
  print(p)
  dev.off()
}
