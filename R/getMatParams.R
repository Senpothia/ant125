
#' Convert the list of parameters of a group into a matrix of parameters listed in columns
#'
#' @param coef
#' list from getParams()for one of the groups: RN, LN, RF, LF
#'
#' @return a matrix of the linear model
#' @export
#'
#' @examples
#'
getMatParams<-function(coef){


  MAT <- matrix(unlist(coef), ncol = 3, byrow = TRUE)
  if(names(coef)[1]== "LF"){

    dimnames(MAT) <- list(env$namesTurns, c("D0", "D1", "D2"))
  }

  if(names(coef)[1]== "RF"){

    dimnames(MAT) <- list(env$namesTurns, c("D0", "D1", "D2"))
  }

  if(names(coef)[1]== "LN"){

    dimnames(MAT) <- list(env$namesFrequencies, c("D0", "D1", "D2"))
  }

  if(names(coef)[1]== "LN"){

    dimnames(MAT) <- list(env$namesFrequencies, c("D0", "D1", "D2"))
  }


  return(MAT)

}
