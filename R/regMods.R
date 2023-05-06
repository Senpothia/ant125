
#' Returns a list of all group coefficient matrices: RN, RF, LN, LF
#' after regression on all the coefficients matrices
#'
#' @param data data file to be processed
#'
#' @return
#' @export
#'
#' @examples
#' regMods("data")
#'
regMods<-function(data){

  nomCoefs<-c("D0" , "D1", "D3")
  TAB<-getMeasures(data, ",", ".")
  # N<-sort(unique(TAB$N))
  # F<-sort(unique(TAB$F))
  nTypes<-sort(unique(TAB$N))
  frequencies<-sort(unique(TAB$F))

  MODS<-getModels(TAB)

  paramsLF<-getModparams(MODS, "LF")
  matLF<-getMatParams(paramsLF)
  dimnames(matLF) <- list(nTypes,nomCoefs)

  paramsRF<-getModparams(MODS, "RF")
  matRF<-getMatParams(paramsRF)
  dimnames(matRF) <- list(nTypes, nomCoefs)

  paramsLN<-getModparams(MODS, "LN")
  matLN<-getMatParams(paramsLN)
  dimnames(matLN) <- list(frequencies, nomCoefs)

  paramsRN<-getModparams(MODS, "RN")
  matRN<-getMatParams(paramsRN)
  dimnames(matRN) <- list(frequencies, nomCoefs)

  print("----  REGRESSION SUR LES COEFFICIENTS REELS   ------")

  MLF<-paraModsRegs(matLF, nTypes)
  MRF<-paraModsRegs(matRF, nTypes)
  MRN<-paraModsRegs(matRN, frequencies)
  MLN<-paraModsRegs(matLN, frequencies)

  COEFS<-list(MLF, MRF, MRN, MLN)
  names(COEFS)<-c("LF", "RF", "RN", "LN")
 # MATRICE <- matrix(unlist(COEFS), ncol = 3, byrow = FALSE)
  print(COEFS)
  return(COEFS)

}
