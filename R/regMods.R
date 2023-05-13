
#' Returns a list of all group coefficient matrices: RN, RF, LN, LF
#' after regression on all the coefficients matrices
#'
#' @param data data file to be processed given as a string
#'
#' @return
#' @export
#'
#' @examples
#' regMods("data")
#'
regMods<-function(data){

  nomCoefs<-c("D0" , "D1", "D3")

  TAB<-getMeasures(data)

  # nTypes<-sort(unique(TAB$N))
  # frequencies<-sort(unique(TAB$F))

  MODS<-getModels(TAB)

  paramsLF<-getModparams(MODS, "LF")
  matLF<-getMatParams(paramsLF)
  dimnames(matLF) <- list(env$namesTurns,nomCoefs)

  paramsRF<-getModparams(MODS, "RF")
  matRF<-getMatParams(paramsRF)
  dimnames(matRF) <- list(env$namesTurns, nomCoefs)

  paramsLN<-getModparams(MODS, "LN")
  matLN<-getMatParams(paramsLN)
  dimnames(matLN) <- list(env$namesFrequencies, nomCoefs)

  paramsRN<-getModparams(MODS, "RN")
  matRN<-getMatParams(paramsRN)
  dimnames(matRN) <- list(env$namesFrequencies, nomCoefs)

  print("----  REGRESSION SUR LES COEFFICIENTS REELS   ------")

  MLF<-paraModsRegs(matLF, env$turns)
  MRF<-paraModsRegs(matRF, env$turns)
  MRN<-paraModsRegs(matRN, env$frequencies)
  MLN<-paraModsRegs(matLN, env$frequencies)

  COEFS<-list(MLF, MRF, MRN, MLN)
  names(COEFS)<-c("LF", "RF", "RN", "LN")

  print(COEFS)
  return(COEFS)

}
