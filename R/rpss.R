#
#' @author Jean V. Adams https://stackoverflow.com/questions/17002101/random-sample-with-multiple-probabilities-in-r
#' @description function to take a Random Proportional Stratified Sample (RPSS) of size n
#' @param stratum factor variable sur laquelle échantilloner
#' @param n taille de l'échantillon   d
#' @param REPLACE
#' @return Résultat de la fonction survey::stratsample. Indice dans stratum
#' @references Voir les packages "sampling" et "survey"; A creuser!
#' @title La fonction "rpss"
#' @examples voir objepan(). rpss(stratum=interaction(df$var1, df$var2), n=Obj)->samplec
#' @export

# stratum=listou[[k]][ , "datacontrolvec"]
# n=final.obj$obj[k]
# probum<-listou[[k]][ , panel.number.var.yc ]
rpss <- function(stratum, n, probum=NULL, REPLACE) {
  props <- table(stratum)/length(stratum)
  nstrat <- as.vector(round(n*props))
  nstrat[nstrat==0] <- 1
  names(nstrat) <- names(props)
  library(survey)
  #if(is.null(probum)){
  #res<-stratsample(stratum, nstrat)
  #}else{
  # original stratsample
  # stratsample<-function (strata, counts)
  # {
  #   strata <- as.character(strata)
  #   n <- length(strata)
  #   rval <- integer(sum(counts))
  #   allrows <- 1:n
  #   j <- 0
  #   for (i in 1:length(counts)) {
  #     thisstrat <- names(counts)[i]
  #     rval[j + (1:counts[i])] <- sample(allrows[strata == thisstrat],
  #                                       counts[i])
  #     j <- j + counts[i]
  #   }
  #   rval
  # }
  strata<-stratum
  counts<-nstrat
  if(is.null(probum)){probumvec<-rep(x = 1, times=length(strata))}else{probumvec<-probum}
  if(length(strata)!=length(probumvec)){message("error probumvec")}
  strata <- as.character(strata)
  n <- length(strata)
  rval <- integer(sum(counts))
  allrows <- 1:n
  j <- 0
  for (i in 1:length(counts)) {
    thisstrat <- names(counts)[i]
    rval[j + (1:counts[i])] <- sample(allrows[strata == thisstrat],
                                      counts[i], prob = probumvec[strata == thisstrat], replace = REPLACE)
    j <- j + counts[i]
  }
  res<-rval
  #}
  return(res)
}
