#' @author Elie
#' @description "control2pan" permet de comparer deux objets objepan
#' @param pan1 =ech, objepan 1
#' @param pan2 =smple, objepan 2
#varpan=datas$"Est.dans.l.echantillon."=="REG",
#' @param var.control ="Departement", variable de controle
#' @param vars.test =c("Anciennete", "rur"), variable de test
#' @return listidfi, un df avec les prop
#' @references Voir basiques::objepan() et les packages "sampling" et "survey"; A creuser!
#' @note
#' @title La fonction "controlpan"
#' @examples
#' @export

control2pan<-function(pan1=ech, pan2=smple){
  #
  vars.test1<-pan1$vars.test[pan1$vars.test%in%pan2$vars.test]
  vars.test2<-pan2$vars.test[pan2$vars.test%in%pan1$vars.test]
  vars.test<-c(vars.test1, vars.test2)
  vars.test<-unique(vars.test)
  #
  res0<-lapply(vars.test, function(k){
    pan1k<-pan1$data[[k]]
    pan2k<-pan2$data[[k]]
    data.frame(cbind(pan1k, pan2k))->df
    names(df)<-c(paste("pan1", names(pan1k), sep="."),
                 paste("pan2", names(pan2k), sep="."))
    return(df)
  }
  )
  lit0<-list()
  for(j in 1:length(vars.test)){
    lit0[[vars.test[j] ]]<-res0[[j]]
  }
  #
  lita<-list()
  for(j in 1:length(vars.test)){
    diffpan1<-matrix(lit0[[j]]$pan1.Freq, ncol = 2, byrow = TRUE)
    diffpan1<-diffpan1[, 1]-diffpan1[, 2]
    diffpan2<-matrix(lit0[[j]]$pan2.Freq, ncol = 2, byrow = TRUE)
    diffpan2<-diffpan2[, 1]-diffpan2[, 2]
    data.frame("diffpan1"=diffpan1, "diffpan2"=diffpan2)->df
    lita[[vars.test[j]]]<-df
  }
  litu<-list()
  for(j in 1:length(vars.test)){
    apply(lita[[j]], MARGIN = 2, FUN = function(x) mean(abs(x), na.rm=TRUE) )->meanu
    apply(lita[[j]], MARGIN = 2, FUN = function(x) median(abs(x), na.rm=TRUE))->medianu
    vu<-c("mean"=meanu, "median"=medianu)
    litu[[vars.test[j]]]<-vu
  }
  return(list(lit0, lita, litu))
}
