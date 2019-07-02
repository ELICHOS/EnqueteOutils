#' @author Elie
#' @description "controlpan" permet de controler la repartition des modalites des variables vars.test, dans la sous-population precisee par var.control, selon les modalites de la variable de panel varpan
#' @param data =datas, data.frame de base
#' @param varpan ="Est.dans.l.echantillon.regional", variable de panel
#varpan=datas$"Est.dans.l.echantillon."=="REG",
#' @param var.control ="Departement", variable de controle
#' @param vars.test =c("Anciennete", "rur"), variable de test
#' @return listidfi, un df avec les prop
#' @references Voir les packages "sampling" et "survey"; A creuser!
#' @note
#' @title La fonction "controlpan"
#' @examples
#' @export
controlpan<-function(data=datas,
              varpan="Est.dans.l.echantillon.regional",
              #varpan=datas$"Est.dans.l.echantillon."=="REG",
              var.control="Departement",
              vars.test=c("Anciennete", "rur"),
              mod.keep.sample=c("REG", "Ensemble")){
  data[varpan ,]->dataech
  listi<-list()
  for(i in vars.test){
    #rbind(
    data.frame(round(prop.table(table(data[ , var.control], data[ , i], data[ , varpan]), margin = c(3, 1))*100,2))->tempi#,
    #round(prop.table(table(data[ , varpan], dataech[ , var.control], dataech[, i]), margin=1)*100,2))
    names(tempi)<-c(var.control, i, varpan,  "Freq")
    tempi<-tempi[, c(varpan, var.control, i, "Freq")]
    listi[[i]]<-tempi
  }
  list0i<-list()
  for(i in vars.test){
    #rbind(
    data.frame(round(prop.table(table(data[ , var.control], data[ , i]), margin = 1)*100,2))->temp0i#,
    #round(prop.table(table(data[ , varpan], dataech[ , var.control], dataech[, i]), margin=1)*100,2))
    temp0i[, varpan]<-"Ensemble"
    names(temp0i)<-c(var.control, i, "Freq", varpan)
    temp0i<-temp0i[, c(varpan, var.control, i, "Freq")]
    list0i[[i]]<-temp0i
  }
  listidfi<-list()
  for(i in 1:length(vars.test)){
    rbind(listi[[i]], list0i[[i]])->dfi
    dfi<-dfi[order(dfi[, var.control], dfi[, vars.test[i]]) ,]
    # names(dfi)<-c("var.control", vars.test[i], "varpan", "Freq")
    dfi<-dfi[dfi[, varpan]%in%mod.keep.sample , ]
    names(dfi)[1]<-"sample.var"
    listidfi[[vars.test[i]]]<-dfi
  }

  #
  list("data"=listidfi, "vars.test"=vars.test)
}
