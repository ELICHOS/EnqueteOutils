#'  sphinx_to_R: sort un data.frame à partir d'un fichier .que (sphinx) enregistré en csv
#' @description Pour enregistrer le gichier .que en csv: lors de l'import, ne pas splitter par tab, seulement par "=".Iil faut une première ligne vide et un header=TRUE pour créer "V1"
#' @param csv.file fichier csv sphinx
#' @param ...  others arguments passsed to read.csv (header=FALSE, skip=8, sep = c("=", "\\t"),encoding = 'UTF-8')
#' @return un df
#' @export
sphinx_to_R<-function(csv.file= "C:/Users/elie/Desktop/Bureau/FNDSA 21_copie.csv" ,
                      header=FALSE ,encoding="UTF-8" ){

  library(stringr)
  library(dplyr)
  library(basiques)
  source("C:/Users/elie/Desktop/basiques/R/transform_levels_factor.R")
  #
  #header=FALSE, skip=8, sep = c("=", "\\t"),encoding = 'UTF-8'
  read.csv(file = csv.file, header=FALSE ,encoding="UTF-8" ) ->test
  print(dim(test))
  test$V1<-gsub(pattern = "\t", replacement = "rang.", x = test$V1)
  ####
  print(head(test))
  print(test[test$V1=="rang.rang.Première" , ])
  print(subset(test, test$V1=="rang.rang.Première"))
  c(as.numeric(row.names(test[test$V1=="rang.rang.Première" , ])), as.numeric(row.names(test[test$V1=="rang.rang.Première" , ]))-1)->roxti
  print(roxti)
  test[roxti[order(roxti)] , ]->roxti
  roxti<-data.frame(roxti)
  print(nrow(roxti))
  listi<-list()
  for(i in 1:( nrow(roxti)/2 ) ){
    listi[[i]]<-rep(i, times=2)
  }
  roxti$rank.title<-unlist(listi)
  #### V3: Creation d'une variable indiquant le rang dde chaque info ####
  test$V3<-unlist(sapply(X = 1:nrow(test), FUN = function(i){
    if(str_count(test$V1[i], "rang.")==1){
      "titre1"
    } else {if(str_count(test$V1[i], "rang.")==2){
      "titre2"
    } else  {
      if(str_count(test$V1[i], "rang.")==0){
        "titre0"
      } else "error"
    }
    }
  })
  )
  #### V4: Creation d'une variable avec le code variable issu de Sphinx ####
  test$rowrank<-as.numeric(row.names(test))

  test$V4<-unlist((sapply(X = 1:nrow(test), FUN = function(i){
    subset(test, test$V3=="titre1")->test1
    if(test$V3[i]=="titre1"&grepl("Question", x = test$V1[i])){
      test[(i+2), "V2"]->res
    }else{
      subset(test1, test1$rowrank<i)->tempi
      test[tempi[nrow(tempi) , "rowrank"]+2, "V2"]->res
    }
    if(is.null(res)|length(res)==0){
      res<-NA
    }
    as.character(res)
  }
  )
  ))


  # test$V4<-unlist((sapply(X = 1:nrow(test), FUN = function(i){
  #   subset(test, test$V3=="titre1")->test1
  #   if(test$V3[i]=="titre1"&grepl("Question", x = test$V1[i])){
  #     test[(i+2), "V2"]
  #   }else{
  #     subset(test1, test1$rowrank<i)->tempi
  #     test[tempi[nrow(tempi) , "rowrank"]+2, "V2"]
  #   }
  # }
  # )
  # ))
  #### V4: Control ####
  subset(test, test$V1=="rang.rang.Variable")->test.var
  FALSE%in%(test.var$V2==test.var$V4)
  #test.var[test.var$V2!=test.var$V4 ,]
  #### Reshape ####
  reshape(test, direction="wide", v.names = c("V2"), timevar = "V1", idvar = "V4")->test.reshap
  names(test.reshap)<-gsub(pattern = "V2.rang.rang.", replacement = "", x = names(test.reshap))
  names(test.reshap)<-gsub(pattern = "V2.rang.", replacement = "", x = names(test.reshap))
  subset(test.reshap, !is.na(test.reshap$Question))->test.reshap
  test.reshap[] <- lapply(test.reshap, function(x) if(is.factor(x)) droplevels(x) else x)
  #### Reshape control ####
  dim(test.reshap)[1]==length(test$V1[test$V1=="rang.Question"])

  #### Modalités ####
  test.reshap$Type<-tranfofacto(test.reshap$Type,codage = list(
    "choix.multiple"="2",
    "scale"="5",
    "choix.unique"="1",
    "autre.precisez"="6"
  ))
  test.reshap$text<-test.reshap$Libellé
  test.reshap$name<-test.reshap$Variable
  test.reshap$class<-"Q"

  #### Return ####
  res<-list("test.reshap"=test.reshap, "roxti"=roxti)
  return(res)
}
