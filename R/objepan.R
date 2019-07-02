#' @author Elie
#' @description "objepan" permet de sampler un data.frame en recourant a des probabilites (statistiques) externes.
#' @param Objectif.total    400 # taille du sample
#' @param data.frame.sample  datas.pannb #data.frame avec les donnees non samplees
#' @param nb.criteres  2 nb de criteres externes de sample (sur la population de reference)
#' @param datas.crit  list(popudep.AURA, sample.type) liste des data.frame avec les donnees externes, dans l'ordre
#' @param type.crit  c("eff", "prop") pour chaque df externe, dans l'ordre: s'agit-il des effectifs ou des proportions?
#' @param sample.index  c("Departement", "Instit.Assoc") pour chaque df externe, dans l'ordre: la variable correpsondante dans le df non sample
#' @param crit.index  c("Nom.du.departement", "type") pour chaque df externe, dans l'ordre: la variable correspondante dans chaque df externe
#' @param exclude.sample  c("0", "0") pour chaque df externe, dans l'ordre: levels non pertinents qui trainent
#' @param controls  FALSE logical: des variables dans le df non sample serviront-elles de controle au moment du sample?
#' @param controls.vars  c("Statut", "Milieu") nom (characters) des variables de controle
#' @param var.poid.supp  "var.poid.supp"
#' @param hierarch.crit  TRUE
#' @param idvar.sample  NULL le cas echeant, nom (character) de la vairable d'id dans le df non sample
#' @return res<-list("final.obj"=final.obj, "crit"=lista, "sample.size"=sample.size, "sample.id"=sample.id, "newdata"=listou.df, "newdata.list"=listou)
#' @references Voir les packages "sampling" et "survey"; A creuser!
#' @note Typiquement: la repartition effective de la population par departements dans l'ensemble de la region AURA hors enquete. objepan permet aussi de sampler, a partir de ces prob externes, en prenant en compte la representativite interne de chaque sous-echantillon: il me fait 20 personnes de la Loire, mais dans mon data.frame il y a 50% d'hommes dans la loire, je veux donc selectionner dans  ces 20 personnes environ 10 femmes et 10 hommes.
#' @title La fonction "objepan"
#' @examples
#' objepan<-function(Objectif.total = 400, data.frame.sample=datas.pannb, nb.criteres=2,
#'datas.crit=list(popudep.AURA, sample.type), type.crit=c("eff", "prop"),
#'sample.index=c("Departement", "Instit.Assoc"),
#'crit.index=c("Nom.du.departement", "type"),
#'exclude.sample=c("0", "0"),
#'controls=FALSE,
#'controls.vars=c("Statut", "Milieu"),
#'var.poid.supp="var.poid.supp",
#'hierarch.crit=TRUE,
#'idvar.sample=NULL)->test
#' @export



#Parametres de test (projet: ENQ CONJ)
# Objectif.total = 400; data.frame.sample=datas.pannb; nb.criteres=2;
# datas.crit=list(popudep.AURA, sample.type); type.crit=c("eff", "prop");
# sample.index=c("Departement", "Instit.Assoc");
# crit.index=c("Nom.du.département", "type");
# exclude.sample=c("0", "0")
# datas.pannb<-datas
# datas.pannb$var.poid.supp<-poids.sample(data = datas.pannb, varpoids = c("Ancienneté", "Fréquence.contact"), orderpoids = list(c(4, 3, 1, 2, 5), c(1, 3, 4, 2)))$poids
# var.poid.supp="var.poid.supp"
# Objectif.total = 400; data.frame.sample=datas.pannb2; nb.criteres=2;
#                    datas.crit=list(popudep.AURA, sample.type); type.crit=c("eff", "prop");
#                    sample.index=c("Departement", "Instit.Assoc");
#                    crit.index=c("Nom.du.département", "type");
#                    exclude.sample=c("0", "0");
#                    controls=TRUE;
#                    controls.vars=c("rur", "anclog");
#                    poid.supp = FALSE; var.poid.supp = "var.poid.supp";
#                    hierarch.crit=FALSE;
#                    idvar.sample=NULL
####

objepan<-function(Objectif.total = 400, data.frame.sample=datas.pannb, nb.criteres=2,
                  datas.crit=list(popudep.AURA, sample.type), type.crit=c("eff", "prop"),
                  sample.index=c("Departement", "Instit.Assoc"),
                  crit.index=c("Nom.du.departement", "type"),
                  exclude.sample=c("0", "0"),
                  controls=TRUE,
                  controls.vars=c("Milieu"),
                  poid.supp=FALSE,
                  var.poid.supp="var.poid.supp",
                  hierarch.crit=TRUE,
                  idvar.sample=NULL){
  #source(file="C:/Users/Flora2/Desktop/basiques/R/rpss.R", echo=FALSE)
  #library(basiques)
  # Premiere partie ####
  if(length(datas.crit)!=nb.criteres|length(sample.index)!=nb.criteres|length(exclude.sample)!=nb.criteres){message("Error length")}else{"Length ok"}
  #
  datacontrol<-data.frame.sample[ , names(data.frame.sample)%in%controls.vars]
  if(length(controls.vars)>1){
  apply(X = datacontrol, MARGIN = 1, FUN = function(x) paste(x, collapse="-"))->datacontrolvec
  } else {if(length(controls.vars)==1){
    datacontrol->datacontrolvec
  } else {message("fatal error occured")}
  }
  data.frame.sample$datacontrolvec<-datacontrolvec
  props <- table(data.frame.sample$datacontrolvec)/length(data.frame.sample$datacontrolvec)
  nstrat<-data.frame(props)
  names(nstrat)<-c("Var1", "Poids.control")
  #nstrat$controlvars<-row.names(nstrat)
  merge(data.frame.sample, nstrat, by.x = "datacontrolvec", by.y = "Var1", all.x=TRUE, all.y = FALSE, sort = FALSE)->data.frame.sample
 as.numeric(factor(data.frame.sample$Poids.control))->data.frame.sample$Poids.control
 summary(data.frame.sample$Poids.control)
 if(poid.supp==TRUE){
 as.numeric(factor(data.frame.sample$var.poid.supp))->data.frame.sample$var.poid.supp
 data.frame.sample$final.poid<-apply(data.frame.sample[, c(var.poid.supp, "Poids.control")], MARGIN = 1, FUN = mean)
 } else {if(poid.supp==FALSE){
   data.frame.sample$final.poid<-data.frame.sample$Poids.control
 }}
 data.frame.sample$final.poid2<-NA
  #
  for(k in 1:nb.criteres){
    datas.crit[[k]][, crit.index[k]]<-droplevels(datas.crit[[k]][, crit.index[k]])
  }
  #
  listo<-list()
  for(i in 1:nb.criteres){
    as.character(datas.crit[[i]][, crit.index[i]])->listo[[i]]
  }
  apply(expand.grid(listo), 1, paste, collapse=".")->obj.vars
  #
  data.frame.sample[ , sample.index]->df
  apply(df, MARGIN = 2, FUN = as.character)->df
  data.frame.sample$code<-apply(df, MARGIN = 1, paste, collapse=".")
  #
  listo.prop<-list()
  for( i in 1:nb.criteres){
    if(type.crit[i]=="eff"){
      vals<-datas.crit[[i]][, names(datas.crit[[i]])%w/o%crit.index[i]]
      vals/sum(vals)->listo.prop[[i]]
    } else { if(type.crit[i]=="prop"){
      datas.crit[[i]][, names(datas.crit[[i]])%w/o%crit.index[i]]->listo.prop[[i]]
    }
    }
  }
  apply(expand.grid(listo.prop), 1, paste, collapse="*")->obj.prop
  unlist(lapply(X = obj.prop, FUN = function(x) eval(parse(text = x))))->prop
  data.frame(cbind(obj.vars,
                   data.frame(matrix(data = unlist(strsplit(obj.vars, split='.', fixed=TRUE)), nrow = length(obj.vars), ncol = nb.criteres, byrow = TRUE)),
                   obj.prop,
                   prop
                   ),
             stringsAsFactors = FALSE)->obj

  #
  library(plyr)
  plyr::count(df = data.frame.sample, vars = sample.index)->dfo
  dfo$rmtrue<-apply(X = dfo[ , names(dfo)!="freq"], MARGIN = 1, FUN = function(k) any(k %in% c(exclude.sample)))
  dfo<-subset(dfo, subset = dfo$rmtrue!=TRUE)
  dfo$obj.vars<-apply(dfo[ , names(dfo)%w/o%c("freq", "rmtrue")], MARGIN = 1, paste, collapse=".")
  #
  merge(obj, dfo, by="obj.vars")->final.obj
  final.obj$obj<-round(final.obj$prop*Objectif.total, 0)
  final.obj[ c(sample.index, "prop", "freq", "obj")]->final.obj
  #
  lista<-list()
  for(k in 1:length(sample.index)){
    data.frame(cbind(table(data.frame.sample[, sample.index[k]], exclude = exclude.sample),
               prop.table(table(data.frame.sample[, sample.index[k]], exclude = exclude.sample))))->tempodf
    names(tempodf)<-c("Effectifs", "Frequence")
    tempodf$varid<-row.names(tempodf)
    if(type.crit[[k]]=="eff"){
      Frequence.obj<-datas.crit[[k]][, names(datas.crit[[k]])!=crit.index[k]]/sum(datas.crit[[k]][, names(datas.crit[[k]])!=crit.index[k]])
      Effectifs.obj<-round(Frequence.obj*Objectif.total, 0)
      datas.crit[[k]]<-cbind(datas.crit[[k]], Frequence.obj, Effectifs.obj)
    } else {if(type.crit[[k]]=="prop"){
      Frequence.obj<-datas.crit[[k]][, names(datas.crit[[k]])!=crit.index[k]]
      Effectifs.obj<-round(Frequence.obj*Objectif.total, 0)
      datas.crit[[k]]<-cbind(datas.crit[[k]], Frequence.obj, Effectifs.obj)
    }
    }
    merge(tempodf, datas.crit[k], by.x="varid", by.y=crit.index[k])->tempodf
    tempodf[, c("varid",
                "Effectifs",
                "Effectifs.obj",
                "Frequence",
                "Frequence.obj",
                names(datas.crit[k])[names(datas.crit[k])!=crit.index[k]])]->lista[[sample.index[k]]]
  }
  #### listou : individus par département et association: premier niveau ####
  listou<-lapply(1:nrow(final.obj), function(k){
    vecu<-sapply(X = 1:nb.criteres, FUN = function(u){
      #vecu<-list()
      if(u<nb.criteres){
        paste("data.frame.sample[, '", sample.index[u], "']==final.obj[k , names(final.obj)=='", sample.index[u], "']&", sep="")#->vecu[[u]]
      } else {
        if(u==nb.criteres){
          paste("data.frame.sample[, '", sample.index[u], "']==final.obj[k , names(final.obj)=='", sample.index[u], "']", sep="")#->vecu[[u]]
        }
      }
      #paste(vecu, collapase="")->vecu
      #return(vecu)
    }
    )
    paste0(vecu, collapse="")->vecu
    #
    subset(data.frame.sample,
                    subset=eval(parse(text=vecu))
    )
  }
  )
#### SAMPLE on listou: deuxième niveau  ####
  for(k in seq_along(listou) ){
    if(final.obj$freq[k]<=final.obj$obj[k]){
      listou[[k]]$SAMPLE<-TRUE
    }else{
      if(final.obj$freq[k]>final.obj$obj[k]){
        if(controls==TRUE){
          savec<-rep(FALSE, times=nrow(listou[[k]]))
          #sample(x = 1:nrow(listou[[k]]), size = final.obj$obj[k], replace = FALSE, prob = listou[[k]][, var.poid.supp])->samplec

          if(length(controls.vars)>1){
            vecy<-c()
          # for(y in 1:length(controls.vars)){
          #   if(y<length(controls.vars)){
          #     paste("listou[[k]][, '", controls.vars[y], "'],", sep = "")->vecy[y]
          #   } else {
          #     if(y==length(controls.vars)){
          #       paste("listou[[k]][, '", controls.vars[y], "']", sep = "")->vecy[y]
          #     }
          #   }
          # }
          for(y in 1:length(controls.vars)){
             if(y<length(controls.vars)){
               paste(controls.vars[y], sep = "")->vecy[y]
             } else {
               if(y==length(controls.vars)){
                 paste(controls.vars[y], sep = "")->vecy[y]
               }
             }
           }
          #vecy<-paste0(vecy, collapse="")
          #vecy<-paste("interaction(", vecy, ", drop=TRUE)", sep="")
          message(vecy)
          message(class(vecy))
          #listou[[k]]$vecy<-apply(listou[[k]][, vecy], MARGIN = 1, FUN = paste, collapse="-")
          veco<-apply(listou[[k]][, vecy], MARGIN = 1, FUN = paste, collapse="-")
          if(poid.supp==FALSE){
          rpss(stratum=veco, n=final.obj$obj[k])->samplec
          } else {message("error: pas de poids supp!!!!")}
          #rpss(stratum=listou[[k]][ , "datacontrolvec"], n=final.obj$obj[k], probum = listou[[k]][ , var.poid.supp ])->samplec
          # data.frame.sample$final.poid<-data.frame.sample$final.poid/sum(data.frame.sample$final.poid)
          #listou[[k]]$final.poid2<-as.numeric(factor(listou[[k]]$final.poid))
          #listou[[k]]$final.poid/sum(data.frame.sample$final.poid)
          #sample(x = 1:nrow(listou[[k]]), size = final.obj$obj[k], replace = FALSE, prob = listou[[k]][, "final.poid2"])->samplec
          } else {if(length(controls.vars)==1){
            #listou[[k]]$vecy<-listou[[k]][ , controls.vars]
            rpss(stratum=listou[[k]][ , controls.vars], n=final.obj$obj[k])->samplec
          }}
          savec[samplec]<-TRUE
          listou[[k]]$SAMPLE<-savec
        }
        if(controls==FALSE){
          if(poid.supp==TRUE){
            message("error: pas de poids supp!!!!")
            #savec<-rep(FALSE, times=nrow(listou[[k]]))
            #sample(x = 1:nrow(listou[[k]]), size = final.obj$obj[k], replace = FALSE, prob = listou[[k]][, var.poid.supp]
            #)->samplec
           ## savec[samplec]<-TRUE
           # listou[[k]]$SAMPLE<-savec
          } else {
            if(poid.supp==FALSE){
              savec<-rep(FALSE, times=nrow(listou[[k]]))
              #listou[[k]]$vecy<-listou[[k]][ , controls.vars]
              sample(x = 1:nrow(listou[[k]]), size = final.obj$obj[k], replace = FALSE
              )->samplec
              savec[samplec]<-TRUE
              listou[[k]]$SAMPLE<-savec
            }
          }
        }
      }
    }
  }
  final.obj$SAMPLE<-unlist(lapply(listou, FUN = function(k){
    #length(
    k$SAMPLE->temp
    length(temp[temp==TRUE])#[isTRUE(k$SAMPLE)]
  #)
  })
  )
  Ensemble<-data.frame(
  "prop"=sum(final.obj$prop), "freq"=sum(final.obj$freq), "obj"=sum(final.obj$obj), "SAMPLE"=sum(final.obj$SAMPLE)
  )
#  }
  do.call("rbind", listou)->listou.df
  #
  lousti<-list()
  for(i in 1:nb.criteres){
    if(i==1){
      lousti[[i]]<-ddply(final.obj,.(eval(parse(text = sample.index[i]))),transform,freq.1 = sum(eval(parse(text = "freq"))),obj.1 = sum(eval(parse(text = "obj"))))[, c("freq.1", "obj.1")]
    }
    if(i>1){
      namo<-paste(c("freq.", "obj."), i, sep="")
      lousti[[i]]<-ddply(final.obj,.(eval(parse(text = sample.index[1])), eval(parse(text = sample.index[i]))),transform,freq.i = sum(eval(parse(text = "freq"))),obj.i = sum(eval(parse(text = "obj"))))#[, c("freq.i", "obj.i")]
      #names(lousti[[i]])<-namo
    }
  }
  # if(hierarch.crit=TRUE){
  #   for(k in seq_along(listou) ){
  #     for(cri in nb.criteres){
  #       if(cri==1){
  #       if(lista[[cri]][lista[[cri]]$varid==final.obj[k , sample.index[cri]] , ]$Effectifs<lista[[cri]][lista[[cri]]$varid==final.obj[k , sample.index[cri]] , ]$Effectifs.obj){listou.df[listou.df[, sample.index[cri]]==lista[[cri]]$varid , "SAMPLE"]<-TRUE}
  #       }
  #       if(cri>1&cri!=nb.criteres){
  #         if(lista[[cri-1]][lista[[cri-1]]$varid==final.obj[k , sample.index[cri-1]] , ]$Effectifs>=lista[[cri-1]][lista[[cri-1]]$varid==final.obj[k , sample.index[cri-1]] , ]$Effectifs.obj&
  #            lista[[cri]][lista[[cri]]$varid==final.obj[k , sample.index[cri]] , ]$Effectifs<lista[[cri]][lista[[cri]]$varid==final.obj[k , sample.index[cri]] , ]$Effectifs.obj){
  #           listou.df[
  #             listou.df[, sample.index[1]]==lista[[1]]$varid&
  #               listou.df[, sample.index[cri-1]]==lista[[cri-1]]$varid&
  #               listou.df[, sample.index[cri]]==lista[[cri]]$varid, "SAMPLE"]<-"test"
  #         }
  #       }
  #     }
  #   }
  #       }{
  #          }
  #       }
  #   }
  # }
  #
  #     if(final.obj$freq[k]<=final.obj$obj[k]){
  #
  #     }else{
  #       if(final.obj$freq[k]>final.obj$obj[k]){
  #         savec<-rep(FALSE, times=nrow(listou[[k]]))
  #         sample(x = 1:nrow(listou[[k]]), size = final.obj$obj[k], replace = FALSE, prob = listou[[k]][, var.poid.supp])->samplec
  #         savec[samplec]<-TRUE
  #         listou[[k]]$SAMPLE<-savec
  #       }
  #     }
  #   }
  #   final.obj$SAMPLE<-unlist(lapply(listou, FUN = function(k){
  #     #length(
  #     k$SAMPLE->temp
  #     length(temp[temp==TRUE])#[isTRUE(k$SAMPLE)]
  #     #)
  #   })
  #   )
  # }
  # lapply(seq_along(listou), function(k){
  #   if(final.obj$freq[k]<=final.obj$obj[k]){
  #     listou[[k]]$SAMPLE<-TRUE
  #   }else{
  #     if(final.obj$freq[k]>final.obj$obj[k]){
  #       savec<-rep(FALSE, times=nrow(listou[[k]]))
  #       sample(x = 1:nrow(listou[[k]]), size = final.obj$obj[k], replace = FALSE)->samplec
  #       savec[samplec]<-TRUE
  #       listou[[k]]$SAMPLE<-savec
  #     }
  #   }
  # }

  data.frame(table(listou.df$SAMPLE, exclude=NULL), round(prop.table(table(listou.df$SAMPLE, exclude=NULL))*100, 1))[, c(1, 2, 4)]->sample.size
  names(sample.size)<-c("SAMPLE", "Freq", "Prop")
  #
  if(is.null(idvar.sample)){
    sample.id<-row.names(listou.df[listou.df$SAMPLE==TRUE ,])
  }
  if(!is.null(idvar.sample)){
    sample.id<-listou.df[isTRUE(listou.df$SAMPLE) , idvar.sample]
  }
  # res ####
  res<-list("final.obj"=final.obj, "ensemble"=Ensemble, "crit"=lista, "sample.size"=sample.size, "sample.id"=sample.id, "newdata"=listou.df, "newdata.list"=listou)
  return(res)
}
