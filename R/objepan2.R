#' @author Elie
#' @description "objepan2" permet de sampler un data.frame en recourant a des probabilites (statistiques) externes.
#' @param Objectif.total    k]] # taille du sample
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
#' objepan<-function(Objectif.total = k]], data.frame.sample=datas.pannb, nb.criteres=2,
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
objepan2<-function(Objectif.total = 400, data.frame.sample=datas.pannb, nb.criteres=2,
                  datas.crit=list(popudep.AURA, sample.type), type.crit=c("eff", "prop"),
                  sample.index=c("Departement", "Instit.Assoc"),
                  crit.index=c("Nom.du.département", "type"),
                  exclude.sample=c("0", "0"),
                  varcontrol ,
                  #
                  nb.criteres2=2,
                  sample.index2=c("rur", "anclog"),
                  exclude.sample2=c("0", "0"),
                  #
                  idvar.sample=NULL
                  ){
  #source(file="C:/Users/Flora2/Desktop/basiques/R/rpss.R", echo=FALSE)
  #library(basiques)
  # Premiere partie ####
  if(length(datas.crit)!=nb.criteres|length(sample.index)!=nb.criteres|length(exclude.sample)!=nb.criteres){message("Error length")}else{"Length ok"}
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
  if(is.data.frame(df)){
  apply(df, MARGIN = 2, FUN = as.character)->df
  } else {df<- data.frame(as.character(df))
  names(df)<-sample.index}
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
  if(dim(dfo)[2]>2){
  dfo$rmtrue<-apply(X = dfo[ , names(dfo)!="freq"], MARGIN = 1, FUN = function(k) any(k %in% c(exclude.sample)))
  } else {
    dfo$rmtrue<-sapply(X = dfo[ , names(dfo)!="freq"], FUN = function(k) any(k %in% c(exclude.sample)))
  }
  dfo<-subset(dfo, subset = dfo$rmtrue!=TRUE)
  if(length(sample.index)==1){
    dfo$obj.vars<-dfo[ , names(dfo)%w/o%c("freq", "rmtrue")]
  } else {
  dfo$obj.vars<-apply(dfo[ , names(dfo)%w/o%c("freq", "rmtrue")], MARGIN = 1, paste, collapse=".")
  }
  #
  obj$obj.vars<-gsub(obj$obj.vars, pattern = " ", replacement = "")
  dfo$obj.vars<-gsub(dfo$obj.vars, pattern = " ", replacement = "")

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
  objepantemp<-function(Objectif.total = Objectif.total, data.frame.sample=data.frame.sample, nb.criteres=nb.criteres,
                     datas.crit=list(sample.type), type.crit=c("prop"),
                     sample.index=c("Instit.Assoc"),
                     crit.index=c( "type"),
                     exclude.sample=c("0")
  ){
    #source(file="C:/Users/Flora2/Desktop/basiques/R/rpss.R", echo=FALSE)
    #library(basiques)
    # Premiere partie ####
    if(length(datas.crit)!=nb.criteres|length(sample.index)!=nb.criteres|length(exclude.sample)!=nb.criteres){message("Error length")}else{"Length ok"}
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
    #
    if(is.data.frame(df)){
      apply(df, MARGIN = 2, FUN = as.character)->df
    } else {df<- data.frame(as.character(df))
    names(df)<-sample.index}
    #
    #apply(df, MARGIN = 2, FUN = as.character)->df
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
    #
    if(dim(dfo)[2]>2){
      dfo$rmtrue<-apply(X = dfo[ , names(dfo)!="freq"], MARGIN = 1, FUN = function(k) any(k %in% c(exclude.sample)))
    } else {
      dfo$rmtrue<-sapply(X = dfo[ , names(dfo)!="freq"], FUN = function(k) any(k %in% c(exclude.sample)))
    }
    #
    #dfo$rmtrue<-apply(X = dfo[ , names(dfo)!="freq"], MARGIN = 1, FUN = function(k) any(k %in% c(exclude.sample)))
    dfo<-subset(dfo, subset = dfo$rmtrue!=TRUE)
    #
    if(length(sample.index)==1){
      dfo$obj.vars<-dfo[ , names(dfo)%w/o%c("freq", "rmtrue")]
    } else {
      dfo$obj.vars<-apply(dfo[ , names(dfo)%w/o%c("freq", "rmtrue")], MARGIN = 1, paste, collapse=".")
    }
    #
    #dfo$obj.vars<-apply(dfo[ , names(dfo)%w/o%c("freq", "rmtrue")], MARGIN = 1, paste, collapse=".")
    dfo$obj.vars<-gsub(" ", "", dfo$obj.vars, fixed = TRUE)
    #
    merge(obj, dfo, by="obj.vars")->final.obj
    final.obj$obj<-round(final.obj$prop*Objectif.total, 0)
    final.obj[ c(sample.index, "prop", "freq", "obj")]->final.obj
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
    length(unique(unlist(lapply(listou, function(x){dim(x)[2]}))))==1->lenti
    message(paste("Length de listou 1 : ", lenti))
    return(list("final.obj.temp"=final.obj, "listou.temp"=listou ))
  }
  length(unique(unlist(lapply(listou, function(x){dim(x)[2]}))))==1->lenti
  message(paste("Length de listou 2 : ", lenti))
  message("jusqu'ici, tout va bien")
  ######### apply objepantemp ##################
  listec<-list()
  if(nb.criteres2==0){
    #### DEBUT LISTEC 2 ####
    for(k in seq_along(listou) ){
     listou[[k]]->temporaro # Je sélectionne la data.frame de réponses caractérisé par la conjonction des critères de niveau 1 (dep et assoc.instit)
      #
      interaction(temporaro[ , varcontrol])->temporaro$stratum #Je cré une variable facteur basée sur la conjonction des variables varcontrol
      props <- data.frame(table(temporaro$stratum)/length(temporaro$stratum)) #Je calcule les proportions de cette nouvelle variable facteur
      merge(temporaro, props, by.x="stratum", by.y="Var1")->temporaro #je merge temporaro et ces props: chaque ligne (individu) est associé à une prop
      #
    savec<-rep(FALSE, times=nrow(temporaro)) # j cré un vecteur de la taille de temporari avec FALSE
    message(paste("balise1", k))
    #listou[[k]]$vecy<-listou[[k]][ , controls.vars]

        if(nrow(temporaro)<final.obj$obj[[k]]){ # si la taille de temporaro est inférieur à l'objectif = ON MANQUE DE MONDE
      samplec1<-1:nrow(temporaro) #Toutes les lignes font partie de samplec1
       samplec2<-sample(x = 1:nrow(temporaro), size = (final.obj$obj[[k]]-length(samplec1)), replace = TRUE, prob=temporaro$Freq
          ) # Je sample, dans temporaro, le nombre d'individus manquant pour atteindre obj, à partir des probs précédemment calculées
       sizec1<-temporaro[c(samplec1) , ] # samplec1 => tous les individus de temporaro sont sélectionnés dans le df sizec1
       sizec1$duplicated<-FALSE # dans le df sizec1, aucun individu-ligne n'est répliqué
       sizec2<-temporaro[c(samplec2) , ] # samplec2 => les individus supplémentaires répliqués sont sélectionnés dans temporaro pour donner le df sizec2
       sizec2$duplicated<-TRUE # dans siec2, forcément, tous les individus sont répliqués
       sizec<-rbind(sizec1, sizec2) #le df final, c'est sizec
       samplec<-samplec1 #dans samplec, il n'y a que les individus, sans les répliques
    } else {
      if(nrow(listou[[k]])>=final.obj$obj[[k]]){  # Si il ya trop ou juste assez de monde pas de sample
      sample(x = 1:nrow(temporaro), size = final.obj$obj[[k]], replace = FALSE, prob=temporaro$Freq
      )->samplec
      sizec<-temporaro[c(samplec) , ]
      sizec$duplicated<-FALSE
    }
    }
    savec[samplec]<-TRUE
    temporaro$SAMPLE<-savec
    listou[[k]]<-temporaro
    listec[[k]]<-sizec
    }
    listou->listou2
    do.call("rbind", listou2)->listou3.df
    listec->listec2
    do.call("rbind", listec2)->listec3

    #### FIN LISTEC 2 ####
    }else{
  listou2<-list()
  for(k in seq_along(listou) ){
    listcritk<-list()
    for(h in 1:nb.criteres2){
      data.frame(prop.table(table(listou[[k]][ , sample.index2[h]])))->listcritk[[sample.index2[h]]]
      print(listcritk[[sample.index2[h]]])
    }
    message("ok1")
    objek<-objepantemp(Objectif.total = final.obj$obj[k],
                       #dim(listou[[k]])[1],
                       data.frame.sample = listou[[k]], nb.criteres = nb.criteres2, datas.crit = listcritk,
                type.crit = rep("prop", times = nb.criteres2), sample.index = sample.index2, crit.index=rep("Var1", times=nb.criteres2), exclude.sample=exclude.sample2)
    #    if(length(datas.crit)!=nb.criteres|length(sample.index)!=nb.criteres|length(exclude.sample)!=nb.criteres){message("Error length")}else{"Length ok"}

    message(as.character( dim(listou[[k]])[1]))
    message(paste("objepantemp",final.obj[k , 1], final.obj[k, 2]))
    for(temp in seq_along(objek$listou.temp)){
    savec<-rep(FALSE, times=nrow(objek$listou.temp[[temp]]))
    message("balise1")
    #listou[[k]]$vecy<-listou[[k]][ , controls.vars]
    if(nrow(objek$listou.temp[[temp]])<=objek$final.obj.temp$obj[[temp]]){
      samplec<-1:nrow(objek$listou.temp[[temp]])
    } else {if(nrow(objek$listou.temp[[temp]])>objek$final.obj.temp$obj[[temp]]){
      sample(x = 1:nrow(objek$listou.temp[[temp]]), size = objek$final.obj.temp$obj[[temp]], replace = FALSE
      )->samplec
    }
    }
    savec[samplec]<-TRUE
    objek$listou.temp[[temp]]$SAMPLE<-savec
    message(paste("balise", temp))
    }
    do.call("rbind", objek$listou.temp)->listou2[[k]]
  }
  do.call("rbind", listou2)->listou3.df
  }
  ###### res 1#########
  final.obj$SAMPLE<-unlist(lapply(listou2, FUN = function(k){
    #length(
    k$SAMPLE->temp
    length(temp[temp==TRUE])#[isTRUE(k$SAMPLE)]
    #)
  })
  )
  Ensemble<-data.frame(
    "prop"=sum(final.obj$prop), "freq"=sum(final.obj$freq), "obj"=sum(final.obj$obj), "SAMPLE"=sum(final.obj$SAMPLE)
  )
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
  #
  data.frame(table(listou3.df$SAMPLE, exclude=NULL), round(prop.table(table(listou3.df$SAMPLE, exclude=NULL))*100, 1))[, c(1, 2, 4)]->sample.size
  names(sample.size)<-c("SAMPLE", "Freq", "Prop")
  #
  if(is.null(idvar.sample)){
    sample.id<-row.names(listou3.df[listou3.df$SAMPLE==TRUE ,])
  }
  if(!is.null(idvar.sample)){
    sample.id<-listou3.df[isTRUE(listou3.df$SAMPLE) , idvar.sample]
  }
  #
  listec3->dftest
  aggregate(dftest$Freq~dftest$duplicated+dftest[ , grepl(pattern = "Departement", x = names(dftest))], data = dftest, FUN = quantile)->weighted.quant
  aggregate(as.factor(dftest$duplicated)~dftest[ , grepl(pattern = "Departement", x = names(dftest))], data = dftest, FUN = table)->weighted.tab

  ####### res 2 #######
  res<-list("final.obj"=final.obj, "ensemble"=Ensemble, "crit"=lista, "sample.size"=sample.size, "sample.id"=sample.id,
            "newdata"=listou3.df, "newdata.list"=listou2, "listec"=listec, "listec.df"=listec3,
            "weighted.quant"=weighted.quant, "weighted.tab"=weighted.tab)
  #res<-list("final.obj"=final.obj, "listou"=listou, "listou3.df"=listou3.df)
  return(res)
}



#Parametres de test (projet: ENQ CONJ)
# Objectif.total = k]]; data.frame.sample=datas.pannb; nb.criteres=2;
# datas.crit=list(popudep.AURA, sample.type); type.crit=c("eff", "prop");
# sample.index=c("Departement", "Instit.Assoc");
# crit.index=c("Nom.du.département", "type");
# exclude.sample=c("0", "0")
# datas.pannb<-datas
# datas.pannb$var.poid.supp<-poids.sample(data = datas.pannb, varpoids = c("Ancienneté", "Fréquence.contact"), orderpoids = list(c(4, 3, 1, 2, 5), c(1, 3, 4, 2)))$poids
# var.poid.supp="var.poid.supp"
# Objectif.total = k]]; data.frame.sample=datas.pannb2; nb.criteres=2;
# datas.crit=list(popudep.AURA, sample.type); type.crit=c("eff", "prop");
# sample.index=c("Departement", "Instit.Assoc");
# crit.index=c("Nom.du.département", "type");
# exclude.sample=c("0", "0");
# controls=TRUE;
# controls.vars=c("rur", "anclog");
# poid.supp = FALSE; var.poid.supp = "var.poid.supp";
# hierarch.crit=FALSE;
# idvar.sample=NULL
# Objectif.total = 400; data.frame.sample=datas.pannb; nb.criteres=2;
# datas.crit=list(popudep.AURA, sample.type); type.crit=c("eff", "prop");
# sample.index=c("Departement", "Instit.Assoc");
# crit.index=c("Nom.du.département", "type");
# exclude.sample=c("0", "0");
#
# nb.criteres2=2;
# sample.index2=c("rur", "anclog");
# exclude.sample2=c("0", "0")
