  RBMAT<-function(original.df, var1, var2, prop=FALSE, prop.margin=1){
    ####
    if(is.null(var2)){
      if(length(var1)>1){
        message("INFO: var1 est multiple")
        message(paste("var1 : ", paste(var1, collapse = "/")))
        message("var2 is null")
        data.multi<-original.df[, var1]
        var.multi<-var1
        #
        data.frame(apply(data.multi, MARGIN = 2, FUN = function(x) {as.character(x)}),
                   stringsAsFactors = FALSE)->data.multi
        unique(unlist(data.multi))->levels.multi
        as.character(levels.multi)[order(as.character(levels.multi))]->levels.multi

        lapply(X = 1:length(levels.multi), FUN = function(li){
          unlist(lapply(1:nrow(data.multi), FUN = function(rowi){
            if(TRUE%in%grepl(pattern = levels.multi[li], x = unlist(data.multi[rowi , ]), fixed = TRUE)){
              "OUI"
            } else {"NON"}
          }))
        })->lisOUINON
        names(lisOUINON)<-as.character(levels.multi)
        do.call("cbind", lisOUINON)->matriOUINON1
        matriOUINON1[ , colnames(matriOUINON1)!=""]->matriOUINON1
        lapply(as.data.frame(matriOUINON1), function(x){
          table(x)->tabx
          if(prop==TRUE){
            tabx<-round(prop.table(tabx)*100, 1)
          }
          #print(sum(tabx))
          as.data.frame(tabx)
          })->tabtab
        lapply(1:length(tabtab), FUN = function(i){
          tabtab[[i]]->tabtabi
          tabtabi$x<-as.character(tabtabi$x)
         tabtabi[tabtabi$x=="OUI" , "x"]<-names(tabtab)[i]
         tabtabi[tabtabi$x==names(tabtab)[i] , ]
        })->tabtab2
        do.call("rbind", tabtab2)->tabtab3
        tabtab3->tab
        tab$Freq->vec
        names(vec)<-tab$x
        as.table(vec)->rbmat.null2
      } else {
        message("INFO: var1 est unique")
        message(paste("var1 : ", paste(var1, collapse = "/")))
        message("var2 is null")
      table(original.df[ , var1])->rbmat.null2
      if(prop==TRUE){
        rbmat.null2<-round(prop.table(rbmat.null2)*100, 1)
      }
      }
      return(rbmat.null2)
    } else {

    if(length(var1)>1&length(var2)<=1 ){
      message("INFO: var1 est multiple")
      message(paste("var1 : ", paste(var1, collapse = "/")))
      message("INFO : var2 est unique")
      message(paste("var2 : ", paste(var2, collapse = "/")))

      data.multi<-original.df[, var1]
      var.multi<-var1
      data.unique<-original.df[, var2]
      var.unique<-var2
      ####
      data.frame(apply(data.multi, MARGIN = 2, FUN = function(x) {as.character(x)}))->data.multi
      unique(unlist(data.multi))->levels.multi
      as.character(levels.multi)[order(as.character(levels.multi))]->levels.multi

      lapply(X = 1:length(levels.multi), FUN = function(li){
        unlist(lapply(1:nrow(data.multi), FUN = function(rowi){
          if(TRUE%in%grepl(pattern = levels.multi[li], x = unlist(data.multi[rowi , ]), fixed = TRUE)){
            "OUI"
          } else {"NON"}
        }))
      })->lisOUINON
      names(lisOUINON)<-as.character(levels.multi)
      do.call("cbind", lisOUINON)->matriOUINON1
      matriOUINON1[ , colnames(matriOUINON1)!=""]->matriOUINON1

      ####
      lapply(1:ncol(matriOUINON1) , function(xi1){
        table(matriOUINON1[ , xi1] , original.df[ , var.unique])
      })->listab
      names(listab)<-colnames(matriOUINON1)
      lapply(1:length(listab), FUN = function(xi2){
        rownames(listab[[xi2]])[rownames(listab[[xi2]])=="OUI"]<-names(listab)[xi2]
        #
        as.data.frame.matrix(listab[[xi2]])#[rownames(listab[[xi2]])!="NON" ,  ])
      })->listab.named
      #library(plyr)
      do.call("rbind", listab.named)->df.named
      subset(df.named, row.names(df.named)%in%levels.multi)->df.named
      names(df.named)[names(df.named)=="V3"]<-""
      as.table(as.matrix(df.named))->rbmat
      ####
    } else {
      if(length(var2)>1&length(var1)<=1 ){ #class(data2)=="data.frame"#&ncol(data2)>1
        message("INFO: var2 est multiple")
        message(paste("var2 : ", paste(var2, collapse = "/")))
        message("INFO : var1 est unique")
        message(paste("var1 : ", paste(var1, collapse = "/")))
        ####
        data.multi<-original.df[, var2]
        var.multi<-var2
        data.unique<-original.df[, var1]
        var.unique<-var1
        ####
        data.frame(apply(data.multi, MARGIN = 2, FUN = function(x) {as.character(x)}))->data.multi
        unique(unlist(data.multi))->levels.multi
        as.character(levels.multi)[order(as.character(levels.multi))]->levels.multi

        lapply(X = 1:length(levels.multi), FUN = function(li){
          unlist(lapply(1:nrow(data.multi), FUN = function(rowi){
            if(TRUE%in%grepl(pattern = levels.multi[li], x = unlist(data.multi[rowi , ]), fixed = TRUE)){
              "OUI"
            } else {"NON"}
          }))
        })->lisOUINON
        names(lisOUINON)<-as.character(levels.multi)
        do.call("cbind", lisOUINON)->matriOUINON1
        matriOUINON1[ , colnames(matriOUINON1)!=""]->matriOUINON1

        ####
        lapply(1:ncol(matriOUINON1) , function(xi1){
          table(matriOUINON1[ , xi1] , original.df[ , var.unique])
        })->listab
        names(listab)<-colnames(matriOUINON1)
        lapply(1:length(listab), FUN = function(xi2){
          rownames(listab[[xi2]])[rownames(listab[[xi2]])=="OUI"]<-names(listab)[xi2]
          #
          as.data.frame.matrix(listab[[xi2]])#[rownames(listab[[xi2]])!="NON" ,  ])
        })->listab.named
        #library(plyr)
        do.call("rbind", listab.named)->df.named
        subset(df.named, row.names(df.named)%in%levels.multi)->df.named
        names(df.named)[names(df.named)=="V3"]<-""
        as.table(as.matrix(df.named))->rbmat
        # if(var.multi==var2){
        #   t(rbmat)->rbmat
        # }
        ####
      } else {
        if(length(var2)>1&length(var1)>1){
          message("INFO: var2 est multiple")
          message(paste("var2 : ", paste(var2, collapse = "/")))
          message("INFO : var2 est unique")
          message(paste("var2 : ", paste(var2, collapse = "/")))
          ####
          data.multi<-original.df[, var1]
          var.multi<-var1
          data.multi2<-original.df[, var2]
          var.multi2<-var2
          ####
          data.frame(apply(data.multi, MARGIN = 2, FUN = function(x) {as.character(x)}))->data.multi
          unique(unlist(data.multi))->levels.multi
          as.character(levels.multi)[order(as.character(levels.multi))]->levels.multi

          lapply(X = 1:length(levels.multi), FUN = function(li){
            unlist(lapply(1:nrow(data.multi), FUN = function(rowi){
              if(TRUE%in%grepl(pattern = levels.multi[li], x = unlist(data.multi[rowi , ]), fixed = TRUE)){
                "OUI"
              } else {"NON"}
            }))
          })->lisOUINON
          names(lisOUINON)<-as.character(levels.multi)
          do.call("cbind", lisOUINON)->matriOUINON1
          matriOUINON1[ , colnames(matriOUINON1)!=""]->matriOUINON1

          ####
          data.frame(apply(data.multi2, MARGIN = 2, FUN = function(x) {as.character(x)}))->data.multi2
          unique(unlist(data.multi2))->levels.multi2
          as.character(levels.multi2)[order(as.character(levels.multi2))]->levels.multi2

          lapply(X = 1:length(levels.multi2), FUN = function(li){
            unlist(lapply(1:nrow(data.multi2), FUN = function(rowi){
              if(TRUE%in%grepl(pattern = levels.multi2[li], x = unlist(data.multi2[rowi , ]), fixed = TRUE)){
                "OUI"
              } else {"NON"}
            }))
          })->lisOUINON2
          names(lisOUINON2)<-as.character(levels.multi2)
          do.call("cbind", lisOUINON2)->matriOUINON2
          matriOUINON2[ , colnames(matriOUINON2)!=""]->matriOUINON2

          ####
          lapply(1:ncol(matriOUINON1),FUN = function(xi1){
            lapply(1:ncol(matriOUINON2),FUN = function(xi2){
              table(matriOUINON1[ , xi1], matriOUINON2[ , xi2])->tab1.x12
              as.matrix(tab1.x12)->matx
              rownames(matx)[rownames(matx)=="OUI"]<-colnames(matriOUINON1)[xi1]
              colnames(matx)[colnames(matx)=="OUI"]<-colnames(matriOUINON2)[xi2]
              matx
              #matx[rownames(matx)!="NON"&rownames(matx)!=""&!is.na(rownames(matx)) , ]->matx
            })->listab2
            names(listab2)<-colnames(matriOUINON2)
            listab2
          }) -> listab
          names(listab)<-colnames(matriOUINON1)
          level.for.row<-as.character(levels.multi)[as.character(levels.multi)!=""&!is.na(as.character(levels.multi))]
          level.for.col<-as.character(levels.multi2)[as.character(levels.multi2)!=""&!is.na(as.character(levels.multi2))]
          matrix(nrow = length(level.for.row),ncol = length(as.character(level.for.col)) )->abc
          data.frame(abc)->abc
          row.names(abc)<-level.for.row
          names(abc)<-level.for.col
          for(xj1 in 1:length(listab)){
            for(xj2 in 1:length(listab[[xj1]])){
              data.frame(listab[[xj1]][[xj2]])->temp.df
              temp.df[temp.df$Var1%in%row.names(abc)&temp.df$Var2%in%names(abc) , ]->subs.df
              abc[row.names(abc)==subs.df$Var1 , names(abc)==subs.df$Var2 ]<-subs.df$Freq
            }
          }
          as.table(as.matrix(abc))->rbmat
          ####
        } else {
          if(length(var2)<=1&length(var1)<=1){
            message("INFO: var2 est unique")
            message(paste("var2 : ", paste(var2, collapse = "/")))
            message("INFO : var1 est unique")
            message(paste("var1 : ", paste(var1, collapse = "/")))
            ####
            table(original.df[ , var1], original.df[ , var2])->rbmat
          }}}}
    if(prop==TRUE){
      rbmat<-round(prop.table(rbmat, margin = prop.margin)*100, 1)
      if(prop.margin==1){
        cbind(rbmat, rowSums(rbmat))->rbmat
        colnames(rbmat)[dim(rbmat)[2]]<-"TOTAL"
      } else {
        if(prop.margin==2){
          rbind(rbmat, colSums(rbmat))->rbmat
          rownames(rbmat)[dim(rbmat)[1]]<-"TOTAL"
        }
      }
      #addmargins.TOTAL(tab12, margin = c(2, 1)[prop.margin])->tab12
    } else {
      cbind(rbmat, rowSums(rbmat))->rbmat
      colnames(rbmat)[dim(rbmat)[2]]<-"TOTAL"
      rbind(rbmat, colSums(rbmat))->rbmat
      rownames(rbmat)[dim(rbmat)[1]]<-"TOTAL"
      #addmargins.TOTAL(tab12)->tab12
    }
    #as.data.frame.matrix(tab12)->rbmat
    colnames(rbmat)[colnames(rbmat)==""]<-"Sans réponse"
    colnames(rbmat)[colnames(rbmat)=="V3"]<-"Sans réponse"
    colnames(rbmat)[colnames(rbmat)=="V1"]<-"Sans réponse"
    rbmat->rbmat2
    return(rbmat2)
    }
  }
