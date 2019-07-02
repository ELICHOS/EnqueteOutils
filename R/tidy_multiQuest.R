tidy_multiQuest<-function(df=tosave$DATA , var.pattern="info_source_I4", autre.pattern="AUTRE", REPLACE=FALSE){
  if(length(var.pattern)>1){
    var.pattern<-as.character(var.pattern)
    var.pattern<-var.pattern[nchar(var.pattern)==min(nchar(var.pattern))]
  }

  df[ , grepl(pattern = var.pattern, x = names(df), fixed = TRUE)&grepl(pattern = autre.pattern, x = names(df), fixed = TRUE)==FALSE]->df.var
  #} else {
  #  if(length(var.pattern)>1){
  #    df[ , var.pattern]->df.var
  #    df.var[ , grepl(pattern = autre.pattern, x = names(df.var), fixed = TRUE)==FALSE]->df.var
  #  }}
  ####
  names(df.var)->originals.names
  df.var<-data.frame(apply(X = df.var, MARGIN = 2, FUN = as.character), stringsAsFactors = FALSE)
  unique(unlist(df.var))->levels.var
  levels.var[is.na(levels.var)]<-"MISSING"
  levels.var[levels.var==""]<-"MISSING"
  levels.var<-unique(levels.var)

  lapply(1:length(levels.var), FUN = function(vari){
    apply(X = df.var, MARGIN = 1, FUN = function(indi){
      levels.var[vari]%in%indi
    })
  })->list.vari.indi

  gsub(pattern = " ", replacement = "_", x = levels.var, fixed=TRUE)->name.var
  gsub(pattern = ",", replacement = "", x = name.var, fixed=TRUE)->name.var
  gsub(pattern = "(", replacement = "", x = name.var, fixed=TRUE)->name.var
  gsub(pattern = ")", replacement = "", x = name.var, fixed=TRUE)->name.var
  gsub(pattern = ".", replacement = " ", x = name.var, fixed=TRUE)->name.var

  trimws(name.var, which = "both")->name.var

  paste(var.pattern, name.var, sep="_")->name.var

message(paste("var.pattern : ", var.pattern))
message("________")
message(paste("autre.pattern", autre.pattern))
message("________")
message(paste("name.var", name.var))
message( paste("list.vari.indi : ", length(list.vari.indi) ))
message(paste("length(name.var)", length(name.var)))
message(paste("length(list.vari.indi)", length(list.vari.indi)))

names(list.vari.indi)<-name.var

  data.frame(list.vari.indi)->the.df
  names(the.df)<-name.var

  if(REPLACE==TRUE){
    df[ , !names(df)%in%originals.names]->df
    cbind(df, the.df)->df
    #ames( df[ , grepl(pattern = var.pattern, x = names(df), fixed = TRUE)&grepl(pattern = autre.pattern, x = names(df), fixed = TRUE)==FALSE])<-names(the.df)
    return(df)
  } else {return(the.df)}
}
