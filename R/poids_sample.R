#' @author Elie
#' @description "poids.sample" permet de calculer des poids individuels qui sont le produit de poids relatifs affectés à chaque variable factor().
#' @param data   un data.frame
#' @param varpoids  vecteur des noms des variables à peser (au moins égal à 1). Doit être de type factor().
#' @param orderpoids pour chaque variable à peser, l'ordre de poid des levels. Il s'agit d'un ordre croissant: l'on range les levels par ordre croissant.
#' @title La fonction "poids.sample"
#' @examples
#' data.frame("Ancienneté"=rep(c("A", "B", "C", "D", "E"), 4), "Fréquence.contact"=rep(c("a", "b", "c", "d"), 5))->df
#' poids.sample(data=df,
#' varpoids=c("Ancienneté", "Fréquence.contact"),
#' orderpoids=list(c(4, 3, 1, 2, 5), c(1, 3, 4, 2)))
#' #Signifie que pour "Ancienneté" le levels [4] est le plus faible, le moins pesé, le moins important.
#' @export
poids.sample<-function(data=datas,
                       varpoids=c("Ancienneté", "Fréquence.contact"),
                       orderpoids=list(c(4, 3, 1, 2, 5), c(1, 3, 4, 2))
){

  list1<-lapply(X = 1:length(varpoids), FUN = function(vp){
    data.frame(levels(data[, varpoids[vp]])[orderpoids[[vp]]], (seq(0.1, 0.9, ((0.9-0.1)/(length(levels(data[, varpoids[vp]])
    )-1)
    )
    )
    )
    )->df
    names(df)<-c("var", "poids")
    df
  }
  )
  names(list1)<-varpoids
  #
  lapply(X = 1:length(varpoids), FUN = function(vp){
    data.frame(v0=row.names(data), #v1=data[, names(data)%w/o%varpoids[vp]],
               v4=list1[[varpoids[vp]]][match(data[, varpoids[vp]], list1[[varpoids[vp]]][, "var"]), 2])
  }
  )->tra1
  do.call("cbind", tra1)->tra1
  tra1[, -c(seq(2, (dim(tra1)[2]), by = 2 ) )]->tra1
  #
  lapply(X = 1:length(varpoids), FUN = function(vp){
    if(vp==1){
      data.frame(v0=row.names(data), #v1=data[, names(data)%w/o%varpoids[vp]],
                 v4=list1[[varpoids[vp]]][match(data[, varpoids[vp]], list1[[varpoids[vp]]][, "var"]), 2])
    } else {
      data.frame(#v0=row.names(data), #v1=data[, names(data)%w/o%varpoids[vp]],
        v4=list1[[varpoids[vp]]][match(data[, varpoids[vp]], list1[[varpoids[vp]]][, "var"]), 2])
    }
  }
  )->tra2
  do.call("cbind", tra2)->tra2
  apply(X = tra2[ ,-1], MARGIN = 1, FUN = prod)->indpoid
  #
  #   #merge(data, list1[[varpoids[vp]]], by.x = varpoids[vp], by.y = "var", all.x = TRUE, sort = FALSE)[, "poids"]
  #   #left_join(data, list1[[varpoids[vp]]], )
  # sappl
  #
  # lapply(X = 1:length(varpoids), FUN = function(vp){
  #     lapply(1:dim(list1[[varpoids[vp]]])[1], FUN = function(val){
  #       lapply(1:nrow(data) , FUN = function(i){
  #   if(data[i , varpoids[vp]]==list1[[varpoids[vp]]][val, "var"]){as.numeric(list1[[varpoids[vp]]][val, "poids"])}
  #     })
  #   })
  # })->df0li
  # lapply(X = df0li, FUN = function(iy){
  #   data.frame(iy)
  #   #apply(df1, MARGIN = 1, FUN = sum)
  # })->df0li
  # for(iy in 1:length(df0li)){
  #   df0li[[iy]]->df
  #   df[is.null(df)]
  #   apply(X = df, MARGIN = 1, FUN = sum)
  # }
  # lapply(X = 1:length(df0li), FUN = function(iy){
  #
  #   #apply(X = df0li[[iy]], MARGIN = 1, FUN = sum)
  # })
  #
  #
  #
  #   df0li
  #   ->mat0
  #   df0<-data.frame(mat0)
  #
  #   df1
  # names(list1)<-varpoids
  # outer(list1[[varpoids[i] ]]$var, list1[[varpoids[i] ]]$var, FUN = paste)
  #
  # return(list1)
  res<-list("poids"=indpoid, "verif"=tra1, "table.des.poids"=list1)
  return(res)
}
