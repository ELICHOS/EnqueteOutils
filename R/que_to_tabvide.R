#' @author Elie
#' @description que.to.tabvide utilise le résultat de excel_to_R() pour créer une table vide reprenant la structure de 'lenquête.
#' @param SPHINX_ou_WORD "WORD"
#' @param data =df$df.QR
#' @title La fonction "que.to.tabvide"
#' @export
que.to.tabvide<-function(SPHINX_ou_WORD="WORD", data=df$df.QR, ...){
  data->data.que
  if(SPHINX_ou_WORD=="SPHINX"){
data.que[, c("Libellé", "Variable", "rowrank")]->data.stock
data.stock$rowrank<-order(data.que$rowrank)
data.frame(t(data.stock))->data.stock
data.stock$ID2<-c(" ", " ", " ")
data.stock$ID1<-c(" ", " ", " ")
data.stock[, c(ncol(data.stock), ncol(data.stock)-1, 1:(ncol(data.stock)-2)) ]->data.stock
  }
  if(SPHINX_ou_WORD=="WORD"){
    #
    data.stock$rowrank<-order(data.stock$row_rank1)
    data.stock<-subset(data.stock, grepl("QUESTION", data.stock$style_id)==TRUE)
    data.frame(t(data.stock))->data.stock
    data.stock$ID2<-c(" ", " ", " ", " ", " ")
    data.stock$ID1<-c(" ", " ", " ", " ", " ")
    data.stock[, c(ncol(data.stock), ncol(data.stock)-1, 1:(ncol(data.stock)-2)) ]->data.stock
  }
return(data.stock)
}
