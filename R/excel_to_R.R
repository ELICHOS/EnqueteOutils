#' @author Elie
#' @description excel_to_R utilise le package officer pour créer un df un peu lisible à partir d'une docx d'enquête
#' @param ... le chemin du fichier, et autres arguments de la fonction officer::read_docx(...)
#' @param autre.separate si FALSE, cela veut dire que dans le docx les "Autres précisez: ..." sont présentés comme une modalité de réponse parmi les autres
#' @title La fonction "excel_to_R"
#' @export
excel_to_R<-function(autre.separate=FALSE , ... ){
#### IMPORT DU DOCX et des QR####
library(officer)
#FNDSA32<-officer::read_docx(path = "C:/Users/elie/Documents/Modèles Office personnalisés/Enquête auprès des passagers du CGR.docx")
#PATH<-"//192.168.1.5/Documents utilisateurs/Documents de Elie/FNDSA/Qui_est-ce/FNDSA_structure_questionnaire.docx"

FNDSA32<-officer::read_docx(...)
docx_summary(FNDSA32)->FNDSA32_sum
source(file="C:/Users/elie/Desktop/EnqueteOutils/R/QR_predefinies.R", encoding = "UTF-8")
#### CREATE THE DF ####
data.frame("style"=FNDSA32_sum$style_name, "content"=FNDSA32_sum$text, "cell_id"=FNDSA32_sum$cell_id, "col_span"=FNDSA32_sum$col_span)->df
df$style_id<-gsub(pattern = " ", replacement = "", x = df$style)
df$row_rank1<-row.names(df)
df$VAR<-paste(df$style_id, df$row_rank1, sep="_")
df$row_rank1<-as.numeric(as.character(df$row_rank1))
#df[order(df$row_rank) , ]->df
#### VAR QUESTIONS APPLI TO REPONSES ####
df$VARIABLE<-unlist(sapply(X = 1:nrow(df), FUN = function(i){ #s'applique à chaque lige de df
  subset(df, grepl(pattern = "QUESTION", x = df$style)==TRUE)->test1 #je sélectionne les lignes de type QUESTION
  if(grepl(pattern = "QUESTION", x = df$style[i])|grepl(pattern = "heading", x = df$style[i])){ # Si la lige i est de type QUESTION ou HEADER, VARIABLE = VAR
    df[i, "VAR"]
  }else{ # SINON (ni QUESTION ni header)
    subset(test1, test1$row_rank1<i)->tempi #Je sélectionne les QUESTIONS avant la ligne i (rank < i)
    if(nrow(tempi)<1){ #s'il n'y pas de question avant i, VARIABLE = VAR
      df[i, "VAR"]
    } else {
    df[tempi[nrow(tempi) , "row_rank1"], "VAR"] # SINON VARIABLE = VAR de la dernière question
    }

  }
}
)
)
### FORMATTAGE DES REPONSES (EN LIGNES PAR QUESTION) ####
s <- strsplit(as.character(df$content), split = "\u2610")#U+2610 	CASE DE BULLETIN / BALLOT BOX
data.frame(V1 = rep(df$style, sapply(s, length)), V2 = unlist(s))->df2
data.frame(lapply(X = df, FUN = function(col){
  rep(col, sapply(s, length))
  }), V2 = unlist(s))->df2
df2$V2<-trimws(df2$V2, which = c("both"))
df2<-subset(df2, df2$V2!="")
levels(df2$V2)<-c(levels(df2$V2), "Autre")
df2$V2[grepl("Autre, précisez", df2$V2, ignore.case = TRUE)]<-"Autre, précisez"
#### TIMEVAR ####
df2$row_rank2<-1:nrow(df2)
df2$TIMEVAR<-unlist(sapply(X = 1:nrow(df2), FUN = function(i){
  subset(df2, grepl(pattern = "QUESTION", x = df2$style)==TRUE)->test1
  if(grepl(pattern = "QUESTION", x = df2$style[i])){
    #as.character(df2[i, "style_id"])
    "QUESTION"
  }else{
    if(grepl(pattern = "CONSIGNE", x = df2$style[i])){
      "CONSIGNE"
  }else {if(grepl(pattern = "heading", x = df2$style[i])){
    "TITRE"
  } else {
    subset(test1, test1$row_rank2<df2[i, "row_rank2" ])->tempi
    if(nrow(tempi)<1){
      as.character(df2[i, "style_id"])
    } else {
      as.numeric(as.character(tempi[nrow(tempi) , "row_rank2"]))->prev
      paste(df2[i, "style_id"], ((df2[i, "row_rank2" ]-prev)+0), sep=".")
    }

  }
  }
  }
}
)
)
#####
if(autre.separate==FALSE){
subset(df2, grepl("Autre, précisez", df2$V2, ignore.case = TRUE))->autre.precis.df
autre.precis.df$style<-"QUESTION TEXTE"
autre.precis.df$content<-"Autre, précisez:"
autre.precis.df$V2<-"Autre, précisez:"
autre.precis.df$row_rank2<-autre.precis.df$row_rank2+0.5
autre.precis.df$row_rank1<-autre.precis.df$row_rank1+0.5
autre.precis.df$VAR<-paste(autre.precis.df$VAR, "autre", sep="_")
autre.precis.df$VARIABLE<-paste(autre.precis.df$VARIABLE, "autre", sep="_")
autre.precis.df$style_id<-"QUESTIONTEXTE"
autre.precis.df$TIMEVAR<-"QUESTION"
rbind(data.frame(lapply(df2, FUN = function(x){if(is.factor(x)){as.character(x)}else{x}})), autre.precis.df)->df2
df2[order(df2$row_rank2) , ]->df2
}
#### RESHAPE ####
df3<-subset(df2, (grepl(pattern = "QUESTION", x = df2$style_id)|df2$style_id=="REPONSES")&(df2$cell_id==1|is.na(df2$cell_id)))
################


list.QR<-lapply(X = unique(df3$VARIABLE), FUN = function(v){
  subset(df3, df3$VARIABLE==v)->sub.v
  c(as.character(v),#unique(sub.v[, "VARIABLE"])),
                 as.character(sub.v[, c("V2")]))->subsub.v
  if(length(subsub.v)<=2&grepl("TEXTE", v)==FALSE){
    for( i in names(QR_predefinies)){
      if(grepl(pattern = i, x = v)==TRUE){
      REP<-QR_predefinies[[i]]
      subsub.v<-c(subsub.v, REP)
      }
    }
  }
  subsub.v
  })

n.obs <- sapply(list.QR, length)
seq.max <- seq_len(max(n.obs))
mat <- data.frame(t(sapply(list.QR, "[", i = seq.max)))
####
rep.nb.nom<-paste("REPONSE", 1:(ncol(mat)-2), sep = ".")
names(mat)<-c("VARIABLE", "QUESTION", rep.nb.nom)
dim(mat)[1]->NB.QUESTIONS1
#
merge(mat, df2, by="VARIABLE")->mat2
subset(mat2, (grepl(pattern = "QUESTION", x = mat2$style_id)==TRUE))->mat2
dim(mat2)[1]->NB.QUESTIONS2
if(NB.QUESTIONS1!=NB.QUESTIONS2){
  message("ERROR de NB.QUESTION")
  NB.QUESTIONS<-"ERROR"
  } else {
  NB.QUESTIONS<-NB.QUESTIONS1
}
#
subset(df, grepl(x = df$style_id, pattern = "heading")==TRUE&df$content!="")->df.headers
#
mat2<-mat2[, c("VARIABLE", "style_id", "row_rank1", "QUESTION", rep.nb.nom)]
mat2$Type<-sapply(1:nrow(mat2), FUN = function(i){
  if(grepl("ECHELLE", mat2$style_id[i])|grepl("FERMEE", mat2$style_id[i])){
    "choix.unique"
  } else {
    if(grepl("MULTIPLE", mat2$style_id[i])){
      "choix.multiple"
    } else  {
      if(grepl("TEXTE", mat2$style_id[i])){
      "texte"
      }
    }
  }
})
df.headers[, c("style_id","row_rank1", "content" )]->df.headers
df.headers$Type<-sapply(1:nrow(df.headers), FUN = function(i){
  if(grepl("heading1", df.headers$style_id[i])){
    "Titre 1"
  } else if(grepl("heading", df.headers$style_id[i])){
    "Sous-titre"
  }
}
)
df.headers$VARIABLE<-paste(gsub(pattern = " ", replacement = "", x = df.headers$Type), df.headers$row_rank1, sep="_")
names(df.headers)<-c("style_id", "row_rank1", "QUESTION", "Type", "VARIABLE")
data.frame(matrix(ncol = length(rep.nb.nom), nrow = nrow(df.headers), data = NA))->reptemp
row.names(reptemp)<-row.names(df.headers);names(reptemp)<-rep.nb.nom
cbind(df.headers, reptemp)->df.headers
c(0:9, "-", "right")->avoid.char
paste(avoid.char, collapse="|")->expr.avoir.char
df.headers$QUESTION<-gsub(pattern = expr.avoir.char, replacement = "", x = df.headers$QUESTION)
rbind(mat2,
      df.headers[, names(mat2)])->mat3
mat3<-mat3[order(mat3$row_rank1) , ]
####
# lapply(X = 1:nrow(mat3), FUN = function(i){
#   if()
# })
####
# if(autre.separate==FALSE){
#   mat3$AUTRE.LOG<-apply(X = mat3[, grepl(pattern = "REPONSE", x = names(mat3))],
#                             MARGIN=1,
#                             FUN = function(x){"Autre, précisez:"%in%x})
#   subset(mat3, mat3$AUTRE.LOG==TRUE)->data.que.true
#   data.que.true$VARIABLE<-paste(data.que.true$VARIABLE, "autre", sep="_")
#   data.que.true$style_id<-"QUESTIONTEXTE"
#   data.que.true$row_rank1<-data.que.true$row_rank1+0.5
#   data.que.true$QUESTION<-paste(data.que.true$QUESTION, "autre", sep="_")
#   data.que.true$Type<-"autre.precisez"
#   data.que.true[, grepl(pattern = "REPONSE", x = names(data.que.true))]<-NA

  ####
mat2[ , grepl(pattern = "REPONSE", x = names(mat2))]->dfrep
dfrep$na_count <- apply(dfrep, 1, function(x) sum(is.na(x)))

list.vars<-lapply(1:nrow(mat2), function(i){
  if(mat2$Type[i]!="choix.multiple"){
    as.character(mat2$VARIABLE[i])
  } else {
    if(mat2$Type[i]=="choix.multiple"){
      c(as.character(mat2$VARIABLE[i]), paste(mat2$VARIABLE[i], seq(1, (dim(dfrep)[2]-1), 1), sep=""))
  }
}
  })
names(list.vars)<-mat2$VARIABLE[1:nrow(mat2)]
list("NB.QUESTIONS"=NB.QUESTIONS, "df.GLOBAL"=mat3, "df.QR"=mat2, "df.headers"=df.headers, "list.vars"=list.vars, "dfrep"=dfrep)->res
return(res)
}
