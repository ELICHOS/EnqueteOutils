fromWORD<-function(the.df=nomo, id.var="VARIABLE",label.var="QUESTION", type.var="Type",
                               reponses.pattern="REPONSE", autre.pattern="Autre, précisez") {
# DF ; id.var ; inputId.i ; label.var ; type.var ; reponses.pattern
  the.df->DF
  #
lapy<-lapply(X = 1:nrow(DF), FUN = function(i){
  inputId.i<-as.character(DF[i, id.var])
  print(paste("i: ", i, "inputId.i:", inputId.i, "lenght(inputId.i): ", length(inputId.i)))
  label.i<-DF[i, label.var]
  if(DF[i , type.var]=="Titre 1"){
    h2(label.i)
  } else {
    if(grepl("Titre", DF[i , type.var])==TRUE){
      h4(label.i)
    } else {
      if(DF[i, type.var]=="choix.multiple"){
        choices.i<-DF[ ,  grepl(reponses.pattern, names(DF))==TRUE][i , ]
        as.character(unlist(choices.i))->choices.i
        choices.i[!is.na(choices.i)]->choices.i
        ###
        choices.i <- setNames(
          stri_trans_general(choices.i, id = "latin-ascii"),
          choices.i
        )
        print(choices.i)
        widget1<-checkboxGroupInput(inputId=inputId.i,
                                    label=label.i,
                                    choices=choices.i,
                                    selected=choices.i[1], width = "100%")
        inputId.i.autres<-paste(inputId.i, "autre", sep=".")
        label.i.autres<-paste(label.i, "autre", sep=".")
        isolate(inputId.i)->isolaid
        stri_trans_general(autre.pattern, id = "latin-ascii")->autre.pattern.corr
        # paste("input.", isolaid, ".indexOf('", autre.pattern.corr, "')>-1", sep="")->conditexpr
        # widget2<-conditionalPanel(
        #   condition= conditexpr,
        #   textInput(inputId = inputId.i.autres,
        #             label = label.i.autres,
        #             value = "f",
        #             width = "100%")
        # )
        # list(widget1, widget2)
        widget1
      } else {
        if(DF[i, type.var]=="choix.unique"|DF[i, type.var]=="scale"){
          choices.i<-DF[ grepl(reponses.pattern, names(DF))==TRUE][i , ]
          as.character(unlist(choices.i))->choices.i
          choices.i[!is.na(choices.i)]->choices.i
          radioButtons(inputId=inputId.i,
                       label=label.i,
                       choices=choices.i,
                       selected=choices.i[1], width = "100%", inline=T)
        } else {
          if(DF[i, type.var]=="autre.precisez"){
            textInput(inputId = inputId.i,
                      label = label.i,
                      value = " ",
                      width = "100%")
          } else {
            if(DF[i, type.var]=="texte"){
              textInput(inputId = inputId.i,
                        label = label.i,
                        value = "f",
                        width = "100%")
            } else { }
          }
        }
      }
    }
  }
}
)
return(lapy)
}
