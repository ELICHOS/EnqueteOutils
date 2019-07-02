recodes.by.prompt<-function(var_to_recode=tosave$DATA$autre_activite, recodes=NULL, id=tosave$DATA$numero, old_output=myvar){
  ####

  # divisors <- function(x){
  #   #  Vector of numberes to test against
  #   y <- seq_len(x)
  #   #  Modulo division. If remainder is 0 that number is a divisor of x so return it
  #   y[ x%%y == 0 ]
  # }

  ####
  list.of.recodes<-recodes
  if(is.null(recodes)){
    list.of.recodes<-as.character(unique(unlist(old_output)))
  }
  #print(recodes)
  length(var_to_recode[is.na(var_to_recode)|var_to_recode==""|is.null(var_to_recode)])->length.varnul
  length(var_to_recode)-length.varnul->length.varpasnul
  message(paste("Taille de la variable : ", length(var_to_recode)))
  message(paste("Nombre d'occurences à traiter: ", length.varpasnul))

  myvar.f<-list()
  for( i in 1:length(var_to_recode)){
    if(is.na(var_to_recode[i])|var_to_recode[i]==""|is.null(var_to_recode[i])){
      myvar.f[[i]]<-NA
    } else {
      message(paste("------- DEBUT : ", i))
      message("rappel des recodes : ")
    #print(data.frame(list.of.recodes))
    length(list.of.recodes)->Leng
    #max(divisors(Leng)[divisors(Leng)!=Leng])->Nrows
    print(matrix(data = paste(1:Leng, list.of.recodes, sep=" : "), ncol = 3, byrow = FALSE))
    message("VALEUR A RECODER: ")
    message("----------------------------------------")
    message(var_to_recode[i])
    message("----------------------------------------")
    n <- readline(prompt="Entrer un chiffre ou ajouter un recodes: \n(séparés par des ',')")
    #no<-as.character(n)
    no<-strsplit(x = n, split = ",")[[1]]
    #print(paste("n :", n ))
    #print(paste("no : ", no))
    #print(paste("length(no) : ", length(no) ))
    obj.to.var<-c()
    for(j in 1:length(no) ){
      no[[j]]->noj
      #message("noj :")
      #print(noj)
      as.character(1:length(list.of.recodes))->char.len
      #message("char.len :")
      #print(char.len)
    if(noj%in%char.len){
      message("op: attribution d'un recode existant (num index)")
      list.of.recodes[as.numeric(noj)]->obj.to.var[j]
    } else {
      if(noj%in%list.of.recodes){
        message("op: attribution d'un recode existant (identical char)")
        list.of.recodes[list.of.recodes==noj&!is.na(list.of.recodes)]->obj.to.var[j]
      } else {
      message("op: création d'un recode")
      list.of.recodes<-c(list.of.recodes, noj)
      noj->obj.to.var[j]
    }
    }}
    myvar.f[[i]]<-obj.to.var
    }
  }
  names(myvar.f)<-id
  return(myvar.f)
}
