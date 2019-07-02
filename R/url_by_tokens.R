#' @author Elie
#' @description "url.by.tokens" permet de créer rapidement les URL d'accès aux questionnaires LimeSurvey pour les contacts invités
#' @param text.before.token    première partie de l'url avant le token
#' @param text.after.token  deuxième partie de l'url après le token
#' @param data =contacts, data.frame avec au moins les tokens et les emails
#' @param token.var =NULL character string ou indice numérique. Si NULL alors tokenvar<-"token"
#' @param email.var =NULL character string ou indice numérique. Si NULL alors emailvar<-"email"
#' @return res est une liste avec le df contenant les emails et les urls, et un data.frame de control avec les emails, les url et les tokens.
#' @note Voir sur LimeSurvey pour les infos d'URL et les tokens.
#' @title La fonction "url.by.tokens"
#' @examples
#' url.by.tokens(text.before.token=" https://enqconj-mrie.limequery.org/128784?token=",
#' text.after.token="&lang=fr",
#' data=contacts,
#' token.var=NULL,
#' email.var=NULL)
#' @export
url.by.tokens<-function(text.before.token=" https://enqconj-mrie.limequery.org/128784?token=",
                        text.after.token="&lang=fr",
                        data=contacts,
                        token.var=NULL,
                        email.var=NULL){
  if(is.null(token.var)){tokenvar<-"token"} else {tokenvar<-token.var}
  if(is.null(email.var)){emailvar<-"email"} else {emailvar<-email.var}
  vec.url<-sapply(1:nrow(data), function(i){
    paste(text.before.token,
          data[i, tokenvar],
          text.after.token,
          sep="")
  })
  vec.mails<-data[, emailvar]
  data.frame("url"=vec.url, "email.control"=vec.mails)->df
  ####
  cbind(df, data[, c(emailvar, tokenvar)])->df.control
  if(FALSE%in%(df.control[, "email.control"]==df.control[, emailvar])){
    message("Erreur d'appriemment")
  } else {message("Appariemment OK")}
  ####
  res<-list("df"=df, "df.control"=df.control)
  return(res)
}
