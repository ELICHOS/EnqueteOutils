% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sphinx_to_R.R
\name{sphinx_to_R}
\alias{sphinx_to_R}
\title{sphinx_to_R: sort un data.frame à partir d'un fichier .que (sphinx) enregistré en csv}
\usage{

  sphinx_to_R(csv.file = "C:/Users/elie/Desktop/Bureau/FNDSA 21_copie.csv",
  header = FALSE, encoding = "UTF-8")
}
\arguments{
\item{csv.file}{fichier csv sphinx}

\item{...}{others arguments passsed to read.csv (header=FALSE, skip=8, sep = c("=", "\\t"),encoding = 'UTF-8')}
}
\value{
un df
}
\description{
Pour enregistrer le gichier .que en csv: lors de l'import, ne pas splitter par tab, seulement par "=".Iil faut une première ligne vide et un header=TRUE pour créer "V1"
}
