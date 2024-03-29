% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/poids_sample.R
\name{poids.sample}
\alias{poids.sample}
\title{La fonction "poids.sample"}
\usage{
poids.sample(data = datas, varpoids = c("Ancienneté",
  "Fréquence.contact"), orderpoids = list(c(4, 3, 1, 2, 5), c(1, 3, 4,
  2)))
}
\arguments{
\item{data}{un data.frame}

\item{varpoids}{vecteur des noms des variables à peser (au moins égal à 1). Doit être de type factor().}

\item{orderpoids}{pour chaque variable à peser, l'ordre de poid des levels. Il s'agit d'un ordre croissant: l'on range les levels par ordre croissant.}
}
\description{
"poids.sample" permet de calculer des poids individuels qui sont le produit de poids relatifs affectés à chaque variable factor().
}
\examples{
data.frame("Ancienneté"=rep(c("A", "B", "C", "D", "E"), 4), "Fréquence.contact"=rep(c("a", "b", "c", "d"), 5))->df
poids.sample(data=df,
varpoids=c("Ancienneté", "Fréquence.contact"),
orderpoids=list(c(4, 3, 1, 2, 5), c(1, 3, 4, 2)))
#Signifie que pour "Ancienneté" le levels [4] est le plus faible, le moins pesé, le moins important.
}
\author{
Elie
}
