\name{Tomicus}
\alias{Tomicus}
\docType{data}
\encoding{latin1} 
\title{Maximum temperature at sites hosting 2 species of closely related bark-beetles of genus Tomicus}
\usage{
  data(Tomicus)
}
\description{
  The \code{Tomicus} data set consists of a list with 3 components: \cr 
  \code{destruens} Maximum temperature at 33 sites where \emph{T. destruens} was observed.\cr 
  \code{piniperda} Maximum temperature at 20 sites where \emph{T. piniperda} was observed.\cr 
  \code{both} Maximum temperature at 4 sites where both \emph{T. destruens} and \emph{T. piniperda} were observed in sympatry.\cr 
 }
\format{
 \code{Tomicus} is a list of 3 vectors.
}
\source{
Horn, A. (2006) Comparaison des distributions passee et presente de deux especes proches de scolytes, \emph{Tomicus piniperda} et \emph{T. Destruens} (Coleoptera: Scolytinae). These de l'Universite d'Orleans, France 180pp.}
\examples{
## Comparing mean temperatures at sites where either T. destruens,
## T. piniperda or both are present using comp2mean
data(Tomicus)
c2m(pop1=Tomicus$destruens,pop2=Tomicus$piniperda,
pop3=Tomicus$both, nrandom=99)
}
\keyword{datasets}

