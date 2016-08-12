\name{efea}
\alias{efea}
\docType{data}
\title{Soil macrofauna in a tropical forest site and cultivated plot (traditional slash-and-burn, Elahe, French Guiana) }
\usage{
  data(efea)
}
\description{
  The \code{efea} data set is a list of two data sets. Each one is a data frame giving the species abundance of soil macrofauna in a set of soil samples.  \code{efeb$ef} corresponds to samples collected in a secondary forest in French Guiana while \code{efeb$ea} contains data collected in in a field (aka abattis) in French Guiana. These data were collected in the viciny of the Wayana village of Elahe situated on the Tampoc river.  
 }
\format{
 \code{efea} is a list of two data frame containing the observations of 142 species (columns) at 30 sampling locations (rows).
}
\source{
Rossi, J.-P., Celini, L., Mora, P., Mathieu, J., Lapied, E., Nahmani, J., Ponge, J.-F., and Lavelle, P. (2010). Decreasing fallow duration in tropical slash-and-burn agriculture alters soil macro-invertebrate diversity: A case study in southern French Guiana. Agriculture, Ecosystems and Environment 135, 148-154.
}
\examples{
data(efea)
rich(matrix=efea[[1]])
shared(efea)
}


