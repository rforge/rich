\name{ef}
\alias{ef}
\docType{data}
\title{Soil macrofauna in a tropical forest site (Elahe, French Guiana) }
\usage{
  data(ef)
}
\description{
  The \code{ef} data set is a data frame describing the species abundance of soil macrofauna in a set of soil samples collected in a secondary forest in French Guiana. These data were collected in the viciny of the Wayana village of Elahe situated on the Tampoc river.  
 }
\format{
 \code{ef} is a data frame containing the observations of 121 species (columns) at 30 sampling locations (rows).
}
\source{
Rossi, J.-P., Celini, L., Mora, P., Mathieu, J., Lapied, E., Nahmani, J., Ponge, J.-F., and Lavelle, P. (2010). Decreasing fallow duration in tropical slash-and-burn agriculture alters soil macro-invertebrate diversity: A case study in southern French Guiana. Agriculture, Ecosystems and Environment 135, 148-154.
}
\examples{
## Estimating species richness using rich
data(ef)
rich(matrix=ef)

}

