\name{raref}
\alias{raref}
\title{Estimates the species richness for a given number of sampled individuals on the basis of the corresponding rarefaction curve}
\description{\code{raref} computes rarefaction curve and interpolates the species richness corresponding to a given density of individuals.}
\usage{
raref(matrix, dens, nrandom=99)
}
\arguments{
\item{matrix}{\code{matrix} is a typical species-sample matrix. Rows correspond to samples whereas columns stand for species.}
\item{dens}{A number corresponding to the density for which a species richness estimation is required. \code{dens} must be comprised within the range of the observed density.
}
\item{nrandom}{\code{nrandom} is the number of a randomizations used in the bootstrap estimation. Values of \code{nrandom} <10 or NULL are set to 99.}
}
\value{\code{raref} returns a list with two components.
\item{rar}{A data frame with three columns giving the observed species richness for different sample size and corresponding density of individuals.}
\item{Sinterp}{A vector with the term dens and the corresponding interpolated species richness.}
}
\references{
Gotelli, N., Colwell, R. (2001). Quantifying biodiversity: procedures and pitfalls in the
measurement and comparison of species richness. Ecology Letters 4, 379-391.
}
\author{
Jean-Pierre Rossi \email{jean-pierre.rossi@supagro.inra.fr}
}
\seealso{
\code{\link{rarc}, \link{raref2}, \link{rich}}
}
\examples{
\dontrun{
data(ef)
raref(ef, dens=1100, nrandom=100)
}}

