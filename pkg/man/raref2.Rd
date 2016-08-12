\name{raref2}
\alias{raref2}

\title{Estimates the species richness for a given number of sampled individuals on the basis of the corresponding rarefaction curve}
\description{\code{raref2} computes an estimation of the species richness by thinning the data matrix so that the overall corresponding density is comprised in a fixed interval.}
\usage{
raref2(matrix, dens, tolerance, nrandom=99)
}
\arguments{
\item{matrix}{\code{matrix} is a typical species-sample matrix. Rows correspond to samples whereas columns stand for species.}
\item{dens}{A number corresponding to the density for which a species richness estimation is required.\code{dens} must be comprised within the range of the observed density.}
\item{tolerance}{A number used to calculate the range of acceptable values for the density of simulated communities. Let \eqn{D} be largest density (com1 or com2). The acceptable interval is \eqn{D \pm tolerance \times D}.}

\item{nrandom}{\code{nrandom} is the number of a randomizations used in the bootstrap estimation. Values of \code{nrandom} <10 or NULL are set to 99.}
}
\value{\code{raref2} returns a list with two components.
\item{mean.boot}{The mean value of the bootstrap estimates of the species richness.}
\item{sd.boot}{The standard deviation of the bootstrap estimates.}
}
\author{
Jean-Pierre Rossi \email{jean-pierre.rossi@supagro.inra.fr}
}
\seealso{
\code{\link{raref}, \link{rich}}
}
\examples{
\dontrun{
data(ef)
raref2(matrix=ef,dens=1100,tolerance=0.01,nrandom=50)
}}

