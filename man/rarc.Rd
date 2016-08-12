\name{rarc}
\alias{rarc}
\title{Computes rarefaction curves and statistical envelop}
\description{\code{rarc} computes rarefaction curves and returns a data frame with bootstrap estimates of species richness, its statistical envelop and the average number of individuals for a given sample size.}
\usage{rarc(matrix, samplesize=NULL, nrandom=99, p1=0.975, p2=0.025, save=FALSE)}
\arguments{
\item{matrix}{\code{matrix} is a typical species-sample matrix. Rows correspond to samples whereas columns stand for species.}
\item{samplesize}{A vector containing the different sample sizes for which the computations are required. If missing or non vector, \code{samplesize} is replaced by a sequence from 1 to the maximum possible sample size given the size of \code{matrix}.}
\item{nrandom}{\code{nrandom} is the number of a randomizations used in the bootstrap estimation. Values of \code{nrandom} <10 or NULL are set to 99.}
\item{p1}{The probability value used to compute the upper bound of the statistical envelop using \code{quantile}. Default set to 0.975.}
\item{p2}{The probability value used to compute the lower bound of the statistical envelop using \code{quantile}. Default set to 0.025.}
\item{save}{If \code{TRUE} the function returns the raw values used in the estimation of the quantiles.}
}
\value{
\item{out}{A data frame with 5 columns giving the mean species richness, the lower and upper bound of the richness corresponding to the quantile values for p1 and p2, the mean number of individuals and the sample size.}
\item{bootstrapped.val}{Generated if \code{save=TRUE}. A list of length of the number of different sample sizes analyzed containing the raw values used to computed the quantiles. These values can be used for example to compute the standard error as illustrated in the examples below.}}
\author{Jean-Pierre Rossi \email{jean-pierre.rossi@supagro.inra.fr}}
\seealso{\code{\link{raref}, \link{raref2}, \link{rich}}}

\examples{
\dontrun{
data(ef)
t <- rarc(ef,samplesize=c(5,10,15,20,25), nrandom=30, p1=0.975, p2=0.025)
head(t)

# Plot the outputs
plot(t$out[,6],t$out[,1], type="b", ylim=range(c(t$out[,2],t$out[,3])),
	xlab="number of sampling units", ylab="richness")
points(t$out[,6] , t$out[,2], type="l", col="red")
points(t$out[,6] , t$out[,3], type="l", col="blue")
}

#See the package vignette for more examples. Type: vignette("rich_introduction").
}
