\name{rich}
\alias{rich}
\title{Species richness, their confidence interval and other useful  indices}
\description{Computes the cumulative and average species richness over a set of samples, the associated bootstrap statistics and other useful indices.}
\usage{rich(matrix, verbose = FALSE, nrandom = NULL)}
\arguments{
\item{matrix}{\code{matrix} is a typical species-sample matrix. Rows correspond to samples whereas columns stand for species.}
\item{verbose}{If \code{verbose=FALSE}, a simplied output is returned.}
\item{nrandom}{Number or randomizations if bootstrap estimations are required. Non-null values < 10 are set to 99.}
}
\details{
\code{rich} computes basic descriptive statistics from typical species by sample data sets.}
\value{
\item{cr }{Cumulated richness over sampling units.} 
\item{mr }{Mean richness over sampling units.}
\item{mrsd }{Standard deviation of the mean richness.}
\item{singletons }{Species with at most one observation.}
\item{doubletons }{Species with at most two observations.}
\item{uniques }{Species encountered in only one sample.}
\item{duplicates }{Species encountered in only two samples.}

\item{bootCR }{A data frame showing the outputs of the bootstrap analyses of the cumulative richness (computed if \code{nrandom != FALSE}):\cr
      \code{cr.obs} Observed cumulative richness, equal to \code{cr} above. \cr
      \code{cr.boot }Mean of the bootstrap values. \cr
      \code{cr.bcorr }Mean of the bootstrap values corrected for the bias estimated below. \cr
      \code{cr.bias }Bias. \cr
      \code{cr.se }Standard error of the cumulative richness estimated by bootstrap. This is estimated as the standard deviation of the bootstrap values. \cr
      \code{cr.lbn }Lower bound of the first order normal approximation confidence interval (see \code{boot.ci {boot}}). \cr
      \code{cr.ubn }Upper bound of the first order normal approximation confidence interval (see \code{boot.ci {boot}}). \cr}

\item{bootMR }{A data frame showing the outputs of the bootstrap analyses of the mean richness (computed if \code{nrandom != FALSE}):\cr
      \code{mr.obs} Observed mean richness, equal to \code{mr} above. \cr
      \code{mr.boot }Mean of the bootstrap values. \cr
      \code{mr.bcorr }Mean of the bootstrap values corrected for the bias estimated below. \cr
      \code{mr.bias }Bias. \cr
      \code{mr.se }Standard error of the mean richness estimated by bootstrap. This is estimated as the standard deviation of the bootstrap values. \cr
      \code{mr.lbn }Lower bound of the first order normal approximation confidence interval (see \code{boot.ci {boot}}). \cr
      \code{mr.ubn }Upper bound of the first order normal approximation confidence interval (see \code{boot.ci {boot}}). \cr}

\item{nrandom }{Number of randomizations used in the bootstrap.}
\item{richvec }{A vector containing the observed richness in each sample or populations. The mean of \code{richvec} corresponds to \code{mr} (available if \code{verbose == TRUE}).}
\item{matrix }{The data set passed to \code{rich} as the \code{matrix} argument (available if \code{verbose == TRUE}).}
\item{matrixbin }{Binary (presence/absence) transformation of the data set passed to \code{rich} as the \code{matrix} argument (available if \code{verbose == TRUE}).}
\item{sumrow }{A vector of the sum of species for each sample (available if \code{verbose == TRUE}).}
\item{sumcol }{A vector of the sum of sample for each species (available if \code{verbose == TRUE}).}
\item{zeroes }{Number of zeroes in the data set (available if \code{verbose == TRUE}).}
}     
\references{
Manly, B.F.J. (1997). Randomization and Monte Carlo methods in biology. Chapman & Hall.
}
\author{
Jean-Pierre Rossi \email{jean-pierre.rossi@supagro.inra.fr}
}
\seealso{
\code{\link{rarc}, \link{raref2}, \link{raref}}
}
\examples{
data(ef)
# No bootstrap statistics
rich(matrix=ef)

\dontrun{
# Bootstrap estimation based on 499 randomizations
rich(matrix=ef, nrandom=499)}
}


