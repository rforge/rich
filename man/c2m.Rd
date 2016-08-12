\name{c2m}
\alias{c2m}

\title{Compares 2 mean values using a randomization test}
\description{
Mean values of 2 populations are compared using a randomization procedure. Overlapping populations are allowed.}
\usage{
c2m(pop1, pop2,pop3=NULL,nrandom,pr1=0.025,pr2=0.975,verbose=TRUE)
}
\arguments{
  \item{pop1}{A vector with the observed values for population 1.}
  \item{pop2}{A vector with the observed values for population 2.}
  \item{pop3}{A vector with the observed values that are common to population 1 and 2.}
  \item{nrandom}{Number of randomizations to perform. Default fixed to 99.}
  \item{pr1}{Lower probability level for quantile computations. Default fixed to 0.025.}
  \item{pr2}{Higher probability level for quantile computations. Default fixed to 0.975.}
  \item{verbose}{If \code{verbose} is TRUE \code{c2m} returns a vector that contains the observed and randomized differences between mean richnesses.}
}
\details{This randomization test compares the average value of a quantitative variable sampled in 2 populations.
Details are available in Manly (1997). In some cases, populations share some observed values: for example if we compare the mean annual temperature of sites where either species A or B is present and if A and B are sympatric in some localities (see example below). Those shared values are passed to \code{c2m} by the argument \code{pop3}.\cr
If the mean value for population 1 \eqn{\ge} mean value for population 2, \code{p} is the number of randomizations for which the mean value for population 1 \eqn{\ge} mean value for population 2 divided by the number of randomizations + 1. 
If the mean value for population 1 \eqn{\le} mean value for population 2, \code{p} is the number of randomizations for which the mean value for population 1 \eqn{\le} mean value for population 2 divided by the number of randomizations + 1.
If \eqn{mv1 = mv2} \code{p} in not computed (\code{p=NC}).
}

\value{
\item{res }{A data frame showing the outputs of the randomization test:\cr
      \code{mv1 } Observed mean values over samples forming population 1. \cr
      \code{mv2 } Observed mean values over samples forming population 2. \cr
      \code{mv1-mv2 } Difference between observed mean values of population 1 and population 2.\cr
   \code{p } Probability of encountering such a value for \code{mv1-mv2 } (see details above).\cr
      \code{quantile for pr1 } Quantile value for probability level \code{pr1}. \cr
      \code{quantile for pr2 } Quantile value for probability level \code{pr2}. \cr
      \code{randomized mv1-mv2 } Mean values of randomized and the observed values. \cr
      \code{nrandom } Number of randomizations used in the test.\cr}
\item{rand }{A vector of \code{nrandom+1} values corresponding to the observed difference of \code{mv1-mv2} and the randomized values. \code{rand} is available if \code{verbose == TRUE.}}
}

\references{
Manly, B.F.J. (1997). Randomization and Monte Carlo methods in biology. Chapman & Hall.
}
\author{
Jean-Pierre Rossi, <jean-pierre.rossi@supagro.inra.fr>
}
\note{
The observed difference between populations is included in the numerator and the denominator when computing the probability \code{p}. This is justified because if the null hypothesis (there is no difference between populations) is true then the observed difference between populations is just another value for the randomization distribution (Manly, 1997, p. 7).
}

\seealso{
\code{\link{c2cv}, \link{rich}}
}

\examples{

\dontrun{
# The example of mandible length of male and female 
# golden jackals from Manly (1997), p.4.
males<-c(120, 107, 110, 116, 114, 111, 113, 117, 114, 112)
females<-c(110, 111, 107, 108, 110, 105, 107, 106, 111, 111)
out <- c2m(pop1=males, pop2=females, nrandom=99)
out$res
hist(out$rand)
abline(v=out$res[3,1], col="red")
abline(v=out$res[5,1], col="blue")
abline(v=out$res[6,1], col="blue")

# Compare simulated datasets
pop1<-rnorm(10)
pop2<-rnorm(10)
out <- c2m(pop1=pop1, pop2=pop2, nrandom=99)
out$res
hist(out$rand)
abline(v=out$res[3,1], col="red")
abline(v=out$res[5,1], col="blue")
abline(v=out$res[6,1], col="blue")


# Maximum temperature in a set of sites where the bark beetle Tomicus destruens
# or T. piniperda are present. Both species are present at 4 sites.
data(Tomicus)
out <- c2m(pop1=Tomicus$destruens,pop2=Tomicus$piniperda, 
pop3=Tomicus$both, nrandom=99)
out$res
hist(out$rand)
abline(v=out$res[3,1], col="red")
abline(v=out$res[5,1], col="blue")
abline(v=out$res[6,1], col="blue")

# using c2m with outputs of rich
data(ef)
o1 <- rich(matrix=ef, nrandom=99, verbose=TRUE)
data(ea)
o2 <- rich(matrix=ea, nrandom=99, verbose=TRUE)
out <- c2m(pop1=o1$sumrow, pop2=o2$sumrow, nrandom=999, verbose=TRUE)
hist(out$rand)
abline(v=out$res[3,1], col="red")
abline(v=out$res[5,1], col="blue")
abline(v=out$res[6,1], col="blue")
}}

