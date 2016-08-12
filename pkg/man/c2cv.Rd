\name{c2cv}
\alias{c2cv}

\title{Comparison of 2 values of species richness using a randomization procedure}

\description{Species richnesses are computed as the cumulative value over all samples. Richnesses are compared by mean of a randomization test without controlling for differences of sampling regime of communities density.

}
\usage{
c2cv(com1,com2,nrandom=99,pr1=0.025,pr2=0.975,verbose=TRUE)}
 
\arguments{
 \item{com1}{A first species-sample matrix (community 1). Rows correspond to samples whereas columns stand for species.}
 \item{com2}{A second species-sample matrix (community 2). Rows correspond to samples whereas columns stand for species.}
 \item{nrandom}{Number of randomizations to be performed. Default fixed to 99.}
  \item{pr1}{Lower probability level for quantile computations. Default fixed to 0.025.}
  \item{pr2}{Higher probability level for quantile computations. Default fixed to 0.975.}
 \item{verbose}{If \code{verbose} is TRUE \code{c2cv} returns a vector that contains the observed and randomized differences between richnesses.}
}
\details{

If the observed richness for community 1 \eqn{\ge} observed value for community 2, \code{c2cv} returns a probability \code{p} estimated as the number of randomizations for which the observed value for community 1 \eqn{\ge} observed value for community 2 divided by the number of randomization + 1. Similarly, if the observed value for community 1 \eqn{\le} observed value for community 2, \code{p} corresponds to the frequency of such situation in the randomizations.}

\value{
If \code{verbose==TRUE} \code{c2cm} returns a data frame and a vector with the randomized values. Otherwise, only the data frame is returned.
\item{res }{A data frame with the outputs of the randomization test:\cr
   \code{cv1} Observed cumulative richness for community 1. \cr
   \code{cv2} Observed cumulative richness for community 2. \cr
   \code{cv1-cv2} Difference between observed cumulative richness of community 1 and community 2. \cr
   \code{p } Probability of encountering such a value for \code{cv1-cv2} (see details above). \cr 
   \code{quantile for pr1} Quantile value for probability level \code{pr1}. \cr
   \code{quantile for pr2} Quantile value for probability level \code{pr2}. \cr
   \code{randomized cv1-cv2} Mean values of randomized and the observed values. \cr
   \code{nrandom} Number of randomizations used in the test. \cr}   
\item{rand}{ A vector of \code{nrandom+1} values corresponding to the observed difference of \code{cv1-cv2} and the randomized values. \code{rand} is available if \code{verbose == TRUE.}}}

\references{
Manly, B.F.J. (1997). Randomization and Monte Carlo methods in biology. Chapman & Hall.
}
\author{
Jean-Pierre Rossi, <jean-pierre.rossi@supagro.inra.fr>
}
\note{
The observed difference between populations is included in the numerator and the denominator when computing the probability \code{p}. This is justified because if the null hypothesis (there is no difference between populations) is true then the observed difference between populations is just another value for the randomization distribution (Manly, 1997, p. 7).
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{c2m}, \link{rich}}
}
\examples{
\dontrun{
data(efeb)
c2cv(com1=efeb$ef,com2=efeb$eb,nrandom=100,verbose=FALSE)
}}

