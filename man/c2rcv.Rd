\name{c2rcv}
\Rdversion{0.1}
\alias{c2rcv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compares the cumulative richness of two communities and rescales the richnesses in function of density}

\description{
\code{c2rcv} first rarefies the richness of the community with highest density to get an estimation of its richness for a density comparable to that of the community with lower density. A randomization test is then performed on the difference between these richnesses. Randomizations are performed under the constraint that the resulting density for both communities is comparable to that of the community with lower density.}

\usage{
c2rcv(com1,com2,nrandom,tolerance=0.1,pr1=0.025,pr2=0.975,verbose=FALSE)}

\arguments{
 \item{com1}{A first species-sample matrix (community 1). Rows correspond to samples whereas columns stand for species.}
 \item{com2}{A second species-sample matrix (community 2). Rows correspond to samples whereas columns stand for species.}
 \item{nrandom}{Number of randomizations to be performed. Default fixed to 99.}
 \item{tolerance}{A number used to calculate the range of acceptable values for the density of simulated communities. Let \eqn{D} be largest density (com1 or com2). The acceptable interval is \eqn{D \pm tolerance \times D}.}
  \item{pr1}{Lower probability level for quantile computations. Default fixed to 0.025.}
  \item{pr2}{Higher probability level for quantile computations. Default fixed to 0.975.}
\item{verbose}{If \code{verbose==TRUE} the function returns detailed randomization outputs.}
}

\details{
 The function calls \link{raref2} to rarefy the species richness of the community with higher density.
}
\value{
\item{dmean }{The mean value of the difference between the rarefied cumulative richness and the observed cumulative richness in the other community.}
\item{q1 }{First quantile for the distribution of randomized values of \code{dmean}. }
\item{q2 }{Second quantile for the distribution of randomized values of \code{dmean}.} 

If \code{verbose==TRUE}, \code{c2rcv} returns 6 additional records of length equal to the number of randomization:
\item{d }{A vector of length \code{nrandom} containing the values of the differences between the rarefied cumulative richness and the observed cumulative richness in the other community for each randomization.} 
\item{drand }{A vector of length \code{nrandom} containing the values of the randomized differences between the rarefied cumulative richness and the observed cumulative richness in the other community.} 
\item{rarefieD }{A vector of length \code{nrandom} containing the density of the community with the largest initial density after rarefaction. These values are as close to the density of the community with lower density as the parameter \code{tolerance} is low.} 
\item{lowerD }{The density of the community with lower density. This value is constant.} 
\item{rarefiedS }{A vector of length \code{nrandom} containing the cumulative richness of the rarefied community. These values are to be compared with \code{lowerDS}.} 
\item{lowerDS }{Cumulative richness of the community with lower density.} 
}

\references{
Manly, B.F.J. (1997). Randomization and Monte Carlo methods in biology. Chapman & Hall.
}
\author{
Jean-Pierre Rossi \email{jean-pierre.rossi@supagro.inra.fr}
}
\note{
The observed difference between populations is included in the numerator and the denominator when computing the probability \code{p}. This is justified because if the null hypothesis (there is no difference between populations) is true then the observed difference between populations is just another value for the randomization distribution (Manly, 1997, p. 7). The results are sensitive to the value of the \code{tolerance} parameter.
}
\seealso{
\code{\link{raref2}, \link{c2cv}, \link{rich}} 
}

\examples{
data(efea)
c2rcv(com1=efea$ef,com2=efea$ea,nrandom=99,tolerance=0.01,verbose=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ species richness }
\keyword{ rarefy }
\keyword{ richness comparison }
% __ONLY ONE__ keyword per line
