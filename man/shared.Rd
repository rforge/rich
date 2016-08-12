\name{shared}
\alias{shared}
\title{Computes the number of species shared by groups of sampling units and other useful statistics}
\description{\code{shared} computes the richness of each group of sample depicting a community, the number of species shared by pairs of communities and the total number of species for each pairs of community.}
\usage{
shared(lmatrix)
}
\arguments{
  \item{lmatrix}{A list of data matrices representing the species composition of local communities. All matrices must have the same size.}
}
\value{A data frame with: on the diagonal the observed richness for each local community, above the diagonal: the richness common to pairs of communities (shared richness) and below the diagonal: the total richness for pooled pairs of communities.
}
\references{
Empirical data have been presented in a way very similar to what is returned by \code{shared} in the following papers:

Rossi, J.-P., Mathieu, J., Cooper, M., and Grimaldi, M. (2006). Soil macrofaunal biodiversity in amazonian pastures: Matching sampling with patterns. Soil Biology & Biochemistry, 38, 2178-2187.

Rossi, J.-P., Celini, L., Mora, P., Mathieu, J., Lapied, E., Nahmani, J., Ponge, J.-F., and Lavelle, P. (2010). Decreasing fallow duration in tropical slash-and-burn agriculture alters soil macro-invertebrate diversity: A case study in southern French Guiana. Agriculture, Ecosystems and Environment 135, 148-154.
}
\author{
Jean-Pierre Rossi \email{jean-pierre.rossi@supagro.inra.fr}
}
\seealso{
\code{\link{rich}}}
\examples{
data(efeb)
shared(efeb)

sp1<-c(1,2,3,4,5)
sp2<-c(0,0,0,0,0)
sp3<-c(1,1,0,0,0)
sp4<-c(0,0,0,0,0)
site1<-cbind(sp1, sp2, sp3, sp4)
colnames(site1)<-c("sp1", "sp2", "sp3", "sp4")
sp1<-c(1,2,3)
sp2<-c(1,0,0)
sp3<-c(0,0,0)
sp4<-c(0,0,0)
site2<-cbind(sp1, sp2, sp3, sp4)
colnames(site2)<-c("sp1", "sp2", "sp3", "sp4")
sp1<-c(1,2,3,4)
sp2<-c(1,0,0,0)
sp3<-c(1,0,0,0)
sp4<-c(1,0,0,0)
site3<-cbind(sp1, sp2, sp3, sp4)
colnames(site3)<-c("sp1", "sp2", "sp3", "sp4")

data<-list(site1,site2, site3)
names(data)<-c("site1","site2","site3")
shared(data)}
