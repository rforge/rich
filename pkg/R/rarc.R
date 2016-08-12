rarc <- function(matrix, samplesize=NULL, nrandom=99, p1=0.975, p2=0.025, save=FALSE)
 {
  sSRobs<- function(D,d,ss)
  {E<-D[d,]
  F<-E[1:ss,]
  return(c(specpool(F)$Species,sum(F)))
  }
if(is.null(nrandom)==TRUE | nrandom<10) nrandom<-99
a<-as.matrix(matrix)
if(is.null(samplesize)==TRUE || is.vector(samplesize)==FALSE) samplesize<-seq(1:dim(a)[1])
sortie<-matrix(NA, ncol=5,nrow=length(samplesize))
if(save==TRUE) bootlist <- vector("list", length(samplesize))
if(any(samplesize>dim(matrix)[1])) stop("sample size larger than number of lines in matrix")

for (i in 1:(length(samplesize))) {
  sb<-boot(a, sSRobs, R=nrandom, ss=samplesize[i])
  sortie[i,1]<-mean(sb$t[,1])
  sortie[i,2]<-quantile(sb$t[,1], p=p2)
  sortie[i,3]<-quantile(sb$t[,1], p=p1)
  sortie[i,4]<-mean(sb$t[,2])
  sortie[i,5]<-sd(sb$t[,2])
  if(save==TRUE) bootlist[[i]] <- sb$t
}
sortie<-as.data.frame(sortie);sortie$sample<-samplesize
names(sortie)[1:5]<-c("mean.richness", "lb.richness", "ub.richness", "mean.nb.individuals", "samples")
output <- list(out=sortie)
if(save==TRUE) output<-list(out=sortie, bootstrapped.val=bootlist)
return(output)
}


