c2m <-
function(pop1,pop2,pop3=NULL,nrandom=99,pr1=0.025,pr2=0.975,verbose=TRUE)
 { 

if(is.null(pop3)==TRUE){
	n.groupe1 <- length(pop1) ; n.groupe2 <- length(pop2)
	observation.vec<-c(pop1, pop2) ; GrandN<-n.groupe1 + n.groupe2
	vecteur_numero_ordre <- c(1:GrandN)
	mean.scores.simulated <-vector(length=nrandom)
	for (random in 1:(nrandom)) {
	  vecteur_ordre <- sample(vecteur_numero_ordre,GrandN,replace=FALSE)
	  vecteur_ordrep1<-vecteur_ordre[1:n.groupe1]
	  vecteur_ordrep2<-vecteur_ordre[(n.groupe1+1):(n.groupe1+n.groupe2)] 
	  vecteur.random.1<-observation.vec[vecteur_ordrep1]
	  vecteur.random.2<-observation.vec[vecteur_ordrep2]
	  mean.scores.simulated[random]<-mean(vecteur.random.1)-mean(vecteur.random.2)
	  }
		mean.scores.obs<- mean(pop1) - mean(pop2)
		mean.pop1.obs<- mean(pop1) ; mean.pop2.obs<- mean(pop2)
	}

if(is.null(pop3)==FALSE){
	n.groupe1 <- length(pop1)
	n.groupe2 <- length(pop2)
	n.groupe3 <- length(pop3)
	observation.vec<-c(pop1, pop2, pop3)
	GrandN<-n.groupe1 + n.groupe2 + n.groupe3
	vecteur_numero_ordre<-c(1:GrandN)
	mean.scores.simulated <-vector(length=nrandom)
	
	for (random in 1:(nrandom)) {
	  vecteur_ordre <- sample(vecteur_numero_ordre,GrandN,replace=FALSE)
	  vecteur_ordrep1<-vecteur_ordre[1:n.groupe1]
	  vecteur_ordrep2<-vecteur_ordre[(n.groupe1+1):(n.groupe1+n.groupe2)] 
	  vecteur_ordrep3<-vecteur_ordre[(n.groupe1+n.groupe2+1):(n.groupe1+n.groupe2+n.groupe3)]
	  vecteur.random.1<-observation.vec[vecteur_ordrep1]
	  vecteur.random.2<-observation.vec[vecteur_ordrep2]
	  vecteur.random.3<-observation.vec[vecteur_ordrep3]
	  mean.scores.simulated[random]<-mean(vecteur.random.1)-mean(vecteur.random.2)
	  mean.scores.simulated[random] <- mean(c(vecteur.random.1,vecteur.random.3)) - mean(c(vecteur.random.2,vecteur.random.3))
	  }
		mean.scores.obs<- mean(c(pop1,pop3)) - mean(c(pop2,pop3))
		mean.pop1.obs<- mean(c(pop1,pop3)) ; mean.pop2.obs<- mean(c(pop2,pop3))
	}

mean.scores<-c(mean.scores.obs,mean.scores.simulated[1:nrandom])
inf<-mean.scores[mean.scores<mean.scores.obs]
sup<-mean.scores[mean.scores>mean.scores.obs]
egal<-mean.scores[mean.scores==mean.scores.obs]

outtable<-as.data.frame(matrix(ncol=1,nrow=8));names(outtable)<-" " 
row.names(outtable)<- c("mv1","mv2", "mv1-mv2", "p", paste("quantile",pr1,sep=" "), paste("quantile", pr2, sep=" "), "randomized mv1-mv2", "nrandom")
outtable[1,1]<- mean.pop1.obs
outtable[2,1]<- mean.pop2.obs
outtable[3,1]<-mean.scores.obs
      if(mean.scores.obs>0) 
      outtable[4,1]<-(length(sup)+length(egal))/(nrandom+1)
      if(mean.scores.obs<0)
      outtable[4,1]<-(length(inf)+length(egal))/(nrandom+1)
      if(mean.scores.obs==0)
      outtable[4,1]<-"NC"
outtable[5,1]<-quantile(mean.scores,probs = c(pr1, pr2),names=FALSE)[1]
outtable[6,1]<-quantile(mean.scores,probs = c(pr1, pr2),names=FALSE)[2]
outtable[7,1]<-mean(mean.scores)
outtable[8,1]<-nrandom

  if(verbose==TRUE) out<-list(res=outtable, rand=mean.scores)
  if(verbose==FALSE) out<-list(res=outtable) 

return(out)
}
