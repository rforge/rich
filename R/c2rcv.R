c2rcv <-
function(com1,com2,nrandom=99,tolerance=0.1,pr1=0.025,pr2=0.975,verbose=FALSE)
 {
   thinningB<-function(comD=comD,d,ub=ub,lb=lb,com=com,pr1 = pr1,pr2 = pr2)
 {
    it<-1;sss<-sum(comD)
    co<-comD[d,] 
    while(sss>ub){
    repeat{
      if(is.null(dim(co)[1])==TRUE || sum(co)<ub) {break}
      x<-sample(seq(1:dim(co)[1]),1)
      co<-co[-x,]
      sss<-sum(co)}
    if(sum(co)<lb) {co<-comD;sss<-sum(co)} 
    if(is.null(dim(co)[1])==TRUE) {co<-comD;sss<-sum(co)} 
    if(is.null(dim(co)[1])==FALSE && dim(co)[1]<2) {co<-comD;sss<-sum(co)} 
    if(it>100) {break}#{print("quit cause no solution");break}
    it<-it+1}
    outc<-c2cv(com1=co,com2=com,nrandom=1,pr1 = pr1,pr2 = pr2,verbose = TRUE)
    v<-as.numeric(c(outc$rand,sum(com),sum(co),outc$res[1,1],outc$res[2,1]))
    return(v)
 }
com1<-as.matrix(com1);com2<-as.matrix(com2)
if(tolerance<=0) stop("invalid tolerance value")
s1<-sum(com1);s2<-sum(com2)
seuil<-min(sum(com1),sum(com2))
lb<-seuil-seuil*tolerance
ub<-seuil+seuil*tolerance
 
if(s1>s2) {comD<-com1 ; com<-com2}
if(s2>s1) {comD<-com2 ; com<-com1}
if(s2==s1) stop("equal densities")
cat("computing...", "\n")
d.boot<-boot(comD,thinningB, R=nrandom, ub=ub, lb=lb, com=com,pr1=pr1,pr2=pr2)
q1<-quantile(d.boot$t[,2],probs = c(pr1, pr2),names=FALSE)[1]
q2<-quantile(d.boot$t[,2],probs = c(pr1, pr2),names=FALSE)[2]
if(verbose==TRUE)
out<-list(dmean=mean(d.boot$t[,1]),q1=q1,q2=q2,d=d.boot$t[,1],
drand=d.boot$t[,2],rarefieD=d.boot$t[,4], lowerD=d.boot$t[,3],
 rarefiedS=d.boot$t[,5], lowerDS=d.boot$t[,6])
else out<-list(dmean=mean(d.boot$t[,1]),q1=q1,q2=q2)
cat("done.", "\n")
return(out)
}
