# The point estimating function for binary group
ptbin.modified<-function(psest,z,y,ftilt,m0est=NULL,m1est=NULL,weight.external=NULL){
  # point estimate
  mu1est <- sum(z*y*ftilt(psest,weight.external)/psest) / sum(z*ftilt(psest,weight.external)/psest)
  mu0est <- sum((1-z)*y*ftilt(psest,weight.external)/(1-psest)) / sum((1-z)*ftilt(psest,weight.external)/(1-psest))

  if(is.null(m0est)){
    return(c(mu0est,mu1est))
  }else{
    aug0hest<-sum(ftilt(psest,weight.external)*m0est)/sum(ftilt(psest,weight.external))
    aug0zest<-sum((1-z)*m0est*ftilt(psest,weight.external)/(1-psest))/sum((1-z)*ftilt(psest,weight.external)/(1-psest))
    aug1hest<-sum(ftilt(psest,weight.external)*m1est)/sum(ftilt(psest,weight.external))
    aug1zest<-sum(z*m1est*ftilt(psest,weight.external)/psest)/sum(z*ftilt(psest,weight.external)/psest)

    mu0estaug<-mu0est+aug0hest-aug0zest
    mu1estaug<-mu1est+aug1hest-aug1zest

    return(c(mu0est,aug0hest,aug0zest,mu1est,aug1hest,aug1zest,mu0estaug,mu1estaug))
  }
}
