FS=function(y,Y,T1,H=1,h=0,R=NULL,alpha=0.05) {
  
  Tn=length(y)
  N=ncol(Y)
  
  y1=y[1:T1];y2=y[(T1+1):Tn]
  Y1=Y[1:T1,];Y2=Y[(T1+1):Tn,]
  
  Y.select=function(j,Y1.U) {
    X=cbind(Y1.U,Y1[,j])
    XX=t(X)%*%X
    XX.inv=try(solve(XX),silent=T)
    if ("try-error" %in% class(XX.inv)) {XX.inv=MASS::ginv(XX)}
    b.hat=XX.inv%*%t(X)%*%y1
    e.hat=y1-as.vector(X%*%b.hat)
    sigma2.hat=mean(e.hat^2)
    return(sigma2.hat)
  }
  
  U=NULL;Uc=1:N
  if(is.null(R)) {
    B=H*log(log(N))*log(T1)/T1
    IC=log(mean(y1^2))
    for (r in 1:T1) {
      sigma2.grid=sapply(Uc,FUN=Y.select,Y1.U=Y1[,U])
      IC.r=log(min(sigma2.grid))+B*r
      if(IC.r<=IC) {
        j.hat=Uc[which.min(sigma2.grid)]
        U=c(U,j.hat);Uc=setdiff(Uc,j.hat)
        IC=IC.r
      } else {
        break
      }
    }
  } else {
    for (r in 1:R) {
      sigma2.grid=sapply(Uc,FUN=Y.select,Y1.U=Y1[,U])
      j.hat=Uc[which.min(sigma2.grid)]
      U=c(U,j.hat);Uc=setdiff(Uc,j.hat)
    }
  }
  
  X=Y1[,U,drop=F]
  b.hat=as.vector(solve(t(X)%*%X)%*%t(X)%*%y1)
  
  R.hat=length(U)
  beta.hat=rep(0,N);beta.hat[U]=b.hat
  RSquared=var(as.vector(X%*%b.hat))/var(y1)
  
  y2.0.hat=as.vector(Y2[,U,drop=F]%*%b.hat)
  d.hat=y2-y2.0.hat
  ATE=mean(d.hat)
  
  if(h>0) {
    gamma.d=as.vector(acf(d.hat,lag.max=h,type="covariance",plot=F)$acf)
    wh=1-(1:h)/(h+1)
    lrvar.d=gamma.d[1]+2*sum(wh*gamma.d[-1])
  } else {
    lrvar.d=var(d.hat)
  }
  
  Z=ATE/sqrt(lrvar.d/(Tn-T1))
  phi=as.numeric(abs(Z)>qnorm(1-alpha/2))
  
  return(list(R=R.hat,beta=beta.hat,RSquared=RSquared,y2.0=y2.0.hat,d=d.hat,ATE=ATE,Z=Z,phi=phi))
}


