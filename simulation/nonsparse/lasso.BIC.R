lasso.BIC=function(y,Y,T1,lambda.grid=seq(0.01,1,by=0.01),H=2,h=0,alpha=0.05) {
  
  suppressMessages(library(glmnet))
  
  Tn=length(y)
  N=ncol(Y)
  B=H*log(log(N))*log(T1)/T1
  
  y1=y[1:T1];y2=y[(T1+1):Tn]
  Y1=Y[1:T1,];Y2=Y[(T1+1):Tn,]
  
  fit.lasso=glmnet(x=Y1,y=y1,family="gaussian",lambda=lambda.grid,intercept=F)
  e.hat=y1-Y1%*%fit.lasso$beta
  IC=log(colMeans(e.hat^2))+B*fit.lasso$df
  
  min.id=which.min(IC)
  lambda.hat=fit.lasso$lambda[min.id]
  
  R.hat=fit.lasso$df[min.id]
  beta.hat=fit.lasso$beta[,min.id];names(beta.hat)=NULL
  RSquared=var(as.vector(Y1%*%beta.hat))/var(y1)
  
  y2.0.hat=as.vector(Y2%*%beta.hat)
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
  
  return(list(R=R.hat,lambda=lambda.hat,beta=beta.hat,RSquared=RSquared,y2.0=y2.0.hat,d=d.hat,ATE=ATE,Z=Z,phi=phi))
}


