DGP.Lambda=function(K=4,N=100,lambda.major=c(min=1,max=2),lambda.minor=c(min=-0.1,max=0.1),seed=NULL) {
  
  set.seed(seed)
  Lambda=rbind(matrix(runif((K+1)*K,min=lambda.major[["min"]],max=lambda.major[["max"]]),nrow=K+1,ncol=K),
               matrix(runif(K*(N-K),min=lambda.minor[["min"]],max=lambda.minor[["max"]]),nrow=N-K,ncol=K))
  
  return(Lambda)
}


DGP.Delta=function(T2,mu=c(0,0.5,1,0.35,0.7),rho=c(0.3,0,0,0.3,0.3),sigma.w=1,burn=1000,seed=NULL) {
  
  library(foreach)
  
  set.seed(seed)
  w=rnorm(T2+burn,sd=sigma.w)
  
  if(burn==0) {
    
    # AR(1) with shift
    AR.1=function(rho,mu,w,T2) {
      if(rho==0) {
        D=mu+w
      } else {
        d=0;D=NULL
        for (t in 1:T2) {
          d=mu+rho*d+w[t]
          D=c(D,d)
        }
      }
      return(D)
    }
    
    Delta=cbind(w,
                foreach(i=1:length(mu),.combine=cbind) %do% {AR.1(rho[i],mu[i],w,T2)})
    
  } else {
    
    # AR(1) with shift
    AR.1=function(rho,mu,w,T2,burn) {
      if(rho==0) {
        D=mu+w[-(1:burn)]
        return(D)
      } else {
        d=0;D=NULL
        for (t in 1:(T2+burn)) {
          d=mu+rho*d+w[t]
          D=c(D,d)
        }
        return(D[-(1:burn)])
      }
    }
    
    Delta=cbind(w[-(1:burn)],
                foreach(i=1:length(mu),.combine=cbind) %do% {AR.1(rho[i],mu[i],w,T2,burn)})
    
  }
  
  colnames(Delta)=NULL
  return(Delta)
}


lrvar=function(x,lag.max) {
  if(lag.max>0) {
    gamma.seq=as.vector(acf(x,lag.max,type="covariance",plot=F)$acf)
    wh=1-(1:lag.max)/(lag.max+1)
    lrvar=gamma.seq[1]+2*sum(wh*gamma.seq[-1])
  } else {
    lrvar=var(x)
  }
  return(lrvar)
}


beta0.dense=function(Lambda) {
  library(Matrix)
  sigma.eta=0.5
  Sigma.f.iid=Diagonal(4,(1:4)^2)
  Sigma.f.nnd=Diagonal(4,c(1,1/(1-0.9^2),1+0.8^2+0.4^2,(1+2*0.5*0.5+0.5^2)/(1-0.5^2)))
  beta_0=data.frame(iid=as.vector(solve(Lambda[-1,]%*%Sigma.f.iid%*%t(Lambda[-1,])+Diagonal(nrow(Lambda)-1,sigma.eta^2))%*%Lambda[-1,]%*%Sigma.f.iid%*%Lambda[1,]),
                    nnd=as.vector(solve(Lambda[-1,]%*%Sigma.f.nnd%*%t(Lambda[-1,])+Diagonal(nrow(Lambda)-1,sigma.eta^2))%*%Lambda[-1,]%*%Sigma.f.nnd%*%Lambda[1,]))
  return(beta_0)
}


FS.simulation.dense=function(T1,Delta,Lambda,h=0,
                             ARMA=list(list(ar=0.9),list(ma=c(0.8,0.4)),list(ar=0.5,ma=0.5)),
                             H=c(FS=1,lasso=2),lambda.grid=seq(0.01,1,by=0.01),
                             sigma.eta=0.5,sigma.u=1,alpha=0.05,burn=1000,seed=NULL) {
  
  library(foreach)
  
  T2=nrow(Delta);Tn=T1+T2
  N=nrow(Lambda)-1;K=ncol(Lambda)
  J=ncol(Delta)
  if(length(h)==1) {h=rep(h,J+1)}
  
  set.seed(seed)
  eta=matrix(rnorm(Tn*(N+1),sd=sigma.eta),nrow=Tn,ncol=N+1)
  if(is.null(ARMA)) {
    f=foreach(k=1:K,.combine=cbind) %do% {rnorm(Tn,sd=k*sigma.u)}
  } else {
    f=cbind(rnorm(Tn,sd=sigma.u),
            foreach(k=1:length(ARMA),.combine=cbind) %do% {as.vector(arima.sim(model=ARMA[[k]],n=Tn,n.start=burn,sd=sigma.u))})
  }
  
  Y=f%*%t(Lambda)+eta
  y1=Y[1:T1,1];y2.0=Y[(T1+1):Tn,1]
  Y=Y[,-1]
  
  # FS
  ## Out-of-sample Prediction
  result.FS=FS(y=c(y1,y2.0),Y=Y,T1=T1,H=H["FS"],h=h[1],alpha=alpha)
  e2.FS=result.FS$d
  Bias.FS=mean(e2.FS)
  RMSE.FS=sqrt(mean(e2.FS^2))
  RSquared.FS=var(result.FS$y2.0)/var(y2.0)
  ## Treatment Effect
  Delta.FS=Delta+e2.FS
  ATE.FS=colMeans(Delta.FS)
  LARvar.FS=foreach(j=1:J,.combine=c) %do% {lrvar(Delta.FS[,j],lag.max=h[j+1])}
  Z.FS=ATE.FS/sqrt(LARvar.FS/T2)
  phi.FS=as.numeric(abs(Z.FS)>qnorm(1-alpha/2))
  
  # LASSO
  ## Out-of-sample Prediction
  result.lasso=lasso.BIC(y=c(y1,y2.0),Y=Y,T1=T1,lambda.grid=lambda.grid,H=H["lasso"],h=h[1],alpha=alpha)
  e2.lasso=result.lasso$d
  Bias.lasso=mean(e2.lasso)
  RMSE.lasso=sqrt(mean(e2.lasso^2))
  RSquared.lasso=var(result.lasso$y2.0)/var(y2.0)
  ## Treatment Effect
  Delta.lasso=Delta+e2.lasso
  ATE.lasso=colMeans(Delta.lasso)
  LARvar.lasso=foreach(j=1:J,.combine=c) %do% {lrvar(Delta.lasso[,j],lag.max=h[j+1])}
  Z.lasso=ATE.lasso/sqrt(LARvar.lasso/T2)
  phi.lasso=as.numeric(abs(Z.lasso)>qnorm(1-alpha/2))
  
  # SCM
  ## Out-of-sample Prediction
  result.scm=scm(y=c(y1,y2.0),Y=Y,T1=T1,h=h[1],alpha=alpha)
  e2.scm=result.scm$d
  Bias.scm=mean(e2.scm)
  RMSE.scm=sqrt(mean(e2.scm^2))
  RSquared.scm=var(result.scm$y2.0)/var(y2.0)
  ## Treatment Effect
  Delta.scm=Delta+e2.scm
  ATE.scm=colMeans(Delta.scm)
  LARvar.scm=foreach(j=1:J,.combine=c) %do% {lrvar(Delta.scm[,j],lag.max=h[j+1])}
  Z.scm=ATE.scm/sqrt(LARvar.scm/T2)
  phi.scm=as.numeric(abs(Z.scm)>qnorm(1-alpha/2))
  
  return(c(FS=c(R=result.FS$R,
                Bias=Bias.FS,RMSE=RMSE.FS,RSquared.in=result.FS$RSquared,RSquared.out=RSquared.FS,
                ATE=c(result.FS$ATE,ATE.FS),Z=c(result.FS$Z,Z.FS[1:2]),phi=c(result.FS$phi,phi.FS)),
           lasso=c(R=result.lasso$R,lambda=result.lasso$lambda,
                   Bias=Bias.lasso,RMSE=RMSE.lasso,RSquared.in=result.lasso$RSquared,RSquared.out=RSquared.lasso,
                   phi=c(result.lasso$phi,phi.lasso)),
           scm=c(R=result.scm$R,
                 Bias=Bias.scm,RMSE=RMSE.scm,RSquared.in=result.scm$RSquared,RSquared.out=RSquared.scm,
                 phi=c(result.scm$phi,phi.scm))))
}


