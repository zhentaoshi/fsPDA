opt_func_scm <- function(y_0, y_N) {
  
  suppressMessages(library(CVXR))
  
  beta <- Variable(ncol(y_N), 1)
  obj <- Minimize(sum_squares(y_0 - y_N %*% beta))
  constr <- list(
    sum_entries(beta) == 1,
    beta >= 0
  )
  
  problem <- Problem(objective = obj, constraints = constr)
  result <- solve(problem, solver = "ECOS_BB")
  beta_hat <- as.vector(result$getValue(beta))
  
  return (beta_hat)
}


scm=function(y,Y,T1,h=0,alpha=0.05,tol=1e-5){
  
  Tn=length(y)
  N=ncol(Y)
  
  y1=y[1:T1];y2=y[(T1+1):Tn]
  Y1=Y[1:T1,];Y2=Y[(T1+1):Tn,]
  
  beta.hat=opt_func_scm(y1,Y1)
  R.hat=sum(abs(beta.hat)>=tol)
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

  return(list(R=R.hat,beta=beta.hat,RSquared=RSquared,y2.0=y2.0.hat,d=d.hat,ATE=ATE,Z=Z,phi=phi))
}  


