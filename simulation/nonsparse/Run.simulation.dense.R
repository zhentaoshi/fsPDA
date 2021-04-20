library(Matrix)
library(foreach)

source("FS.R")
source("lasso.BIC.R")
source("scm.R")
source("FS.simulation.dense.R")


T.grid=data.frame(T1=c(50,100,200),
                  T2=c(50,100,200))
h.iid=cbind(c(1,1,2,1,1,2,2),
            c(2,1,3,1,1,3,3),
            c(2,1,4,1,1,4,4))
h.nnd=cbind(c(1,1,3,1,1,3,3),
            c(2,1,4,1,1,4,4),
            c(4,1,7,1,1,7,7))
rep.time=5000
cl=24


# Lambda
# Lambda=DGP.Lambda()
load("loading.RData")
beta_0=beta0.dense(Lambda)


# Delta
Delta=list()
doParallel::registerDoParallel(cl)
for (i in 1:nrow(T.grid)) {
  Delta[[i]]=foreach(l=1:rep.time+rep.time) %dopar% {DGP.Delta(T2=T.grid$T2[i],seed=l)}
}
doParallel::stopImplicitCluster()


# i.i.d. case
Result.dense.iid=list()
doParallel::registerDoParallel(cl)
for (i in 1:nrow(T.grid)) {
  tc=Sys.time();cat(paste("iid Group",i,"starting at:",tc),"\n")
  Result.dense.iid[[i]]=as.data.frame(foreach(l=1:rep.time,.combine=rbind) %dopar% {
    FS.simulation.dense(T1=T.grid$T1[i],Delta=Delta[[i]][[l]],Lambda,h=h.iid[,i],ARMA=NULL,seed=l)
  })
  tc=Sys.time()-tc;print(tc)
}
doParallel::stopImplicitCluster()

# n.n.d case
Result.dense.nnd=list()
doParallel::registerDoParallel(cl)
for (i in 1:nrow(T.grid)) {
  tc=Sys.time();cat(paste("nnd Group",i,"starting at:",tc),"\n")
  Result.dense.nnd[[i]]=as.data.frame(foreach(l=1:rep.time,.combine=rbind) %dopar% {
    FS.simulation.dense(T1=T.grid$T1[i],Delta=Delta[[i]][[l]],Lambda,h=h.nnd[,i],seed=l)
  })
  tc=Sys.time()-tc;print(tc)
}
doParallel::stopImplicitCluster()


# Summary
result.iid=NULL;result.nnd=NULL
for (i in 1:nrow(T.grid)) {
  result.iid=rbind(result.iid,
                   c(FS.R=median(Result.dense.iid[[i]]$FS.R),
                     colMeans(Result.dense.iid[[i]][,c("FS.Bias","FS.RMSE","FS.RSquared.in","FS.RSquared.out",paste("FS.phi",1:7,sep=""))]),
                     lasso.R=median(Result.dense.iid[[i]]$lasso.R),
                     colMeans(Result.dense.iid[[i]][,c("lasso.Bias","lasso.RMSE","lasso.RSquared.in","lasso.RSquared.out",paste("lasso.phi",1:7,sep=""))]),
                     scm.R=median(Result.dense.iid[[i]]$scm.R),
                     colMeans(Result.dense.iid[[i]][,c("scm.Bias","scm.RMSE","scm.RSquared.in","scm.RSquared.out",paste("scm.phi",1:7,sep=""))])))
  result.nnd=rbind(result.nnd,
                   c(FS.R=median(Result.dense.nnd[[i]]$FS.R),
                     colMeans(Result.dense.nnd[[i]][,c("FS.Bias","FS.RMSE","FS.RSquared.in","FS.RSquared.out",paste("FS.phi",1:7,sep=""))]),
                     lasso.R=median(Result.dense.nnd[[i]]$lasso.R),
                     colMeans(Result.dense.nnd[[i]][,c("lasso.Bias","lasso.RMSE","lasso.RSquared.in","lasso.RSquared.out",paste("lasso.phi",1:7,sep=""))]),
                     scm.R=median(Result.dense.nnd[[i]]$scm.R),
                     colMeans(Result.dense.nnd[[i]][,c("scm.Bias","scm.RMSE","scm.RSquared.in","scm.RSquared.out",paste("scm.phi",1:7,sep=""))])))
}
result=rbind(cbind(DGP="iid",T.grid,result.iid),
             cbind(DGP="nnd",T.grid,result.nnd))

print(result)
save(result,beta_0,Result.dense.iid,Result.dense.nnd,file="Result.dense.RData")


