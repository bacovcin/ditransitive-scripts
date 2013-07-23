library(mgcv)
library(rstan)

standardize = function(dataMat){
  m = mean(dataMat)
  sd = sd(dataMat)
  zDataMat = (dataMat-m)/sd
  return(zDataMat)
}

load('~/Git//ditransitive-scripts/prior.RData')

fit <- stan(file='STANmodel.stan',data=test_dat1,iter=10,chains = 1, pars=c('a','b','heavy'))
print(fit,digits=4)
extract(fit)

fit1 <- stan(fit=fit,data=test_dat1,iter = 10000, chains = 4, pars=c('a','b','heavy'))

print(fit1,digits=4)
plot(fit1)

save(fit1,file='~/Dropbox/Papers/QP1/DigsAbstract/StanResults1.RData')
