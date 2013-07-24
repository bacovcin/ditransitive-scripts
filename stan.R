library(mgcv)
library(rstan)

load('~/Git//ditransitive-scripts/prior.RData')

fit <- stan(file='~/Git/ditransitive-scripts/model.stan',data=prior,iter=10,chains = 1, pars=c('a','b','heavy'))

fit1 <- stan(fit=fit,data=prior,iter = 10000, chains = 4, pars=c('a','b','heavy'))
print(fit1,digits=4)

save(fit1,file='StanResults.RData')
