library(mgcv)
library(rstan)

load('nprior.RData')

set_cppo("fast")
sep <- stan(file='~/Git/ditransitive-scripts/model_sep.stan',data=prior,iter=10,chains = 1, pars=c('a','b','heavy'))
sep <- stan(fit=sep,data=prior,iter = 2000, chains = 5, pars=c('a','b','heavy'))
print(sep,digits=4)
save(sep,file='nsep.RData')

m2uh <- stan(file='~/Git/ditransitive-scripts/model_2unified_hyperprior.stan',data=prior,iter=10,chains = 1, pars=c('a','b','heavy'))
m2uh <- stan(fit=m2uh,data=prior,iter = 2000, chains = 5, pars=c('a','b','heavy'))
print(m2uh,digits=4)
save(m2uh,file='nm2uh.RData')

m3uh <- stan(file='~/Git/ditransitive-scripts/model_3unified_hyperprior.stan',data=prior,iter=10,chains = 1, pars=c('a','b','heavy'))
m3uh <- stan(fit=m3uh,data=prior,iter = 2000, chains = 5, pars=c('a','b','heavy'))
print(m3uh,digits=4)
save(m3uh,file='nm3uh.RData')

m23uh <- stan(file='~/Git/ditransitive-scripts/model_23unified_hyperprior.stan',data=prior,iter=10,chains = 1, pars=c('a','b','heavy'))
m23uh <- stan(fit=m23uh,data=prior,iter = 2000, chains = 5, pars=c('a','b','heavy'))
print(m23uh,digits=4)
save(m23uh,file='nm23uh.RData')

m2up <- stan(file='~/Git/ditransitive-scripts/model_2unified_prior.stan',data=prior,iter=10,chains = 1, pars=c('a','b','heavy'))
m2up <- stan(fit=m2up,data=prior,iter = 2000, chains = 5, pars=c('a','b','heavy'))
print(m2up,digits=4)
save(m2up,file='nm2up.RData')

m3up <- stan(file='~/Git/ditransitive-scripts/model_3unified_prior.stan',data=prior,iter=10,chains = 1, pars=c('a','b','heavy'))
m3up <- stan(fit=m3up,data=prior,iter = 2000, chains = 5, pars=c('a','b','heavy'))
print(m3up,digits=4)
save(m3up,file='nm3up.RData')

m23up <- stan(file='~/Git/ditransitive-scripts/model_23unified_prior.stan',data=prior,iter=10,chains = 1, pars=c('a','b','heavy'))
m23up <- stan(fit=m23up,data=prior,iter = 2000, chains = 5, pars=c('a','b','heavy'))
print(m23up,digits=4)
save(m23up,file='nm23up.RData')

