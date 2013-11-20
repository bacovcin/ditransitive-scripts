library(mgcv)
library(rstan)

load('nprior.RData')

set_cppo("fast")
sep <- stan(file='model_sep.stan',data=prior,iter=10,chains = 1, pars=c('a1','a2','b1','b2','heavy','pro'))
sep <- stan(fit=sep,data=prior,iter = 5000, chains = 5, pars=c('a1','b1','a2','b2','heavy','pro'))
print(sep,digits=4)
save(sep,file='sep.RData')

tog <- stan(file='model_tog.stan',data=prior,iter=10,chains = 1, pars=c('a1','a2','b1','b2','heavy','pro'))
tog <- stan(fit=tog,data=prior,iter = 5000, chains = 5, pars=c('a1','b1','a2','b2','heavy','pro'))
print(sep,digits=4)
save(sep,file='tog.RData')

same <- stan(file='model_same.stan',data=prior,iter=10,chains = 1, pars=c('a1','a2','b1','b2','heavy','pro'))
same <- stan(fit=same,data=prior,iter = 5000, chains = 5, pars=c('a1','b1','a2','b2','heavy','pro'))
print(sep,digits=4)
save(sep,file='same.RData')
