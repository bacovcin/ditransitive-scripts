library(mgcv)
library(rstan)

load('sepprior.RData')
load('ordprior.RData')
load('noiprior.RData')

set_cppo("fast")
sep <- stan(file='full_model.stan',data=sepprior,iter=10,chains = 1, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAD','bToDA','bAccAct','bAccPas','bToPas','gToAD','gToDA','gAccAct','gAccPas','gToPas','dToAD','dToDA','dAccAct','dAccPas','dToPas','order','heavy'))
sep <- stan(fit=sep,data=sepprior,iter = 3000, chains = 3, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAD','bToDA','bAccAct','bAccPas','bToPas','gToAD','gToDA','gAccAct','gAccPas','gToPas','dToAD','dToDA','dAccAct','dAccPas','dToPas','order','heavy'))
print(sep,digits=4)
save(sep,file='sep.RData')

order <- stan(file='order_model.stan',data=orderprior,iter=10,chains = 1, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAD','bToDA','bAccAct','bAccPas','bToPas','gToAD','gToDA','gAccAct','gAccPas','gToPas','dToAD','dToDA','dAccAct','dAccPas','dToPas','heavy'))
order <- stan(fit=order,data=orderprior,iter = 3000, chains = 3, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAD','bToDA','bAccAct','bAccPas','bToPas','gToAD','gToDA','gAccAct','gAccPas','gToPas','dToAD','dToDA','dAccAct','dAccPas','dToPas','heavy'))
print(order,digits=4)
save(order,file='order.RData')
