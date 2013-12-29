library(mgcv)
library(rstan)

load('noiprior.RData')

set_cppo("fast")
noint <- stan(file='noi_model.stan',data=nointprior,iter=10,chains = 1, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAD','bToDA','bAccAct','bAccPas','bToPas','gToAD','gToDA','gAccAct','gAccPas','gToPas','heavy'))
noint <- stan(fit=noint,data=nointprior,iter = 3000, chains = 3, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAD','bToDA','bAccAct','bAccPas','bToPas','gToAD','gToDA','gAccAct','gAccPas','gToPas','heavy'))
print(noint,digits=4)
save(noint,file='noint.RData')

dasame <- stan(file='dasame.stan',data=nointprior,iter=10,chains = 1, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAD','bDA','bAccPas','bToPas','gToAD','gDA','gAccPas','gToPas','heavy'))
dsame <- stan(fit=dasame,data=nointprior,iter = 3000, chains = 3, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAD','bDA','bAccPas','bToPas','gToAD','gDA','gAccPas','gToPas','heavy'))
print(dasame,digits=4)
save(dasame,file='dasame.RData')



asame <- stan(file='asame.stan',data=nointprior,iter=10,chains = 1, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAD','bToDA','bAcc','bToPas','gToAD','gToDA','gAccAct','gAccPas','gToPas','heavy'))
asame <- stan(fit=asame,data=nointprior,iter = 3000, chains = 3, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAD','bToDA','bAcc','bToPas','gToAD','gToDA','gAccAct','gAccPas','gToPas','heavy'))
print(asame,digits=4)
save(asame,file='asame.RData')

agsame <- stan(file='agsame.stan',data=nointprior,iter=10,chains = 1, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAD','bToDA','bAcc','bToPas','gToAD','gToDA','gAcc','gToPas','heavy'))
agsame <- stan(fit=agsame,data=nointprior,iter = 3000, chains = 3, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAD','bToDA','bAcc','bToPas','gToAD','gToDA','gAcc','gToPas','heavy'))
print(agsame,digits=4)
save(agsame,file='agsame.RData')

bsame_act <- stan(file='bsame_act.stan',data=nointprior,iter=10,chains = 1, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAct','bAccAct','bAccPas','bToPas','gToAD','gToDA','gAccAct','gAccPas','gToPas','heavy'))
bsame_act <- stan(fit=bsame_act,data=nointprior,iter = 3000, chains = 3, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAct','bAccAct','bAccPas','bToPas','gToAD','gToDA','gAccAct','gAccPas','gToPas','heavy'))
print(bsame_act,digits=4)
save(bsame_act,file='bsame_act.RData')

bsame_to <- stan(file='bsame_to.stan',data=nointprior,iter=10,chains = 1, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bTo','bAccAct','bAccPas','gToAD','gToDA','gAccAct','gAccPas','gToPas','heavy'))
bsame_to <- stan(fit=bsame_to,data=nointprior,iter = 3000, chains = 3, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bTo','bAccAct','bAccPas','gToAD','gToDA','gAccAct','gAccPas','gToPas','heavy'))
print(bsame_to,digits=4)
save(bsame_to,file='bsame_to.RData')

bsame_ad <- stan(file='bsame_ad.stan',data=nointprior,iter=10,chains = 1, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAD','bAccAct','bAccPas','bToDA','gToAD','gToDA','gAccAct','gAccPas','gToPas','heavy'))
bsame_ad <- stan(fit=bsame_ad,data=nointprior,iter = 3000, chains = 3, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAD','bAccAct','bAccPas','bToDA','gToAD','gToDA','gAccAct','gAccPas','gToPas','heavy'))
print(bsame_ad,digits=4)
save(bsame_ad,file='bsame_ad.RData')

bgsame_act <- stan(file='bgsame_act.stan',data=nointprior,iter=10,chains = 1, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAct','bAccAct','bAccPas','bToPas','gToAct','gAccAct','gAccPas','gToPas','heavy'))
bgsame_act <- stan(fit=bgsame_act,data=nointprior,iter = 3000, chains = 3, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAct','bAccAct','bAccPas','bToPas','gToAct','gAccAct','gAccPas','gToPas','heavy'))
print(bgsame_act,digits=4)
save(bgsame_act,file='bgsame_act.RData')

bgsame_to <- stan(file='bgsame_to.stan',data=nointprior,iter=10,chains = 1, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bTo','bAccAct','bAccPas','gTo','gAccAct','gAccPas','heavy'))
bgsame_to <- stan(fit=bgsame_to,data=nointprior,iter = 3000, chains = 3, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bTo','bAccAct','bAccPas','gTo','gAccAct','gAccPas','heavy'))
print(bgsame_to,digits=4)
save(bgsame_to,file='bgsame_to.RData')

bgsame_ad <- stan(file='bgsame_ad.stan',data=nointprior,iter=10,chains = 1, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAD','bAccAct','bAccPas','bToDA','gToAD','gToDA','gAccAct','gAccPas','heavy'))
bgsame_ad <- stan(fit=bgsame_ad,data=nointprior,iter = 3000, chains = 3, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAD','bAccAct','bAccPas','bToDA','gToAD','gToDA','gAccAct','gAccPas','heavy'))
print(bgsame_ad,digits=4)
save(bgsame_ad,file='bgsame_ad.RData')



absame_act <- stan(file='absame_act.stan',data=nointprior,iter=10,chains = 1, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAct','bAcc','bToPas','gToAD','gToDA','gAccAct','gAccPas','gToPas','heavy'))
absame_act <- stan(fit=absame_act,data=nointprior,iter = 3000, chains = 3, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAct','bAcc','bToPas','gToAD','gToDA','gAccAct','gAccPas','gToPas','heavy'))
print(absame_act,digits=4)
save(absame_act,file='absame_act.RData')

absame_to <- stan(file='absame_to.stan',data=nointprior,iter=10,chains = 1, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bTo','bAcc','gToAD','gToDA','gAccAct','gAccPas','gToPas','heavy'))
absame_to <- stan(fit=absame_to,data=nointprior,iter = 3000, chains = 3, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bTo','bAcc','gToAD','gToDA','gAccAct','gAccPas','gToPas','heavy'))
print(absame_to,digits=4)
save(absame_to,file='absame_to.RData')

absame_ad <- stan(file='absame_ad.stan',data=nointprior,iter=10,chains = 1, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAD','bAcc','bToDA','gToAD','gToDA','gAccAct','gAccPas','gToPas','heavy'))
absame_ad <- stan(fit=absame_ad,data=nointprior,iter = 3000, chains = 3, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAD','bAcc','bToDA','gToAD','gToDA','gAccAct','gAccPas','gToPas','heavy'))
print(absame_ad,digits=4)
save(absame_ad,file='absame_ad.RData')

abgsame_act <- stan(file='abgsame_act.stan',data=nointprior,iter=10,chains = 1, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAct','bAcc','bToPas','gToAct','gAccAct','gAccPas','gToPas','heavy'))
abgsame_act <- stan(fit=abgsame_act,data=nointprior,iter = 3000, chains = 3, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAct','bAcc','bToPas','gToAct','gAccAct','gAccPas','gToPas','heavy'))
print(abgsame_act,digits=4)
save(abgsame_act,file='abgsame_act.RData')

abtogactsame <- stan(file='abtogactsame.stan',data=nointprior,iter=10,chains = 1, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bTo','bAcc','gToAct','gAccAct','gAccPas','gToPas','heavy'))
abtogactsame <- stan(fit=abtogactsame,data=nointprior,iter = 3000, chains = 3, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bTo','bAcc','gToAct','gAccAct','gAccPas','gToPas','heavy'))
print(abtogactsame,digits=4)
save(abtogactsame,file='abtogactsame.RData')

abactgtosame <- stan(file='abactgtosame.stan',data=nointprior,iter=10,chains = 1, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAct','bAcc','bToPas','gTo','gAccAct','gAccPas','heavy'))
abactgtosame <- stan(fit=abactgtosame,data=nointprior,iter = 3000, chains = 3, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAct','bAcc','bToPas','gTo','gAccAct','gAccPas','heavy'))
print(abactgtosame,digits=4)
save(abactgtosame,file='abactgtosame.RData')

abgsame_to <- stan(file='abgsame_to.stan',data=nointprior,iter=10,chains = 1, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bTo','bAcc','gTo','gAccAct','gAccPas','heavy'))
abgsame_to <- stan(fit=abgsame_to,data=nointprior,iter = 3000, chains = 3, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bTo','bAcc','gTo','gAccAct','gAccPas','heavy'))
print(abgsame_to,digits=4)
save(abgsame_to,file='abgsame_to.RData')

abgsame_ad <- stan(file='abgsame_ad.stan',data=nointprior,iter=10,chains = 1, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAD','bAcc','bToDA','gToAD','gToDA','gAccAct','gAccPas','heavy'))
abgsame_ad <- stan(fit=abgsame_ad,data=nointprior,iter = 3000, chains = 3, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAD','bAcc','bToDA','gToAD','gToDA','gAccAct','gAccPas','heavy'))
print(abgsame_ad,digits=4)
save(abgsame_ad,file='abgsame_ad.RData')


agbsame_act <- stan(file='agbsame_act.stan',data=nointprior,iter=10,chains = 1, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAct','bAcc','bToPas','gToAD','gToDA','gAcc','gToPas','heavy'))
agbsame_act <- stan(fit=agbsame_act,data=nointprior,iter = 3000, chains = 3, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAct','bAcc','bToPas','gToAD','gToDA','gAcc','gToPas','heavy'))
print(agbsame_act,digits=4)
save(agbsame_act,file='agbsame_act.RData')

agbsame_to <- stan(file='agbsame_to.stan',data=nointprior,iter=10,chains = 1, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bTo','bAcc','gToAD','gToDA','gAcc','gToPas','heavy'))
agbsame_to <- stan(fit=agbsame_to,data=nointprior,iter = 3000, chains = 3, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bTo','bAcc','gToAD','gToDA','gAcc','gToPas','heavy'))
print(agbsame_to,digits=4)
save(agbsame_to,file='agbsame_to.RData')

agbsame_ad <- stan(file='agbsame_ad.stan',data=nointprior,iter=10,chains = 1, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAD','bAcc','bToDA','gToAD','gToDA','gAcc','gToPas','heavy'))
agbsame_ad <- stan(fit=agbsame_ad,data=nointprior,iter = 3000, chains = 3, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAD','bAcc','bToDA','gToAD','gToDA','gAcc','gToPas','heavy'))
print(agbsame_ad,digits=4)
save(agbsame_ad,file='agbsame_ad.RData')

agbgsame_act <- stan(file='agbgsame_act.stan',data=nointprior,iter=10,chains = 1, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAct','bAcc','bToPas','gToAct','gAcc','gToPas','heavy'))
agbgsame_act <- stan(fit=agbgsame_act,data=nointprior,iter = 3000, chains = 3, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAct','bAcc','bToPas','gToAct','gAcc','gToPas','heavy'))
print(agbgsame_act,digits=4)
save(agbgsame_act,file='agbgsame_act.RData')

agbgsame_to <- stan(file='agbgsame_to.stan',data=nointprior,iter=10,chains = 1, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bTo','bAcc','gTo','gAcc','heavy'))
agbgsame_to <- stan(fit=agbgsame_to,data=nointprior,iter = 3000, chains = 3, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bTo','bAcc','gTo','gAcc','heavy'))
print(agbgsame_to,digits=4)
save(agbgsame_to,file='agbgsame_to.RData')

agbgsame_ad <- stan(file='agbgsame_ad.stan',data=nointprior,iter=10,chains = 1, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAD','bAcc','bToDA','gToAD','gToDA','gAcc','heavy'))
agbgsame_ad <- stan(fit=agbgsame_ad,data=nointprior,iter = 3000, chains = 3, pars=c('aToAD','aToDA','aAccAct','aAccPas','aToPas','bToAD','bAcc','bToDA','gToAD','gToDA','gAcc','heavy'))
print(agbgsame_ad,digits=4)
save(agbgsame_ad,file='agbgsame_ad.RData')

