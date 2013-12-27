standardize = function(dataMat){
  m = mean(dataMat)
  sd = sd(dataMat)
  zDataMat = (dataMat-m)/sd
  return(zDataMat)
}
load('newdit.RData')

pall<-subset(dit,(Pas=='REC'|Pas=='THEME')&(NVerb=='TELL'|NVerb=='ALLOW'|NVerb=='PROMISE'|NVerb=='GIVE'|NVerb=='TAKE'|NVerb=='BRING'|NVerb=='SEND'|NVerb=='DENY'|NVerb=='THROW'|NVerb=='TEACH'|NVerb=='OLDENG'))
thirt<-subset(pall,YoC<=1350&Pas!='REC')
pall<-as.data.frame(rbind(subset(pall,YoC>1350),thirt))
rm(thirt)
pall$DO<-factor(pall$DO)
pall$IO<-factor(pall$IO)
ball<-subset(pall,IOType=='DP'|IOType=='NoIO')
tall<-subset(pall,IOType!='NoIO'&(NVerb=='PROMISE'|NVerb=='GIVE'|NVerb=='OLDENG'))
basdata<-data.frame(Year=ball$YoC,Value=ball$PasValue,Type=c('BarePassive'),IO=ball$IO,DO=ball$DO)
pasdata<-data.frame(Year=tall$YoC,Value=tall$PPValue,Type=c('ToPassive'),IO=tall$IO,DO=tall$DO)

given<-subset(dit,(NVerb=='PROMISE'|NVerb=='GIVE'|NVerb=='OLDENG')&Pas=='ACT')

given.da<-subset(given,NOrder=='DA')
given.ad<-subset(given,NOrder=='AD')

da <- data.frame(Year=given.da$YoC,Value=given.da$PPValue,Type=c('DA'),IO=given.da$IO,DO=given.da$DO)
ad <- data.frame(Year=given.ad$YoC,Value=given.ad$PPValue,Type=c('AD'),IO=given.ad$IO,DO=given.ad$DO)
joint.data <- as.data.frame(rbind(ad,da,basdata,pasdata))
joint.data$Cond<-joint.data$Type

levels(joint.data$Cond)<-c('1','2','3','4')
joint.data$Cond<-as.numeric(as.character(joint.data$Cond))

joint.data$IO <- factor(joint.data$IO)
joint.data$DO <- factor(joint.data$DO)

joint.data$iosum <- factor(joint.data$IO)
levels(joint.data$iosum) <- c(-1,1)
joint.data$iosum<-as.numeric(as.character(joint.data$iosum))

joint.data$dosum <- factor(joint.data$DO)
levels(joint.data$dosum) <- c(-1,1)
joint.data$dosum<-as.numeric(as.character(joint.data$dosum))

joint.data$iopro <- factor(joint.data$IO)
levels(joint.data$iopro) <- c(0,1)
joint.data$iopro<-as.numeric(as.character(joint.data$iopro))

joint.data$donoun <- factor(joint.data$DO)
levels(joint.data$donoun) <- c(1,0)
joint.data$donoun<-as.numeric(as.character(joint.data$donoun))

joint.data$dopro <- factor(joint.data$DO)
levels(joint.data$dopro) <- c(0,1)
joint.data$dopro<-as.numeric(as.character(joint.data$dopro))


joint.data$iodo <- joint.data$iopro*joint.data$dopro

real.data<-data.frame(x=standardize(joint.data$Year),y=joint.data$Value,cond=joint.data$Cond,iosum=joint.data$iosum,iopro=joint.data$iopro,donoun=joint.data$donoun,iodo=joint.data$iodo)


modToAD <- glm(data=subset(real.data,cond==1),y~x*iosum,family=binomial)
modAccPas <- glm(data=subset(real.data,cond==3),y~x*iosum,family=binomial)
modToPas <- glm(data=subset(real.data,cond==4),y~x*iosum,family=binomial)

prior <- list(N = length(real.data$x),y=real.data$y,x=real.data$x,cond=real.data$cond,iosum=real.data$iosum,iopro=real.data$iopro,donoun=real.data$donoun,iodo=real.data$iodo,
	      aMuToAD = summary(modToAD)$coe[1,1],aMuToDA=summary(modToAD)$coe[1,1],
	      aMuAccAct=summary(modAccPas)$coe[1,1],aMuAccPas=summary(modAccPas)$coe[1,1],
	      aMuToPas = summary(modToPas)$coe[1,1],
              aSigmaToAD = summary(modToAD)$coe[1,2]*2,aSigmaToDA=summary(modToAD)$coe[1,2]*3,
              aSigmaAccAct=summary(modAccPas)$coe[1,2]*3,aSigmaAccPas=summary(modAccPas)$coe[1,2]*2,
              aSigmaToPas = summary(modToPas)$coe[1,2]*2,
              bMuToAD = summary(modToAD)$coe[2,1],bMuToDA=summary(modToAD)$coe[2,1],
              bMuAccAct=summary(modAccPas)$coe[2,1],bMuAccPas=summary(modAccPas)$coe[2,1],
              bMuToPas = summary(modToPas)$coe[2,1],
              bSigmaToAD = summary(modToAD)$coe[2,2]*2,bSigmaToDA=summary(modToAD)$coe[2,2]*3,
              bSigmaAccAct=summary(modAccPas)$coe[2,2]*3,bSigmaAccPas=summary(modAccPas)$coe[2,2]*2,
              bSigmaToPas = summary(modToPas)$coe[2,2]*3,
              gMuToAD = summary(modToAD)$coe[3,1],gMuToDA=summary(modToAD)$coe[3,1],
              gMuAccAct=summary(modAccPas)$coe[3,1],gMuAccPas=summary(modAccPas)$coe[3,1],
              gMuToPas = summary(modToPas)$coe[3,1],
              gSigmaToAD = summary(modToAD)$coe[3,2]*2,gSigmaToDA=summary(modToAD)$coe[3,2]*3,
              gSigmaAccAct=summary(modAccPas)$coe[3,2]*3,gSigmaAccPas=summary(modAccPas)$coe[3,2]*2,
              gSigmaToPas = summary(modToPas)$coe[3,2]*2)

sepprior <- c(prior, list(
              dMuToAD = summary(modToAD)$coe[4,1],dMuToDA=summary(modToAD)$coe[4,1],
              dMuAccAct=summary(modAccPas)$coe[4,1],dMuAccPas=summary(modAccPas)$coe[4,1],
              dMuToPas = summary(modToPas)$coe[4,1],
              dSigmaToAD = summary(modToAD)$coe[4,2]*2,dSigmaToDA=summary(modToAD)$coe[4,2]*3,
              dSigmaAccAct=summary(modAccPas)$coe[4,2]*3,dSigmaAccPas=summary(modAccPas)$coe[4,2]*2,
              dSigmaToPas = summary(modToPas)$coe[4,2]*2,
	      orderMu = .5, orderSigma = .25, heavyMu = .5, heavySigma = .3
	     ))

Order1500 <- margin.table(table(subset(joint.data,(Type=='AD'|Type=='DA')&Year>1500&IO=='IONoun'&DO=='DONoun')$Type))
AD1500 <- margin.table(table(subset(joint.data,Type=='AD'&Year>1500&IO=='IONoun'&DO=='DONoun')$Type))

orderprior <- c(prior, list(
              dMuToAD = summary(modToAD)$coe[4,1],dMuToDA=summary(modToAD)$coe[4,1],
              dMuAccAct=summary(modAccPas)$coe[4,1],dMuAccPas=summary(modAccPas)$coe[4,1],
              dMuToPas = summary(modToPas)$coe[4,1],
              dSigmaToAD = summary(modToAD)$coe[4,2]*2,dSigmaToDA=summary(modToAD)$coe[4,2]*3,
              dSigmaAccAct=summary(modAccPas)$coe[4,2]*3,dSigmaAccPas=summary(modAccPas)$coe[4,2]*2,
              dSigmaToPas = summary(modToPas)$coe[4,2]*2,
              orderMu = AD1500/Order1500, orderSigma = .025, heavyMu = .9, heavySigma = .1
             ))

nointprior <- c(prior, list(
               order = AD1500/Order1500, heavyMu = .9, heavySigma = .1
             ))
save(sepprior,file='sepprior.RData')
save(orderprior,file='ordprior.RData')
save(nointprior,file='noiprior.RData')

