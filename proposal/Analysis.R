library(plyr)
library(ggplot2)
library(rstan)
load('~/Git/ditransitive-scripts/proposal/newdit.RData')

standardize = function(dataMat){
  m = mean(dataMat)
  sd = sd(dataMat)
  zDataMat = (dataMat-m)/sd
  return(zDataMat)
}

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

joint.data$Bin<-cut(joint.data$Year,seq(650,1950,100),seq(700,1900,100))
joint.data$Bin<-as.numeric(as.character(joint.data$Bin))

new.data.1<-ddply(subset(joint.data,Cond==1),.(Type,Bin,IO,DO),summarize,new.val=sum(Value)/sum(!is.na(Value)),n=sum(!is.na(Value)))
new.data.2<-ddply(subset(joint.data,Cond==2),.(Type,Bin,IO,DO),summarize,new.val=sum(Value)/sum(!is.na(Value)),n=sum(!is.na(Value)))
new.data.3<-ddply(subset(joint.data,Cond==3),.(Type,Bin,IO,DO),summarize,new.val=sum(Value)/sum(!is.na(Value)),n=sum(!is.na(Value)))
new.data.3$Type<-factor(c('BarePassive'))
new.data.4<-ddply(subset(joint.data,Cond==4),.(Type,Bin,IO,DO),summarize,new.val=sum(Value)/sum(!is.na(Value)),n=sum(!is.na(Value)))
new.data.4$Type<-factor(c('ToPassive'))
new.data<-as.data.frame(rbind(new.data.1,new.data.2,new.data.3,new.data.4))

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

real.data<-data.frame(Year=joint.data$Year,x=standardize(joint.data$Year),y=joint.data$Value,cond=joint.data$Cond,iosum=joint.data$iosum,iopro=joint.data$iopro,donoun=joint.data$donoun,iodo=joint.data$iodo)

data.1<-subset(real.data,cond==1)
data.2<-subset(real.data,cond==2&donoun==1)
data.3<-subset(real.data,cond==3&donoun==1)
data.4<-subset(real.data,cond==4)

load('order.RData')
orderd<-as.data.frame(order)


L1<-data.frame(yhat.order= ((1 - ((1 - mean(orderd$order)) * data.1$iodo))/ (1 + exp(-(mean(orderd$aToAD) + mean(orderd$bToAD) * data.1$x + mean(orderd$gToAD) * data.1$iosum + mean(orderd$dToAD) * data.1$iosum * data.1$x)))))
L1$phat.order=((L1$yhat.order)^(data.1$y))*((1-L1$yhat.order)^(1-data.1$y))

L2<-data.frame(yhat.order= (((1 - ((1 - mean(orderd$order)) * data.2$iopro)) * data.2$donoun)/ (1 + exp(-(mean(orderd$aToDA) + mean(orderd$bToDA) * data.2$x + mean(orderd$gToDA) * data.2$iosum + mean(orderd$dToDA) * data.2$iosum * data.2$x)))) * (1 - (mean(orderd$heavy) / (1 + exp(-(mean(orderd$aAccAct) + mean(orderd$bAccAct) * data.2$x + mean(orderd$gAccAct) * data.2$iosum + mean(orderd$dAccAct) * data.2$iosum * data.2$x))))))
L2$phat.order=((L2$yhat.order)^(data.2$y))*((1-L2$yhat.order)^(1-data.2$y))

L3<-data.frame(yhat.order= (data.3$donoun / (1 + exp(-(mean(orderd$aAccPas) + mean(orderd$bAccPas) * data.3$x + mean(orderd$gAccPas) * data.3$iosum + mean(orderd$dAccPas) * data.3$iosum * data.3$x)))))
L3$phat.order=((L3$yhat.order)^(data.3$y))*((1-L3$yhat.order)^(1-data.3$y))

L4<-data.frame(yhat.order= ((1 - ((1 - mean(orderd$order)) * data.4$iopro))/ (1 + exp(-(mean(orderd$aToPas) + mean(orderd$bToPas) * data.4$x + mean(orderd$gToPas) * data.4$iosum + mean(orderd$dToPas) * data.4$iosum * data.4$x)))))
L4$phat.order=((L4$yhat.order)^(data.4$y))*((1-L4$yhat.order)^(1-data.4$y))

Order1500 <- margin.table(table(subset(joint.data,(Type=='AD'|Type=='DA')&Year>1500&IO=='IONoun'&DO=='DONoun')$Type))
AD1500 <- margin.table(table(subset(joint.data,Type=='AD'&Year>1500&IO=='IONoun'&DO=='DONoun')$Type))
ord = AD1500/Order1500

load('noint.RData')
noid<-as.data.frame(noint)

L1$yhat.noint= ((1 - ((1 - ord) * data.1$iodo))/ (1 + exp(-(mean(noid$aToAD) + mean(noid$bToAD) * data.1$x + mean(noid$gToAD) * data.1$iosum))))
L1$phat.noint=((L1$yhat.noint)^(data.1$y))*((1-L1$yhat.noint)^(1-data.1$y))

L2$yhat.noint= (((1 - ((1 - ord) * data.2$iopro)) * data.2$donoun)/ (1 + exp(-(mean(noid$aToDA) + mean(noid$bToDA) * data.2$x + mean(noid$gToDA) * data.2$iosum)))) * (1 - (mean(noid$heavy) / (1 + exp(-(mean(noid$aAccAct) + mean(noid$bAccAct) * data.2$x + mean(noid$gAccAct) * data.2$iosum)))))
L2$phat.noint=((L2$yhat.noint)^(data.2$y))*((1-L2$yhat.noint)^(1-data.2$y))

L3$yhat.noint= (data.3$donoun / (1 + exp(-(mean(noid$aAccPas) + mean(noid$bAccPas) * data.3$x + mean(noid$gAccPas) * data.3$iosum))))
L3$phat.noint=((L3$yhat.noint)^(data.3$y))*((1-L3$yhat.noint)^(1-data.3$y))

L4$yhat.noint= ((1 - ((1 - ord) * data.4$iopro))/ (1 + exp(-(mean(noid$aToPas) + mean(noid$bToPas) * data.4$x + mean(noid$gToPas) * data.4$iosum))))
L4$phat.noint=((L4$yhat.noint)^(data.4$y))*((1-L4$yhat.noint)^(1-data.4$y))


load('asame.RData')
asamed<-as.data.frame(asame)
samed<-as.data.frame(asame)

L1$yhat.asame= ((1 - ((1 - ord) * data.1$iodo))/ (1 + exp(-(mean(samed$aToAD) + mean(samed$bToAD) * data.1$x + mean(samed$gToAD) * data.1$iosum))))
L1$phat.asame=((L1$yhat.asame)^(data.1$y))*((1-L1$yhat.asame)^(1-data.1$y))

L2$yhat.asame= (((1 - ((1 - ord) * data.2$iopro)) * data.2$donoun)/ (1 + exp(-(mean(samed$aToDA) + mean(samed$bToDA) * data.2$x + mean(samed$gToDA) * data.2$iosum)))) * (1 - (mean(samed$heavy) / (1 + exp(-(mean(samed$aAccAct) + mean(samed$bAcc) * data.2$x + mean(samed$gAccAct) * data.2$iosum)))))
L2$phat.asame=((L2$yhat.asame)^(data.2$y))*((1-L2$yhat.asame)^(1-data.2$y))

L3$yhat.asame= (data.3$donoun / (1 + exp(-(mean(samed$aAccPas) + mean(samed$bAcc) * data.3$x + mean(samed$gAccPas) * data.3$iosum))))
L3$phat.asame=((L3$yhat.asame)^(data.3$y))*((1-L3$yhat.asame)^(1-data.3$y))

L4$yhat.asame= ((1 - ((1 - ord) * data.4$iopro))/ (1 + exp(-(mean(samed$aToPas) + mean(samed$bToPas) * data.4$x + mean(samed$gToPas) * data.4$iosum))))
L4$phat.asame=((L4$yhat.asame)^(data.4$y))*((1-L4$yhat.asame)^(1-data.4$y))


load('agsame.RData')
agsamed<-as.data.frame(agsame)
samed<-as.data.frame(agsame)

L1$yhat.agsame= ((1 - ((1 - ord) * data.1$iodo))/ (1 + exp(-(mean(samed$aToAD) + mean(samed$bToAD) * data.1$x + mean(samed$gToAD) * data.1$iosum))))
L1$phat.agsame=((L1$yhat.agsame)^(data.1$y))*((1-L1$yhat.agsame)^(1-data.1$y))

L2$yhat.agsame= (((1 - ((1 - ord) * data.2$iopro)) * data.2$donoun)/ (1 + exp(-(mean(samed$aToDA) + mean(samed$bToDA) * data.2$x + mean(samed$gToDA) * data.2$iosum)))) * (1 - (mean(samed$heavy) / (1 + exp(-(mean(samed$aAccAct) + mean(samed$bAcc) * data.2$x + mean(samed$gAcc) * data.2$iosum)))))
L2$phat.agsame=((L2$yhat.agsame)^(data.2$y))*((1-L2$yhat.agsame)^(1-data.2$y))

L3$yhat.agsame= (data.3$donoun / (1 + exp(-(mean(samed$aAccPas) + mean(samed$bAcc) * data.3$x + mean(samed$gAcc) * data.3$iosum))))
L3$phat.agsame=((L3$yhat.agsame)^(data.3$y))*((1-L3$yhat.agsame)^(1-data.3$y))

L4$yhat.agsame= ((1 - ((1 - ord) * data.4$iopro))/ (1 + exp(-(mean(samed$aToPas) + mean(samed$bToPas) * data.4$x + mean(samed$gToPas) * data.4$iosum))))
L4$phat.agsame=((L4$yhat.agsame)^(data.4$y))*((1-L4$yhat.agsame)^(1-data.4$y))

load('bsame_act.RData')
bsamed_act<-as.data.frame(bsame_act)
samed<-as.data.frame(bsame_act)

L1$yhat.bsame_act= ((1 - ((1 - ord) * data.1$iodo))/ (1 + exp(-(mean(samed$aToAD) + mean(samed$bToAct) * data.1$x + mean(samed$gToAD) * data.1$iosum))))
L1$phat.bsame_act=((L1$yhat.bsame_act)^(data.1$y))*((1-L1$yhat.bsame_act)^(1-data.1$y))

L2$yhat.bsame_act= (((1 - ((1 - ord) * data.2$iopro)) * data.2$donoun)/ (1 + exp(-(mean(samed$aToDA) + mean(samed$bToAct) * data.2$x + mean(samed$gToDA) * data.2$iosum)))) * (1 - (mean(samed$heavy) / (1 + exp(-(mean(samed$aAccAct) + mean(samed$bAccAct) * data.2$x + mean(samed$gAccAct) * data.2$iosum)))))
L2$phat.bsame_act=((L2$yhat.bsame_act)^(data.2$y))*((1-L2$yhat.bsame_act)^(1-data.2$y))

L3$yhat.bsame_act= (data.3$donoun / (1 + exp(-(mean(samed$aAccPas) + mean(samed$bAccPas) * data.3$x + mean(samed$gAccPas) * data.3$iosum))))
L3$phat.bsame_act=((L3$yhat.bsame_act)^(data.3$y))*((1-L3$yhat.bsame_act)^(1-data.3$y))

L4$yhat.bsame_act= ((1 - ((1 - ord) * data.4$iopro))/ (1 + exp(-(mean(samed$aToPas) + mean(samed$bToPas) * data.4$x + mean(samed$gToPas) * data.4$iosum))))
L4$phat.bsame_act=((L4$yhat.bsame_act)^(data.4$y))*((1-L4$yhat.bsame_act)^(1-data.4$y))

load('bsame_to.RData')
bsamed_to<-as.data.frame(bsame_to)
samed<-as.data.frame(bsame_to)

L1$yhat.bsame_to= ((1 - ((1 - ord) * data.1$iodo))/ (1 + exp(-(mean(samed$aToAD) + mean(samed$bTo) * data.1$x + mean(samed$gToAD) * data.1$iosum))))
L1$phat.bsame_to=((L1$yhat.bsame_to)^(data.1$y))*((1-L1$yhat.bsame_to)^(1-data.1$y))

L2$yhat.bsame_to= (((1 - ((1 - ord) * data.2$iopro)) * data.2$donoun)/ (1 + exp(-(mean(samed$aToDA) + mean(samed$bTo) * data.2$x + mean(samed$gToDA) * data.2$iosum)))) * (1 - (mean(samed$heavy) / (1 + exp(-(mean(samed$aAccAct) + mean(samed$bAccAct) * data.2$x + mean(samed$gAccAct) * data.2$iosum)))))
L2$phat.bsame_to=((L2$yhat.bsame_to)^(data.2$y))*((1-L2$yhat.bsame_to)^(1-data.2$y))

L3$yhat.bsame_to= (data.3$donoun / (1 + exp(-(mean(samed$aAccPas) + mean(samed$bAccPas) * data.3$x + mean(samed$gAccPas) * data.3$iosum))))
L3$phat.bsame_to=((L3$yhat.bsame_to)^(data.3$y))*((1-L3$yhat.bsame_to)^(1-data.3$y))

L4$yhat.bsame_to= ((1 - ((1 - ord) * data.4$iopro))/ (1 + exp(-(mean(samed$aToPas) + mean(samed$bTo) * data.4$x + mean(samed$gToPas) * data.4$iosum))))
L4$phat.bsame_to=((L4$yhat.bsame_to)^(data.4$y))*((1-L4$yhat.bsame_to)^(1-data.4$y))

load('bsame_ad.RData')
bsamed_ad<-as.data.frame(bsame_ad)
samed<-as.data.frame(bsame_ad)

L1$yhat.bsame_ad= ((1 - ((1 - ord) * data.1$iodo))/ (1 + exp(-(mean(samed$aToAD) + mean(samed$bToAD) * data.1$x + mean(samed$gToAD) * data.1$iosum))))
L1$phat.bsame_ad=((L1$yhat.bsame_ad)^(data.1$y))*((1-L1$yhat.bsame_ad)^(1-data.1$y))

L2$yhat.bsame_ad= (((1 - ((1 - ord) * data.2$iopro)) * data.2$donoun)/ (1 + exp(-(mean(samed$aToDA) + mean(samed$bToDA) * data.2$x + mean(samed$gToDA) * data.2$iosum)))) * (1 - (mean(samed$heavy) / (1 + exp(-(mean(samed$aAccAct) + mean(samed$bAccAct) * data.2$x + mean(samed$gAccAct) * data.2$iosum)))))
L2$phat.bsame_ad=((L2$yhat.bsame_ad)^(data.2$y))*((1-L2$yhat.bsame_ad)^(1-data.2$y))

L3$yhat.bsame_ad= (data.3$donoun / (1 + exp(-(mean(samed$aAccPas) + mean(samed$bAccPas) * data.3$x + mean(samed$gAccPas) * data.3$iosum))))
L3$phat.bsame_ad=((L3$yhat.bsame_ad)^(data.3$y))*((1-L3$yhat.bsame_ad)^(1-data.3$y))

L4$yhat.bsame_ad= ((1 - ((1 - ord) * data.4$iopro))/ (1 + exp(-(mean(samed$aToPas) + mean(samed$bToAD) * data.4$x + mean(samed$gToPas) * data.4$iosum))))
L4$phat.bsame_ad=((L4$yhat.bsame_ad)^(data.4$y))*((1-L4$yhat.bsame_ad)^(1-data.4$y))



load('bgsame_act.RData')
bgsamed_act<-as.data.frame(bgsame_act)
samed<-as.data.frame(bgsame_act)

L1$yhat.bgsame_act= ((1 - ((1 - ord) * data.1$iodo))/ (1 + exp(-(mean(samed$aToAD) + mean(samed$bToAct) * data.1$x + mean(samed$gToAct) * data.1$iosum))))
L1$phat.bgsame_act=((L1$yhat.bgsame_act)^(data.1$y))*((1-L1$yhat.bgsame_act)^(1-data.1$y))

L2$yhat.bgsame_act= (((1 - ((1 - ord) * data.2$iopro)) * data.2$donoun)/ (1 + exp(-(mean(samed$aToDA) + mean(samed$bToAct) * data.2$x + mean(samed$gToAct) * data.2$iosum)))) * (1 - (mean(samed$heavy) / (1 + exp(-(mean(samed$aAccAct) + mean(samed$bAccAct) * data.2$x + mean(samed$gAccAct) * data.2$iosum)))))
L2$phat.bgsame_act=((L2$yhat.bgsame_act)^(data.2$y))*((1-L2$yhat.bgsame_act)^(1-data.2$y))

L3$yhat.bgsame_act= (data.3$donoun / (1 + exp(-(mean(samed$aAccPas) + mean(samed$bAccPas) * data.3$x + mean(samed$gAccPas) * data.3$iosum))))
L3$phat.bgsame_act=((L3$yhat.bgsame_act)^(data.3$y))*((1-L3$yhat.bgsame_act)^(1-data.3$y))

L4$yhat.bgsame_act= ((1 - ((1 - ord) * data.4$iopro))/ (1 + exp(-(mean(samed$aToPas) + mean(samed$bToPas) * data.4$x + mean(samed$gToPas) * data.4$iosum))))
L4$phat.bgsame_act=((L4$yhat.bgsame_act)^(data.4$y))*((1-L4$yhat.bgsame_act)^(1-data.4$y))

load('bgsame_to.RData')
bgsamed_to<-as.data.frame(bgsame_to)
samed<-as.data.frame(bgsame_to)

L1$yhat.bgsame_to= ((1 - ((1 - ord) * data.1$iodo))/ (1 + exp(-(mean(samed$aToAD) + mean(samed$bTo) * data.1$x + mean(samed$gTo) * data.1$iosum))))
L1$phat.bgsame_to=((L1$yhat.bgsame_to)^(data.1$y))*((1-L1$yhat.bgsame_to)^(1-data.1$y))

L2$yhat.bgsame_to= (((1 - ((1 - ord) * data.2$iopro)) * data.2$donoun)/ (1 + exp(-(mean(samed$aToDA) + mean(samed$bTo) * data.2$x + mean(samed$gTo) * data.2$iosum)))) * (1 - (mean(samed$heavy) / (1 + exp(-(mean(samed$aAccAct) + mean(samed$bAccAct) * data.2$x + mean(samed$gAccAct) * data.2$iosum)))))
L2$phat.bgsame_to=((L2$yhat.bgsame_to)^(data.2$y))*((1-L2$yhat.bgsame_to)^(1-data.2$y))
 
L3$yhat.bgsame_to= (data.3$donoun / (1 + exp(-(mean(samed$aAccPas) + mean(samed$bAccPas) * data.3$x + mean(samed$gAccPas) * data.3$iosum))))
L3$phat.bgsame_to=((L3$yhat.bgsame_to)^(data.3$y))*((1-L3$yhat.bgsame_to)^(1-data.3$y))

L4$yhat.bgsame_to= ((1 - ((1 - ord) * data.4$iopro))/ (1 + exp(-(mean(samed$aToPas) + mean(samed$bTo) * data.4$x + mean(samed$gTo) * data.4$iosum))))
L4$phat.bgsame_to=((L4$yhat.bgsame_to)^(data.4$y))*((1-L4$yhat.bgsame_to)^(1-data.4$y))

load('bgsame_ad.RData')
bgsamed_ad<-as.data.frame(bgsame_ad)
samed<-as.data.frame(bgsame_ad)

L1$yhat.bgsame_ad= ((1 - ((1 - ord) * data.1$iodo))/ (1 + exp(-(mean(samed$aToAD) + mean(samed$bToAD) * data.1$x + mean(samed$gToAD) * data.1$iosum))))
L1$phat.bgsame_ad=((L1$yhat.bgsame_ad)^(data.1$y))*((1-L1$yhat.bgsame_ad)^(1-data.1$y))

L2$yhat.bgsame_ad= (((1 - ((1 - ord) * data.2$iopro)) * data.2$donoun)/ (1 + exp(-(mean(samed$aToDA) + mean(samed$bToDA) * data.2$x + mean(samed$gToDA) * data.2$iosum)))) * (1 - (mean(samed$heavy) / (1 + exp(-(mean(samed$aAccAct) + mean(samed$bAccAct) * data.2$x + mean(samed$gAccAct) * data.2$iosum)))))
L2$phat.bgsame_ad=((L2$yhat.bgsame_ad)^(data.2$y))*((1-L2$yhat.bgsame_ad)^(1-data.2$y))

L3$yhat.bgsame_ad= (data.3$donoun / (1 + exp(-(mean(samed$aAccPas) + mean(samed$bAccPas) * data.3$x + mean(samed$gAccPas) * data.3$iosum))))
L3$phat.bgsame_ad=((L3$yhat.bgsame_ad)^(data.3$y))*((1-L3$yhat.bgsame_ad)^(1-data.3$y))

L4$yhat.bgsame_ad= ((1 - ((1 - ord) * data.4$iopro))/ (1 + exp(-(mean(samed$aToPas) + mean(samed$bToAD) * data.4$x + mean(samed$gToAD) * data.4$iosum))))
L4$phat.bgsame_ad=((L4$yhat.bgsame_ad)^(data.4$y))*((1-L4$yhat.bgsame_ad)^(1-data.4$y))


load('absame_act.RData')
absamed_act<-as.data.frame(absame_act)
samed<-as.data.frame(absame_act)

L1$yhat.absame_act= ((1 - ((1 - ord) * data.1$iodo))/ (1 + exp(-(mean(samed$aToAD) + mean(samed$bToAct) * data.1$x + mean(samed$gToAD) * data.1$iosum))))
L1$phat.absame_act=((L1$yhat.absame_act)^(data.1$y))*((1-L1$yhat.absame_act)^(1-data.1$y))

L2$yhat.absame_act= (((1 - ((1 - ord) * data.2$iopro)) * data.2$donoun)/ (1 + exp(-(mean(samed$aToDA) + mean(samed$bToAct) * data.2$x + mean(samed$gToDA) * data.2$iosum)))) * (1 - (mean(samed$heavy) / (1 + exp(-(mean(samed$aAccAct) + mean(samed$bAcc) * data.2$x + mean(samed$gAccAct) * data.2$iosum)))))
L2$phat.absame_act=((L2$yhat.absame_act)^(data.2$y))*((1-L2$yhat.absame_act)^(1-data.2$y))

L3$yhat.absame_act= (data.3$donoun / (1 + exp(-(mean(samed$aAccPas) + mean(samed$bAcc) * data.3$x + mean(samed$gAccPas) * data.3$iosum))))
L3$phat.absame_act=((L3$yhat.absame_act)^(data.3$y))*((1-L3$yhat.absame_act)^(1-data.3$y))

L4$yhat.absame_act= ((1 - ((1 - ord) * data.4$iopro))/ (1 + exp(-(mean(samed$aToPas) + mean(samed$bToPas) * data.4$x + mean(samed$gToPas) * data.4$iosum))))
L4$phat.absame_act=((L4$yhat.absame_act)^(data.4$y))*((1-L4$yhat.absame_act)^(1-data.4$y))

load('absame_to.RData')
absamed_to<-as.data.frame(absame_to)
samed<-as.data.frame(absame_to)

L1$yhat.absame_to= ((1 - ((1 - ord) * data.1$iodo))/ (1 + exp(-(mean(samed$aToAD) + mean(samed$bTo) * data.1$x + mean(samed$gToAD) * data.1$iosum))))
L1$phat.absame_to=((L1$yhat.absame_to)^(data.1$y))*((1-L1$yhat.absame_to)^(1-data.1$y))

L2$yhat.absame_to= (((1 - ((1 - ord) * data.2$iopro)) * data.2$donoun)/ (1 + exp(-(mean(samed$aToDA) + mean(samed$bTo) * data.2$x + mean(samed$gToDA) * data.2$iosum)))) * (1 - (mean(samed$heavy) / (1 + exp(-(mean(samed$aAccAct) + mean(samed$bAcc) * data.2$x + mean(samed$gAccAct) * data.2$iosum)))))
L2$phat.absame_to=((L2$yhat.absame_to)^(data.2$y))*((1-L2$yhat.absame_to)^(1-data.2$y))

L3$yhat.absame_to= (data.3$donoun / (1 + exp(-(mean(samed$aAccPas) + mean(samed$bAcc) * data.3$x + mean(samed$gAccPas) * data.3$iosum))))
L3$phat.absame_to=((L3$yhat.absame_to)^(data.3$y))*((1-L3$yhat.absame_to)^(1-data.3$y))

L4$yhat.absame_to= ((1 - ((1 - ord) * data.4$iopro))/ (1 + exp(-(mean(samed$aToPas) + mean(samed$bTo) * data.4$x + mean(samed$gToPas) * data.4$iosum))))
L4$phat.absame_to=((L4$yhat.absame_to)^(data.4$y))*((1-L4$yhat.absame_to)^(1-data.4$y))

load('absame_ad.RData')
absamed_ad<-as.data.frame(absame_ad)
samed<-as.data.frame(absame_ad)

L1$yhat.absame_ad= ((1 - ((1 - ord) * data.1$iodo))/ (1 + exp(-(mean(samed$aToAD) + mean(samed$bToAD) * data.1$x + mean(samed$gToAD) * data.1$iosum))))
L1$phat.absame_ad=((L1$yhat.absame_ad)^(data.1$y))*((1-L1$yhat.absame_ad)^(1-data.1$y))

L2$yhat.absame_ad= (((1 - ((1 - ord) * data.2$iopro)) * data.2$donoun)/ (1 + exp(-(mean(samed$aToDA) + mean(samed$bToDA) * data.2$x + mean(samed$gToDA) * data.2$iosum)))) * (1 - (mean(samed$heavy) / (1 + exp(-(mean(samed$aAccAct) + mean(samed$bAcc) * data.2$x + mean(samed$gAccAct) * data.2$iosum)))))
L2$phat.absame_ad=((L2$yhat.absame_ad)^(data.2$y))*((1-L2$yhat.absame_ad)^(1-data.2$y))

L3$yhat.absame_ad= (data.3$donoun / (1 + exp(-(mean(samed$aAccPas) + mean(samed$bAcc) * data.3$x + mean(samed$gAccPas) * data.3$iosum))))
L3$phat.absame_ad=((L3$yhat.absame_ad)^(data.3$y))*((1-L3$yhat.absame_ad)^(1-data.3$y))

L4$yhat.absame_ad= ((1 - ((1 - ord) * data.4$iopro))/ (1 + exp(-(mean(samed$aToPas) + mean(samed$bToAD) * data.4$x + mean(samed$gToPas) * data.4$iosum))))
L4$phat.absame_ad=((L4$yhat.absame_ad)^(data.4$y))*((1-L4$yhat.absame_ad)^(1-data.4$y))

load('abgsame_act.RData')
abgsamed_act<-as.data.frame(abgsame_act)
samed<-as.data.frame(abgsame_act)

L1$yhat.abgsame_act= ((1 - ((1 - ord) * data.1$iodo))/ (1 + exp(-(mean(samed$aToAD) + mean(samed$bToAct) * data.1$x + mean(samed$gToAct) * data.1$iosum))))
L1$phat.abgsame_act=((L1$yhat.abgsame_act)^(data.1$y))*((1-L1$yhat.abgsame_act)^(1-data.1$y))

L2$yhat.abgsame_act= (((1 - ((1 - ord) * data.2$iopro)) * data.2$donoun)/ (1 + exp(-(mean(samed$aToDA) + mean(samed$bToAct) * data.2$x + mean(samed$gToAct) * data.2$iosum)))) * (1 - (mean(samed$heavy) / (1 + exp(-(mean(samed$aAccAct) + mean(samed$bAcc) * data.2$x + mean(samed$gAccAct) * data.2$iosum)))))
L2$phat.abgsame_act=((L2$yhat.abgsame_act)^(data.2$y))*((1-L2$yhat.abgsame_act)^(1-data.2$y))

L3$yhat.abgsame_act= (data.3$donoun / (1 + exp(-(mean(samed$aAccPas) + mean(samed$bAcc) * data.3$x + mean(samed$gAccPas) * data.3$iosum))))
L3$phat.abgsame_act=((L3$yhat.abgsame_act)^(data.3$y))*((1-L3$yhat.abgsame_act)^(1-data.3$y))

L4$yhat.abgsame_act= ((1 - ((1 - ord) * data.4$iopro))/ (1 + exp(-(mean(samed$aToPas) + mean(samed$bToPas) * data.4$x + mean(samed$gToPas) * data.4$iosum))))
L4$phat.abgsame_act=((L4$yhat.abgsame_act)^(data.4$y))*((1-L4$yhat.abgsame_act)^(1-data.4$y))

load('abgsame_to.RData')
abgsamed_to<-as.data.frame(abgsame_to)
samed<-as.data.frame(abgsame_to)

L1$yhat.abgsame_to= ((1 - ((1 - ord) * data.1$iodo))/ (1 + exp(-(mean(samed$aToAD) + mean(samed$bTo) * data.1$x + mean(samed$gTo) * data.1$iosum))))
L1$phat.abgsame_to=((L1$yhat.abgsame_to)^(data.1$y))*((1-L1$yhat.abgsame_to)^(1-data.1$y))

L2$yhat.abgsame_to= (((1 - ((1 - ord) * data.2$iopro)) * data.2$donoun)/ (1 + exp(-(mean(samed$aToDA) + mean(samed$bTo) * data.2$x + mean(samed$gTo) * data.2$iosum)))) * (1 - (mean(samed$heavy) / (1 + exp(-(mean(samed$aAccAct) + mean(samed$bAcc) * data.2$x + mean(samed$gAccAct) * data.2$iosum)))))
L2$phat.abgsame_to=((L2$yhat.abgsame_to)^(data.2$y))*((1-L2$yhat.abgsame_to)^(1-data.2$y))

L3$yhat.abgsame_to= (data.3$donoun / (1 + exp(-(mean(samed$aAccPas) + mean(samed$bAcc) * data.3$x + mean(samed$gAccPas) * data.3$iosum))))
L3$phat.abgsame_to=((L3$yhat.abgsame_to)^(data.3$y))*((1-L3$yhat.abgsame_to)^(1-data.3$y))

L4$yhat.abgsame_to= ((1 - ((1 - ord) * data.4$iopro))/ (1 + exp(-(mean(samed$aToPas) + mean(samed$bTo) * data.4$x + mean(samed$gTo) * data.4$iosum))))
L4$phat.abgsame_to=((L4$yhat.abgsame_to)^(data.4$y))*((1-L4$yhat.abgsame_to)^(1-data.4$y))

load('abgsame_ad.RData')
abgsamed_ad<-as.data.frame(abgsame_ad)
samed<-as.data.frame(abgsame_ad)

L1$yhat.abgsame_ad= ((1 - ((1 - ord) * data.1$iodo))/ (1 + exp(-(mean(samed$aToAD) + mean(samed$bToAD) * data.1$x + mean(samed$gToAD) * data.1$iosum))))
L1$phat.abgsame_ad=((L1$yhat.abgsame_ad)^(data.1$y))*((1-L1$yhat.abgsame_ad)^(1-data.1$y))

L2$yhat.abgsame_ad= (((1 - ((1 - ord) * data.2$iopro)) * data.2$donoun)/ (1 + exp(-(mean(samed$aToDA) + mean(samed$bToDA) * data.2$x + mean(samed$gToDA) * data.2$iosum)))) * (1 - (mean(samed$heavy) / (1 + exp(-(mean(samed$aAccAct) + mean(samed$bAcc) * data.2$x + mean(samed$gAccAct) * data.2$iosum)))))
L2$phat.abgsame_ad=((L2$yhat.abgsame_ad)^(data.2$y))*((1-L2$yhat.abgsame_ad)^(1-data.2$y))

L3$yhat.abgsame_ad= (data.3$donoun / (1 + exp(-(mean(samed$aAccPas) + mean(samed$bAcc) * data.3$x + mean(samed$gAccPas) * data.3$iosum))))
L3$phat.abgsame_ad=((L3$yhat.abgsame_ad)^(data.3$y))*((1-L3$yhat.abgsame_ad)^(1-data.3$y))

L4$yhat.abgsame_ad= ((1 - ((1 - ord) * data.4$iopro))/ (1 + exp(-(mean(samed$aToPas) + mean(samed$bToAD) * data.4$x + mean(samed$gToAD) * data.4$iosum))))
L4$phat.abgsame_ad=((L4$yhat.abgsame_ad)^(data.4$y))*((1-L4$yhat.abgsame_ad)^(1-data.4$y))


load('agbsame_act.RData')
agbsamed_act<-as.data.frame(agbsame_act)
samed<-as.data.frame(agbsame_act)

L1$yhat.agbsame_act= ((1 - ((1 - ord) * data.1$iodo))/ (1 + exp(-(mean(samed$aToAD) + mean(samed$bToAct) * data.1$x + mean(samed$gToAD) * data.1$iosum))))
L1$phat.agbsame_act=((L1$yhat.agbsame_act)^(data.1$y))*((1-L1$yhat.agbsame_act)^(1-data.1$y))

L2$yhat.agbsame_act= (((1 - ((1 - ord) * data.2$iopro)) * data.2$donoun)/ (1 + exp(-(mean(samed$aToDA) + mean(samed$bToAct) * data.2$x + mean(samed$gToDA) * data.2$iosum)))) * (1 - (mean(samed$heavy) / (1 + exp(-(mean(samed$aAccAct) + mean(samed$bAcc) * data.2$x + mean(samed$gAcc) * data.2$iosum)))))
L2$phat.agbsame_act=((L2$yhat.agbsame_act)^(data.2$y))*((1-L2$yhat.agbsame_act)^(1-data.2$y))

L3$yhat.agbsame_act= (data.3$donoun / (1 + exp(-(mean(samed$aAccPas) + mean(samed$bAcc) * data.3$x + mean(samed$gAcc) * data.3$iosum))))
L3$phat.agbsame_act=((L3$yhat.agbsame_act)^(data.3$y))*((1-L3$yhat.agbsame_act)^(1-data.3$y))

L4$yhat.agbsame_act= ((1 - ((1 - ord) * data.4$iopro))/ (1 + exp(-(mean(samed$aToPas) + mean(samed$bToPas) * data.4$x + mean(samed$gToPas) * data.4$iosum))))
L4$phat.agbsame_act=((L4$yhat.agbsame_act)^(data.4$y))*((1-L4$yhat.agbsame_act)^(1-data.4$y))

load('agbsame_to.RData')
agbsamed_to<-as.data.frame(agbsame_to)
samed<-as.data.frame(agbsame_to)

L1$yhat.agbsame_to= ((1 - ((1 - ord) * data.1$iodo))/ (1 + exp(-(mean(samed$aToAD) + mean(samed$bTo) * data.1$x + mean(samed$gToAD) * data.1$iosum))))
L1$phat.agbsame_to=((L1$yhat.agbsame_to)^(data.1$y))*((1-L1$yhat.agbsame_to)^(1-data.1$y))

L2$yhat.agbsame_to= (((1 - ((1 - ord) * data.2$iopro)) * data.2$donoun)/ (1 + exp(-(mean(samed$aToDA) + mean(samed$bTo) * data.2$x + mean(samed$gToDA) * data.2$iosum)))) * (1 - (mean(samed$heavy) / (1 + exp(-(mean(samed$aAccAct) + mean(samed$bAcc) * data.2$x + mean(samed$gAcc) * data.2$iosum)))))
L2$phat.agbsame_to=((L2$yhat.agbsame_to)^(data.2$y))*((1-L2$yhat.agbsame_to)^(1-data.2$y))

L3$yhat.agbsame_to= (data.3$donoun / (1 + exp(-(mean(samed$aAccPas) + mean(samed$bAcc) * data.3$x + mean(samed$gAcc) * data.3$iosum))))
L3$phat.agbsame_to=((L3$yhat.agbsame_to)^(data.3$y))*((1-L3$yhat.agbsame_to)^(1-data.3$y))

L4$yhat.agbsame_to= ((1 - ((1 - ord) * data.4$iopro))/ (1 + exp(-(mean(samed$aToPas) + mean(samed$bTo) * data.4$x + mean(samed$gToPas) * data.4$iosum))))
L4$phat.agbsame_to=((L4$yhat.agbsame_to)^(data.4$y))*((1-L4$yhat.agbsame_to)^(1-data.4$y))

load('agbsame_ad.RData')
agbsamed_ad<-as.data.frame(agbsame_ad)
samed<-as.data.frame(agbsame_ad)

L1$yhat.agbsame_ad= ((1 - ((1 - ord) * data.1$iodo))/ (1 + exp(-(mean(samed$aToAD) + mean(samed$bToAD) * data.1$x + mean(samed$gToAD) * data.1$iosum))))
L1$phat.agbsame_ad=((L1$yhat.agbsame_ad)^(data.1$y))*((1-L1$yhat.agbsame_ad)^(1-data.1$y))

L2$yhat.agbsame_ad= (((1 - ((1 - ord) * data.2$iopro)) * data.2$donoun)/ (1 + exp(-(mean(samed$aToDA) + mean(samed$bToDA) * data.2$x + mean(samed$gToDA) * data.2$iosum)))) * (1 - (mean(samed$heavy) / (1 + exp(-(mean(samed$aAccAct) + mean(samed$bAcc) * data.2$x + mean(samed$gAcc) * data.2$iosum)))))
L2$phat.agbsame_ad=((L2$yhat.agbsame_ad)^(data.2$y))*((1-L2$yhat.agbsame_ad)^(1-data.2$y))

L3$yhat.agbsame_ad= (data.3$donoun / (1 + exp(-(mean(samed$aAccPas) + mean(samed$bAcc) * data.3$x + mean(samed$gAcc) * data.3$iosum))))
L3$phat.agbsame_ad=((L3$yhat.agbsame_ad)^(data.3$y))*((1-L3$yhat.agbsame_ad)^(1-data.3$y))

L4$yhat.agbsame_ad= ((1 - ((1 - ord) * data.4$iopro))/ (1 + exp(-(mean(samed$aToPas) + mean(samed$bToAD) * data.4$x + mean(samed$gToPas) * data.4$iosum))))
L4$phat.agbsame_ad=((L4$yhat.agbsame_ad)^(data.4$y))*((1-L4$yhat.agbsame_ad)^(1-data.4$y))

load('agbgsame_act.RData')
agbgsamed_act<-as.data.frame(agbgsame_act)
samed<-as.data.frame(agbgsame_act)

L1$yhat.agbgsame_act= ((1 - ((1 - ord) * data.1$iodo))/ (1 + exp(-(mean(samed$aToAD) + mean(samed$bToAct) * data.1$x + mean(samed$gToAct) * data.1$iosum))))
L1$phat.agbgsame_act=((L1$yhat.agbgsame_act)^(data.1$y))*((1-L1$yhat.agbgsame_act)^(1-data.1$y))

L2$yhat.agbgsame_act= (((1 - ((1 - ord) * data.2$iopro)) * data.2$donoun)/ (1 + exp(-(mean(samed$aToDA) + mean(samed$bToAct) * data.2$x + mean(samed$gToAct) * data.2$iosum)))) * (1 - (mean(samed$heavy) / (1 + exp(-(mean(samed$aAccAct) + mean(samed$bAcc) * data.2$x + mean(samed$gAcc) * data.2$iosum)))))
L2$phat.agbgsame_act=((L2$yhat.agbgsame_act)^(data.2$y))*((1-L2$yhat.agbgsame_act)^(1-data.2$y))

L3$yhat.agbgsame_act= (data.3$donoun / (1 + exp(-(mean(samed$aAccPas) + mean(samed$bAcc) * data.3$x + mean(samed$gAcc) * data.3$iosum))))
L3$phat.agbgsame_act=((L3$yhat.agbgsame_act)^(data.3$y))*((1-L3$yhat.agbgsame_act)^(1-data.3$y))

L4$yhat.agbgsame_act= ((1 - ((1 - ord) * data.4$iopro))/ (1 + exp(-(mean(samed$aToPas) + mean(samed$bToPas) * data.4$x + mean(samed$gToPas) * data.4$iosum))))
L4$phat.agbgsame_act=((L4$yhat.agbgsame_act)^(data.4$y))*((1-L4$yhat.agbgsame_act)^(1-data.4$y))

load('agbgsame_to.RData')
agbgsamed_to<-as.data.frame(agbgsame_to)
samed<-as.data.frame(agbgsame_to)

L1$yhat.agbgsame_to= ((1 - ((1 - ord) * data.1$iodo))/ (1 + exp(-(mean(samed$aToAD) + mean(samed$bTo) * data.1$x + mean(samed$gTo) * data.1$iosum))))
L1$phat.agbgsame_to=((L1$yhat.agbgsame_to)^(data.1$y))*((1-L1$yhat.agbgsame_to)^(1-data.1$y))

L2$yhat.agbgsame_to= (((1 - ((1 - ord) * data.2$iopro)) * data.2$donoun)/ (1 + exp(-(mean(samed$aToDA) + mean(samed$bTo) * data.2$x + mean(samed$gTo) * data.2$iosum)))) * (1 - (mean(samed$heavy) / (1 + exp(-(mean(samed$aAccAct) + mean(samed$bAcc) * data.2$x + mean(samed$gAcc) * data.2$iosum)))))
L2$phat.agbgsame_to=((L2$yhat.agbgsame_to)^(data.2$y))*((1-L2$yhat.agbgsame_to)^(1-data.2$y))

L3$yhat.agbgsame_to= (data.3$donoun / (1 + exp(-(mean(samed$aAccPas) + mean(samed$bAcc) * data.3$x + mean(samed$gAcc) * data.3$iosum))))
L3$phat.agbgsame_to=((L3$yhat.agbgsame_to)^(data.3$y))*((1-L3$yhat.agbgsame_to)^(1-data.3$y))

L4$yhat.agbgsame_to= ((1 - ((1 - ord) * data.4$iopro))/ (1 + exp(-(mean(samed$aToPas) + mean(samed$bTo) * data.4$x + mean(samed$gTo) * data.4$iosum))))
L4$phat.agbgsame_to=((L4$yhat.agbgsame_to)^(data.4$y))*((1-L4$yhat.agbgsame_to)^(1-data.4$y))

load('agbgsame_ad.RData')
agbgsamed_ad<-as.data.frame(agbgsame_ad)
samed<-as.data.frame(agbgsame_ad)

L1$yhat.agbgsame_ad= ((1 - ((1 - ord) * data.1$iodo))/ (1 + exp(-(mean(samed$aToAD) + mean(samed$bToAD) * data.1$x + mean(samed$gToAD) * data.1$iosum))))
L1$phat.agbgsame_ad=((L1$yhat.agbgsame_ad)^(data.1$y))*((1-L1$yhat.agbgsame_ad)^(1-data.1$y))

L2$yhat.agbgsame_ad= (((1 - ((1 - ord) * data.2$iopro)) * data.2$donoun)/ (1 + exp(-(mean(samed$aToDA) + mean(samed$bToDA) * data.2$x + mean(samed$gToDA) * data.2$iosum)))) * (1 - (mean(samed$heavy) / (1 + exp(-(mean(samed$aAccAct) + mean(samed$bAcc) * data.2$x + mean(samed$gAcc) * data.2$iosum)))))
L2$phat.agbgsame_ad=((L2$yhat.agbgsame_ad)^(data.2$y))*((1-L2$yhat.agbgsame_ad)^(1-data.2$y))

L3$yhat.agbgsame_ad= (data.3$donoun / (1 + exp(-(mean(samed$aAccPas) + mean(samed$bAcc) * data.3$x + mean(samed$gAcc) * data.3$iosum))))
L3$phat.agbgsame_ad=((L3$yhat.agbgsame_ad)^(data.3$y))*((1-L3$yhat.agbgsame_ad)^(1-data.3$y))

L4$yhat.agbgsame_ad= ((1 - ((1 - ord) * data.4$iopro))/ (1 + exp(-(mean(samed$aToPas) + mean(samed$bToAD) * data.4$x + mean(samed$gToAD) * data.4$iosum))))
L4$phat.agbgsame_ad=((L4$yhat.agbgsame_ad)^(data.4$y))*((1-L4$yhat.agbgsame_ad)^(1-data.4$y))

load('abgsame_act.RData')
abgsamed_act<-as.data.frame(abgsame_act)
samed<-as.data.frame(abgsame_act)

L1$yhat.abtogactsame= ((1 - ((1 - ord) * data.1$iodo))/ (1 + exp(-(mean(samed$aToAD) + mean(abgsamed_to$bTo) * data.1$x + mean(samed$gToAct) * data.1$iosum))))
L1$phat.abtogactsame=((L1$yhat.abtogactsame)^(data.1$y))*((1-L1$yhat.abtogactsame)^(1-data.1$y))

L2$yhat.abtogactsame= (((1 - ((1 - ord) * data.2$iopro)) * data.2$donoun)/ (1 + exp(-(mean(samed$aToDA) + mean(abgsamed_to$bTo) * data.2$x + mean(samed$gToAct) * data.2$iosum)))) * (1 - (mean(samed$heavy) / (1 + exp(-(mean(samed$aAccAct) + mean(samed$bAcc) * data.2$x + mean(samed$gAccAct) * data.2$iosum)))))
L2$phat.abtogactsame=((L2$yhat.abtogactsame)^(data.2$y))*((1-L2$yhat.abtogactsame)^(1-data.2$y))

L3$yhat.abtogactsame= (data.3$donoun / (1 + exp(-(mean(samed$aAccPas) + mean(samed$bAcc) * data.3$x + mean(samed$gAccPas) * data.3$iosum))))
L3$phat.abtogactsame=((L3$yhat.abtogactsame)^(data.3$y))*((1-L3$yhat.abtogactsame)^(1-data.3$y))

L4$yhat.abtogactsame= ((1 - ((1 - ord) * data.4$iopro))/ (1 + exp(-(mean(samed$aToPas) + mean(abgsamed_to$bTo) * data.4$x + mean(samed$gToPas) * data.4$iosum))))
L4$phat.abtogactsame=((L4$yhat.abtogactsame)^(data.4$y))*((1-L4$yhat.abtogactsame)^(1-data.4$y))

load('abgsame_act.RData')
samed<-as.data.frame(abgsame_act)

L1$yhat.abactgtosame= ((1 - ((1 - ord) * data.1$iodo))/ (1 + exp(-(mean(samed$aToAD) + mean(samed$bToAct) * data.1$x + mean(abgsamed_to$gTo) * data.1$iosum))))
L1$phat.abactgtosame=((L1$yhat.abactgtosame)^(data.1$y))*((1-L1$yhat.abactgtosame)^(1-data.1$y))

L2$yhat.abactgtosame= (((1 - ((1 - ord) * data.2$iopro)) * data.2$donoun)/ (1 + exp(-(mean(samed$aToDA) + mean(samed$bToAct) * data.2$x + mean(abgsamed_to$gTo) * data.2$iosum)))) * (1 - (mean(samed$heavy) / (1 + exp(-(mean(samed$aAccAct) + mean(samed$bAcc) * data.2$x + mean(samed$gAccAct) * data.2$iosum)))))
L2$phat.abactgtosame=((L2$yhat.abactgtosame)^(data.2$y))*((1-L2$yhat.abactgtosame)^(1-data.2$y))

L3$yhat.abactgtosame= (data.3$donoun / (1 + exp(-(mean(samed$aAccPas) + mean(samed$bAcc) * data.3$x + mean(samed$gAccPas) * data.3$iosum))))
L3$phat.abactgtosame=((L3$yhat.abactgtosame)^(data.3$y))*((1-L3$yhat.abactgtosame)^(1-data.3$y))

L4$yhat.abactgtosame= ((1 - ((1 - ord) * data.4$iopro))/ (1 + exp(-(mean(samed$aToPas) + mean(samed$bToPas) * data.4$x + mean(abgsamed_to$gTo) * data.4$iosum))))
L4$phat.abactgtosame=((L4$yhat.abactgtosame)^(data.4$y))*((1-L4$yhat.abactgtosame)^(1-data.4$y))

L<-as.data.frame(rbind(L1,L2,L3,L4))
names = 'order'
AIC = (2*dim(orderd)[2])-(2*sum(log(subset(L,phat.order!=0&phat.order!=1)$phat.order)))
BIC = (log(dim(subset(L,phat.order!=0&phat.order!=1))[1])*dim(orderd)[2])-(2*sum(log(subset(L,phat.order!=0&phat.order!=1)$phat.order)))

names = c(names,'noint')
AIC = c(AIC,(2*dim(noid)[2])-(2*sum(log(subset(L,phat.noint!=0&phat.noint!=1)$phat.noint))))
BIC = c(BIC,(log(dim(subset(L,phat.noint!=0&phat.noint!=1))[1])*dim(noid)[2])-(2*sum(log(subset(L,phat.noint!=0&phat.noint!=1)$phat.noint))))

names = c(names,'asame')
AIC = c(AIC,(2*dim(asamed)[2])-(2*sum(log(subset(L,phat.asame!=0&phat.asame!=1)$phat.asame))))
BIC = c(BIC,(log(dim(subset(L,phat.asame!=0&phat.asame!=1))[1])*dim(asamed)[2])-(2*sum(log(subset(L,phat.asame!=0&phat.asame!=1)$phat.asame))))

names = c(names,'agsame')
AIC = c(AIC,(2*dim(agsamed)[2])-(2*sum(log(subset(L,phat.agsame!=0&phat.agsame!=1)$phat.agsame))))
BIC = c(BIC,(log(dim(subset(L,phat.agsame!=0&phat.agsame!=1))[1])*dim(agsamed)[2])-(2*sum(log(subset(L,phat.agsame!=0&phat.agsame!=1)$phat.agsame))))

names = c(names,'bsame_act')
AIC = c(AIC,(2*dim(bsamed_act)[2])-(2*sum(log(subset(L,phat.bsame_act!=0&phat.bsame_act!=1)$phat.bsame_act))))
BIC = c(BIC,(log(dim(subset(L,phat.bsame_act!=0&phat.bsame_act!=1))[1])*dim(bsamed_act)[2])-(2*sum(log(subset(L,phat.bsame_act!=0&phat.bsame_act!=1)$phat.bsame_act))))

names = c(names,'bsame_to')
AIC = c(AIC,(2*dim(bsamed_to)[2])-(2*sum(log(subset(L,phat.bsame_to!=0&phat.bsame_to!=1)$phat.bsame_to))))
BIC = c(BIC,(log(dim(subset(L,phat.bsame_to!=0&phat.bsame_to!=1))[1])*dim(bsamed_to)[2])-(2*sum(log(subset(L,phat.bsame_to!=0&phat.bsame_to!=1)$phat.bsame_to))))

names = c(names,'bsame_ad')
AIC = c(AIC,(2*dim(bsamed_ad)[2])-(2*sum(log(subset(L,phat.bsame_ad!=0&phat.bsame_ad!=1)$phat.bsame_ad))))
BIC = c(BIC,(log(dim(subset(L,phat.bsame_ad!=0&phat.bsame_ad!=1))[1])*dim(bsamed_ad)[2])-(2*sum(log(subset(L,phat.bsame_ad!=0&phat.bsame_ad!=1)$phat.bsame_ad))))

names = c(names,'bgsame_act')
AIC = c(AIC,(2*dim(bgsamed_act)[2])-(2*sum(log(subset(L,phat.bgsame_act!=0&phat.bgsame_act!=1)$phat.bgsame_act))))
BIC = c(BIC,(log(dim(subset(L,phat.bgsame_act!=0&phat.bgsame_act!=1))[1])*dim(bgsamed_act)[2])-(2*sum(log(subset(L,phat.bgsame_act!=0&phat.bgsame_act!=1)$phat.bgsame_act))))

names = c(names,'bgsame_to')
AIC = c(AIC,(2*dim(bgsamed_to)[2])-(2*sum(log(subset(L,phat.bgsame_to!=0&phat.bgsame_to!=1)$phat.bgsame_to))))
BIC = c(BIC,(log(dim(subset(L,phat.bgsame_to!=0&phat.bgsame_to!=1))[1])*dim(bgsamed_to)[2])-(2*sum(log(subset(L,phat.bgsame_to!=0&phat.bgsame_to!=1)$phat.bgsame_to))))

names = c(names,'bgsame_ad')
AIC = c(AIC,(2*dim(bgsamed_ad)[2])-(2*sum(log(subset(L,phat.bgsame_ad!=0&phat.bgsame_ad!=1)$phat.bgsame_ad))))
BIC = c(BIC,(log(dim(subset(L,phat.bgsame_ad!=0&phat.bgsame_ad!=1))[1])*dim(bgsamed_ad)[2])-(2*sum(log(subset(L,phat.bgsame_ad!=0&phat.bgsame_ad!=1)$phat.bgsame_ad))))


names = c(names,'absame_act')
AIC = c(AIC,(2*dim(absamed_act)[2])-(2*sum(log(subset(L,phat.absame_act!=0&phat.absame_act!=1)$phat.absame_act))))
BIC = c(BIC,(log(dim(subset(L,phat.absame_act!=0&phat.absame_act!=1))[1])*dim(absamed_act)[2])-(2*sum(log(subset(L,phat.absame_act!=0&phat.absame_act!=1)$phat.absame_act))))

names = c(names,'absame_to')
AIC = c(AIC,(2*dim(absamed_to)[2])-(2*sum(log(subset(L,phat.absame_to!=0&phat.absame_to!=1)$phat.absame_to))))
BIC = c(BIC,(log(dim(subset(L,phat.absame_to!=0&phat.absame_to!=1))[1])*dim(absamed_to)[2])-(2*sum(log(subset(L,phat.absame_to!=0&phat.absame_to!=1)$phat.absame_to))))

names = c(names,'absame_ad')
AIC = c(AIC,(2*dim(absamed_ad)[2])-(2*sum(log(subset(L,phat.absame_ad!=0&phat.absame_ad!=1)$phat.absame_ad))))
BIC = c(BIC,(log(dim(subset(L,phat.absame_ad!=0&phat.absame_ad!=1))[1])*dim(absamed_ad)[2])-(2*sum(log(subset(L,phat.absame_ad!=0&phat.absame_ad!=1)$phat.absame_ad))))

names = c(names,'abgsame_act')
AIC = c(AIC,(2*dim(abgsamed_act)[2])-(2*sum(log(subset(L,phat.abgsame_act!=0&phat.abgsame_act!=1)$phat.abgsame_act))))
BIC = c(BIC,(log(dim(subset(L,phat.abgsame_act!=0&phat.abgsame_act!=1))[1])*dim(abgsamed_act)[2])-(2*sum(log(subset(L,phat.abgsame_act!=0&phat.abgsame_act!=1)$phat.abgsame_act))))

names = c(names,'abgsame_to')
AIC = c(AIC,(2*dim(abgsamed_to)[2])-(2*sum(log(subset(L,phat.abgsame_to!=0&phat.abgsame_to!=1)$phat.abgsame_to))))
BIC = c(BIC,(log(dim(subset(L,phat.abgsame_to!=0&phat.abgsame_to!=1))[1])*dim(abgsamed_to)[2])-(2*sum(log(subset(L,phat.abgsame_to!=0&phat.abgsame_to!=1)$phat.abgsame_to))))

names = c(names,'abgsame_ad')
AIC = c(AIC,(2*dim(abgsamed_ad)[2])-(2*sum(log(subset(L,phat.abgsame_ad!=0&phat.abgsame_ad!=1)$phat.abgsame_ad))))
BIC = c(BIC,(log(dim(subset(L,phat.abgsame_ad!=0&phat.abgsame_ad!=1))[1])*dim(abgsamed_ad)[2])-(2*sum(log(subset(L,phat.abgsame_ad!=0&phat.abgsame_ad!=1)$phat.abgsame_ad))))


names = c(names,'agbsame_act')
AIC = c(AIC,(2*dim(agbsamed_act)[2])-(2*sum(log(subset(L,phat.agbsame_act!=0&phat.agbsame_act!=1)$phat.agbsame_act))))
BIC = c(BIC,(log(dim(subset(L,phat.agbsame_act!=0&phat.agbsame_act!=1))[1])*dim(agbsamed_act)[2])-(2*sum(log(subset(L,phat.agbsame_act!=0&phat.agbsame_act!=1)$phat.agbsame_act))))

names = c(names,'agbsame_to')
AIC = c(AIC,(2*dim(agbsamed_to)[2])-(2*sum(log(subset(L,phat.agbsame_to!=0&phat.agbsame_to!=1)$phat.agbsame_to))))
BIC = c(BIC,(log(dim(subset(L,phat.agbsame_to!=0&phat.agbsame_to!=1))[1])*dim(agbsamed_to)[2])-(2*sum(log(subset(L,phat.agbsame_to!=0&phat.agbsame_to!=1)$phat.agbsame_to))))

names = c(names,'agbsame_ad')
AIC = c(AIC,(2*dim(agbsamed_ad)[2])-(2*sum(log(subset(L,phat.agbsame_ad!=0&phat.agbsame_ad!=1)$phat.agbsame_ad))))
BIC = c(BIC,(log(dim(subset(L,phat.agbsame_ad!=0&phat.agbsame_ad!=1))[1])*dim(agbsamed_ad)[2])-(2*sum(log(subset(L,phat.agbsame_ad!=0&phat.agbsame_ad!=1)$phat.agbsame_ad))))

names = c(names,'agbgsame_act')
AIC = c(AIC,(2*dim(agbgsamed_act)[2])-(2*sum(log(subset(L,phat.agbgsame_act!=0&phat.agbgsame_act!=1)$phat.agbgsame_act))))
BIC = c(BIC,(log(dim(subset(L,phat.agbgsame_act!=0&phat.agbgsame_act!=1))[1])*dim(agbgsamed_act)[2])-(2*sum(log(subset(L,phat.agbgsame_act!=0&phat.agbgsame_act!=1)$phat.agbgsame_act))))

names = c(names,'agbgsame_to')
AIC = c(AIC,(2*dim(agbgsamed_to)[2])-(2*sum(log(subset(L,phat.agbgsame_to!=0&phat.agbgsame_to!=1)$phat.agbgsame_to))))
BIC = c(BIC,(log(dim(subset(L,phat.agbgsame_to!=0&phat.agbgsame_to!=1))[1])*dim(agbgsamed_to)[2])-(2*sum(log(subset(L,phat.agbgsame_to!=0&phat.agbgsame_to!=1)$phat.agbgsame_to))))

names = c(names,'agbgsame_ad')
AIC = c(AIC,(2*dim(agbgsamed_ad)[2])-(2*sum(log(subset(L,phat.agbgsame_ad!=0&phat.agbgsame_ad!=1)$phat.agbgsame_ad))))
BIC = c(BIC,(log(dim(subset(L,phat.agbgsame_ad!=0&phat.agbgsame_ad!=1))[1])*dim(agbgsamed_ad)[2])-(2*sum(log(subset(L,phat.agbgsame_ad!=0&phat.agbgsame_ad!=1)$phat.agbgsame_ad))))

names = c(names,'abtogactsame')
AIC = c(AIC,(2*(dim(abgsamed_act)[2]-1))-(2*sum(log(subset(L,phat.abtogactsame!=0&phat.abtogactsame!=1)$phat.abtogactsame))))
BIC = c(BIC,(log(dim(subset(L,phat.abtogactsame!=0&phat.abtogactsame!=1))[1])*(dim(abgsamed_act)[2]-1))-(2*sum(log(subset(L,phat.abtogactsame!=0&phat.abtogactsame!=1)$phat.abtogactsame))))

names = c(names,'abactgtosame')
AIC = c(AIC,(2*(dim(abgsamed_act)[2]-1))-(2*sum(log(subset(L,phat.abactgtosame!=0&phat.abactgtosame!=1)$phat.abactgtosame))))
BIC = c(BIC,(log(dim(subset(L,phat.abactgtosame!=0&phat.abactgtosame!=1))[1])*(dim(abgsamed_act)[2]-1))-(2*sum(log(subset(L,phat.abactgtosame!=0&phat.abactgtosame!=1)$phat.abactgtosame))))

tab<-as.table(cbind(AIC,BIC))
row.names(tab)<-names
tab
names[which.min(AIC)]
names[which.min(BIC)]

load('da_stan.RData')
dad<-as.data.frame(mod)
load('new_sep.RData')
sepd<-as.data.frame(sep)

pred.data.ad<-data.frame(Year=rep(seq(650,1950),4),is.iopro=c(rep(1,1301),rep(1,1301),rep(-1,1301),rep(-1,1301)),is.dopro=c(rep(1,1301),rep(-1,1301),rep(1,1301),rep(-1,1301)),fac.iopro=c(rep(1,1301),rep(1,1301),rep(0,1301),rep(0,1301)),fac.dopro=c(rep(1,1301),rep(0,1301),rep(1,1301),rep(0,1301)))
pred.data.ad$Year<-as.numeric(as.character(pred.data.ad$Year))
pred.data.ad$is.iopro<-as.numeric(as.character(pred.data.ad$is.iopro))
pred.data.ad$is.iodo<-factor(pred.data.ad$is.iopro+pred.data.ad$is.dopro)
levels(pred.data.ad$is.iodo)<-c('0','0','1')
pred.data.ad$is.iodo<-as.numeric(as.character(pred.data.ad$is.iodo))
pred.data.ad$sYear<-(pred.data.ad$Year-mean(joint.data$Year))/sd(joint.data$Year)
pred.data.ad$Type<-factor(c('AD'))
pred.data.ad$IO<-factor(pred.data.ad$is.iopro)
pred.data.ad$DO<-factor(pred.data.ad$is.dopro)
levels(pred.data.ad$IO)<-c('IONoun','IOPronoun')
levels(pred.data.ad$DO)<-c('DONoun','DOPronoun')

Order1500 <- margin.table(table(subset(joint.data,(Type=='AD'|Type=='DA')&Year>1500&IO=='IONoun'&DO=='DONoun')$Type))
AD1500 <- margin.table(table(subset(joint.data,Type=='AD'&Year>1500&IO=='IONoun'&DO=='DONoun')$Type))

pred.data.ad$Value.da<-(1-((1-AD1500/Order1500)*pred.data.ad$is.iodo))/(1+exp(-(mean(sepd$a1)+mean(dad$b1)*pred.data.ad$sYear+mean(dad$g1)*pred.data.ad$is.iopro)))
pred.data.ad$Value.sep<-(1-((1-AD1500/Order1500)*pred.data.ad$is.iodo))/(1+exp(-(mean(sepd$a1)+mean(sepd$b1)*pred.data.ad$sYear+mean(sepd$g1)*pred.data.ad$is.iopro)))
pred.data.ad$Value.both<-(1-((1-AD1500/Order1500)*pred.data.ad$is.iodo))/(1+exp(-(mean(sepd$a1)+mean(c(dad$b1,sepd$b1))*pred.data.ad$sYear+mean(c(dad$g1,sepd$g1))*pred.data.ad$is.iopro)))

pred.data.pasto<-pred.data.ad
levels(pred.data.pasto$Type)<-c('PasTo')

pred.data.pasto$Value.da<-(1-((1-AD1500/Order1500)*pred.data.pasto$fac.iopro))/(1+exp(-(mean(sepd$a1)+mean(dad$b1)*pred.data.pasto$sYear+mean(dad$g1)*pred.data.pasto$is.iopro)))
pred.data.pasto$Value.sep<-(1-((1-AD1500/Order1500)*pred.data.pasto$fac.iopro))/(1+exp(-(mean(sepd$a1)+mean(sepd$b1)*pred.data.pasto$sYear+mean(sepd$g1)*pred.data.pasto$is.iopro)))
pred.data.pasto$Value.both<-(1-((1-AD1500/Order1500)*pred.data.pasto$fac.iopro))/(1+exp(-(mean(sepd$a1)+mean(c(dad$b1,sepd$b1))*pred.data.pasto$sYear+mean(c(dad$g1,sepd$g1))*pred.data.pasto$is.iopro)))

pred.data.pas<-pred.data.ad
levels(pred.data.pas$Type)<-c('Passive')

pred.data.pas$Value.da<-(1-(1*pred.data.pas$fac.dopro))/(1+exp(-(mean(sepd$a4)+mean(dad$b2)*pred.data.pas$sYear+mean(dad$g2)*pred.data.pas$is.iopro)))
pred.data.pas$Value.sep<-(1-(1*pred.data.pas$fac.dopro))/(1+exp(-(mean(sepd$a4)+mean(sepd$b2)*pred.data.pas$sYear+mean(sepd$g3)*pred.data.pas$is.iopro)))
pred.data.pas$Value.both<-(1-(1*pred.data.pas$fac.dopro))/(1+exp(-(mean(sepd$a4)+mean(c(dad$b2,sepd$b2))*pred.data.pas$sYear+mean(c(dad$g2,sepd$g3))*pred.data.pas$is.iopro)))

pred.data.da<-pred.data.ad
levels(pred.data.da$Type)<-c('DA')
pred.data.da$Value.da<-((1-(1*pred.data.da$fac.dopro))/(1+exp(-(mean(dad$a1)+mean(dad$b1)*pred.data.da$sYear+mean(dad$g1)*pred.data.da$is.iopro))))*(1-(mean(dad$heavy)/(1+exp(-(mean(dad$a2)+mean(dad$b2)*pred.data.da$sYear+mean(dad$g2)*pred.data.da$is.iopro)))))
pred.data.da$Value.sep<-((1-(1*pred.data.da$fac.dopro))/(1+exp(-(mean(sepd$a2)+mean(sepd$b1)*pred.data.da$sYear+mean(sepd$g2)*pred.data.da$is.iopro))))*(1-(mean(sepd$heavy)/(1+exp(-(mean(sepd$a3)+mean(sepd$b2)*pred.data.da$sYear+mean(sepd$g3)*pred.data.da$is.iopro)))))
pred.data.da$Value.both<-((1-(1*pred.data.da$fac.dopro))/(1+exp(-(mean(c(dad$a1,sepd$a2))+mean(c(sepd$b1,dad$b1))*pred.data.da$sYear+mean(c(sepd$g2,dad$g1))*pred.data.da$is.iopro))))*(1-(mean(c(sepd$heavy,dad$heavy))/(1+exp(-(mean(c(sepd$a3,dad$a2))+mean(c(sepd$b2,dad$b2))*pred.data.da$sYear+mean(c(sepd$g3,dad$g2))*pred.data.da$is.iopro)))))

pred.data<-as.data.frame(rbind(pred.data.ad,pred.data.da,pred.data.pas,pred.data.pasto))

ggplot(data=new.data,aes(Bin,new.val,colour=IO))+geom_point(aes(size=log2(n)))+geom_line(data=pred.data,aes(Year,Value.da,linetype='da'))+geom_line(data=pred.data,aes(Year,Value.sep,linetype='sep'))+geom_line(data=pred.data,aes(Year,Value.both,linetype='both'))+facet_grid(DO~Type)

DA with pro interaction
pred.data.da<-pred.data.ad
levels(pred.data.da$Type)<-c('DA')
pred.data.da$Value.da<-(((1-((1-AD1500/Order1500)*pred.data.pasto$fac.iopro))*(1-(1*pred.data.da$fac.dopro)))/(1+exp(-(mean(dad$a1)+mean(dad$b1)*pred.data.da$sYear+mean(dad$g1)*pred.data.da$is.iopro))))*(1-(mean(dad$heavy)/(1+exp(-(mean(dad$a2)+mean(dad$b2)*pred.data.da$sYear+mean(dad$g2)*pred.data.da$is.iopro)))))
pred.data.da$Value.sep<-(((1-((1-AD1500/Order1500)*pred.data.pasto$fac.iopro))*(1-(1*pred.data.da$fac.dopro)))/(1+exp(-(mean(sepd$a2)+mean(sepd$b1)*pred.data.da$sYear+mean(sepd$g2)*pred.data.da$is.iopro))))*(1-(mean(sepd$heavy)/(1+exp(-(mean(sepd$a3)+mean(sepd$b2)*pred.data.da$sYear+mean(sepd$g3)*pred.data.da$is.iopro)))))
pred.data.da$Value.both<-(((1-((1-AD1500/Order1500)*pred.data.pasto$fac.iopro))*(1-(1*pred.data.da$fac.dopro)))/(1+exp(-(mean(c(dad$a1,sepd$a2))+mean(c(sepd$b1,dad$b1))*pred.data.da$sYear+mean(c(sepd$g2,dad$g1))*pred.data.da$is.iopro))))*(1-(mean(c(sepd$heavy,dad$heavy))/(1+exp(-(mean(c(sepd$a3,dad$a2))+mean(c(sepd$b2,dad$b2))*pred.data.da$sYear+mean(c(sepd$g3,dad$g2))*pred.data.da$is.iopro)))))
