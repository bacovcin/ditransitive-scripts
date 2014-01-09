library(bbmle)
library(plyr)
library(ggplot2)
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

real.data<-data.frame(Year=joint.data$Year,x=standardize(joint.data$Year),y=joint.data$Value,cond=joint.data$Cond,iosum=joint.data$iosum,iopro=joint.data$iopro,iodo=joint.data$iodo,donoun=joint.data$donoun)

real.data$is.to <- factor(real.data$cond)
levels(real.data$is.to)<-c('1','1','0','1')
real.data$is.to<-as.numeric(as.character(real.data$is.to))

real.data$is.acc <- factor(real.data$cond)
levels(real.data$is.acc)<-c('0','1','1','0')
real.data$is.acc<-as.numeric(as.character(real.data$is.acc))

real.data$acc.sum <- factor(real.data$cond)
levels(real.data$acc.sum)<-c('0','-1','1','0')
real.data$acc.sum<-as.numeric(as.character(real.data$acc.sum))

real.data$is.da <- factor(real.data$cond)
levels(real.data$is.da)<-c('0','1','0','0')
real.data$is.da<-as.numeric(as.character(real.data$is.da))
real.data$is.ad <- factor(real.data$cond)
levels(real.data$is.ad)<-c('1','0','0','0')
real.data$is.ad<-as.numeric(as.character(real.data$is.ad))
real.data$not.ad <- factor(real.data$cond)
levels(real.data$not.ad)<-c('0','1','1','1')
real.data$not.ad<-as.numeric(as.character(real.data$not.ad))

data1<-subset(real.data,cond==1)
data2<-subset(real.data,cond==2&donoun==1)
data3<-subset(real.data,cond==3&donoun==1)
data4<-subset(real.data,cond==4)

true.data<-as.data.frame(rbind(data1,data2,data3,data4))

Order1500 <- margin.table(table(subset(joint.data,(Type=='AD'|Type=='DA')&Year>1500&IO=='IONoun'&DO=='DONoun')$Type))
AD1500 <- margin.table(table(subset(joint.data,Type=='AD'&Year>1500&IO=='IONoun'&DO=='DONoun')$Type))
ord = (Order1500-AD1500)/Order1500
aOrd = (1-ord)*ord

full_ll<-function(aTo=0,aAcc=0,aToAD=0,aToDA=0,aAccAct=0,aAccPas=0,aToPas=0,bTo=0,bAcc=0,bToAD=0,bToDA=0,bAccAct=0,bAccPas=0,bToPas=0,gTo=0,gAcc=0,gToAD=0,gToDA=0,gAccAct=0,gAccPas=0,gToPas=0,dTo=0,dAcc=0,dToAD=0,dToDA=0,dAccAct=0,dAccPas=0,dToPas=0,orderTo=0,orderAcc=0,orderToAD=0,orderToDA=0,orderAccAct=0,orderAccPas=0,orderToPas=0){
  p1<-((1-(1-(ord+orderTo+orderToAD))*data1$iodo)/(1+exp(-(aTo+aToAD+(bTo+bToAD)*data1$x+(gTo+gToAD)*data1$iosum+(dTo+dToAD)*data1$x*data1$iosum))))
  p2<-((1-(1-(ord+orderTo+orderToDA))*data2$iodo)/(1+exp(-(aTo+aToDA+(bTo+bToDA)*data2$x+(gTo+gToDA)*data2$iosum+(dTo+dToDA)*data2$x*data2$iosum))))*
    ((1-(1-(aOrd+orderAcc+orderAccAct))*data2$iopro)-((1-(1-(aOrd+orderAcc+orderAccAct))*data2$iopro)/(1+exp(-(aAcc+aAccAct+(bAcc+bAccAct)*data2$x+(gAcc+gAccAct)*data2$iosum+(dAcc+dAccAct)*data2$x*data2$iosum)))))*data2$donoun
  p3<-((1-(1-(aOrd+orderAcc+orderAccPas))*data3$iopro)/(1+exp(-(aAcc+aAccPas+(bAcc+bAccPas)*data3$x+(gAcc+gAccPas)*data3$iosum+(dAcc+dAccPas)*data3$x*data3$iosum))))*data3$donoun
  p4<-((1-(1-(ord+orderTo+orderToPas))*data4$iopro)/(1+exp(-(aTo+aToPas+(bTo+bToPas)*data4$x+(gTo+gToPas)*data4$iosum+(dTo+dToPas)*data4$x*data4$iosum))))
  sum1<--sum(stats::dbinom(data1$y, 1, p1,log=TRUE))
  sum2<--sum(stats::dbinom(data2$y, 1, p2,log=TRUE))
  sum3<--sum(stats::dbinom(data3$y, 1, p3,log=TRUE))
  sum4<--sum(stats::dbinom(data4$y, 1, p4,log=TRUE))
  results<-sum(sum1,sum2,sum3,sum4) 
  if (is.finite(results)) { 
    if (is.nan(results)){
      results=NA
      
    }
  }
  else{
    results=NA
  } 
  results
}

fita2b2g2d2o2<-mle2(full_ll,start=list(aTo=.6,aAcc=-3.1,aToAD=0,aToDA=0,aAccAct=0,aAccPas=0,aToPas=0,
                                 bTo=3.9,bAcc=2.7,bToAD=0,bToDA=0,bAccAct=0,bAccPas=0,bToPas=0,
                                 gTo=-1.4,gAcc=-.7,gToAD=0,gToDA=0,gAccAct=0,gAccPas=0,gToPas=0,
                                 dTo=0,dAcc=0,dToAD=0,dToDA=0,dAccAct=0,dAccPas=0,dToPas=0,
                                 orderTo=0,orderAcc=0,
                                 orderToAD=0,orderToDA=0,orderAccAct=0,orderAccPas=0,orderToPas=0),
              control=list(trace=4,REPORT=5,maxit=1000))

fita2b2g2d2o1<-mle2(full_ll,start=list(aTo=.6,aAcc=-3.1,aToAD=0,aToDA=0,aAccAct=0,aAccPas=0,aToPas=0,
                                       bTo=3.9,bAcc=2.7,bToAD=0,bToDA=0,bAccAct=0,bAccPas=0,bToPas=0,
                                       gTo=-1.4,gAcc=-.7,gToAD=0,gToDA=0,gAccAct=0,gAccPas=0,gToPas=0,
                                       dTo=0,dAcc=0,dToAD=0,dToDA=0,dAccAct=0,dAccPas=0,dToPas=0,
                                       orderTo=0,orderAcc=0),
                    control=list(trace=4,REPORT=5,maxit=1000))

fita2b2g2d2o0<-mle2(full_ll,start=list(aTo=.6,aAcc=-3.1,aToAD=0,aToDA=0,aAccAct=0,aAccPas=0,aToPas=0,
                                       bTo=3.9,bAcc=2.7,bToAD=0,bToDA=0,bAccAct=0,bAccPas=0,bToPas=0,
                                       gTo=-1.4,gAcc=-.7,gToAD=0,gToDA=0,gAccAct=0,gAccPas=0,gToPas=0,
                                       dTo=0,dAcc=0,dToAD=0,dToDA=0,dAccAct=0,dAccPas=0,dToPas=0),
                    control=list(trace=4,REPORT=5,maxit=1000))

fita2b2g2d1o0<-mle2(full_ll,start=list(aTo=.6,aAcc=-3.1,aToAD=0,aToDA=0,aAccAct=0,aAccPas=0,aToPas=0,
                                       bTo=3.9,bAcc=2.7,bToAD=0,bToDA=0,bAccAct=0,bAccPas=0,bToPas=0,
                                       gTo=-1.4,gAcc=-.7,gToAD=0,gToDA=0,gAccAct=0,gAccPas=0,gToPas=0,
                                       dTo=0,dAcc=0),
                    control=list(trace=4,REPORT=5,maxit=1000))

fita2b2gTd1o0<-mle2(full_ll,start=list(aTo=.6,aAcc=-3.1,aToAD=0,aToDA=0,aAccAct=0,aAccPas=0,aToPas=0,
                                       bTo=3.9,bAcc=2.7,bToAD=0,bToDA=0,bAccAct=0,bAccPas=0,bToPas=0,
                                       gTo=-1.4,gAcc=0,gAccAct=0,gAccPas=0,
                                       dTo=0,dAcc=0),
                    control=list(trace=4,REPORT=5,maxit=1000))

fita2b1gTd1o0<-mle2(full_ll,start=list(aTo=.6,aAcc=-3.1,aToAD=0,aToDA=0,aAccAct=0,aAccPas=0,aToPas=0,
                                         bTo=3.9,bAcc=2.7,
                                         gTo=-1.4,gAcc=0,gAccAct=0,gAccPas=0,
                                         dTo=0,dAcc=0),
                      control=list(trace=4,REPORT=5,maxit=1000))

fita2b1gT.5d1o0<-mle2(full_ll,start=list(aTo=.6,aAcc=-3.1,aToAD=0,aToDA=0,aAccAct=0,aAccPas=0,aToPas=0,
                                       bTo=3.9,bAcc=2.7,
                                       gTo=-1.4,gAccAct=0,
                                       dTo=0,dAcc=0),
                    control=list(trace=4,REPORT=5,maxit=1000))

fita2b1gT.5d0o0<-mle2(full_ll,start=list(aTo=.6,aAcc=-3.1,aToAD=0,aToDA=0,aAccAct=0,aAccPas=0,aToPas=0,
                                         bTo=3.9,bAcc=2.7,
                                         gTo=-1.4,gAccAct=0),
                      control=list(trace=4,REPORT=5,maxit=1000))

AIC(fita2b2g2d2o2,fita2b2g2d2o1,fita2b2g2d2o0,fita2b2g2d1o0,fita2b2gTd1o0,fita2b1gTd1o0,fita2b1gT.5d1o0,fita2b1gT.5d0o0)

pred_ll<-function(aTo=0,aAcc=0,aToAD=0,aToDA=0,aAccAct=0,aAccPas=0,aToPas=0,bTo=0,bAcc=0,bToAD=0,bToDA=0,bAccAct=0,bAccPas=0,bToPas=0,gTo=0,gAcc=0,gToAD=0,gToDA=0,gAccAct=0,gAccPas=0,gToPas=0,dTo=0,dAcc=0,dToAD=0,dToDA=0,dAccAct=0,dAccPas=0,dToPas=0,orderTo=0,orderAcc=0,orderToAD=0,orderToDA=0,orderAccAct=0,orderAccPas=0,orderToPas=0){
  p1<-((1-(1-(ord+orderTo+orderToAD))*pdata1$iodo)/(1+exp(-(aTo+aToAD+(bTo+bToAD)*pdata1$x+(gTo+gToAD)*pdata1$iosum+(dTo+dToAD)*pdata1$x*pdata1$iosum))))
  p2<-((1-(1-(ord+orderTo+orderToDA))*pdata2$iodo)/(1+exp(-(aTo+aToDA+(bTo+bToDA)*pdata2$x+(gTo+gToDA)*pdata2$iosum+(dTo+dToDA)*pdata2$x*pdata2$iosum))))*
    ((1-(1-(ord+orderAcc+orderAccAct))*pdata2$iopro)-((1-(1-(ord+orderAcc+orderAccAct))*pdata2$iopro)/(1+exp(-(aAcc+aAccAct+(bAcc+bAccAct)*pdata2$x+(gAcc+gAccAct)*pdata2$iosum+(dAcc+dAccAct)*pdata2$x*pdata2$iosum)))))*pdata2$donoun
  p3<-((1-(1-(ord+orderAcc+orderAccPas))*pdata3$iopro)/(1+exp(-(aAcc+aAccPas+(bAcc+bAccPas)*pdata3$x+(gAcc+gAccPas)*pdata3$iosum+(dAcc+dAccPas)*pdata3$x*pdata3$iosum))))*pdata3$donoun
  p4<-((1-(1-(ord+orderTo+orderToPas))*pdata4$iopro)/(1+exp(-(aTo+aToPas+(bTo+bToPas)*pdata4$x+(gTo+gToPas)*pdata4$iosum+(dTo+dToPas)*pdata4$x*pdata4$iosum))))
  results <- data.frame(p1=p1,p2=p2,p3=p3,p4=p4)
}


pdata1<-data.frame(Year=rep(seq(650,1950),4),iosum=c(rep(1,1301),rep(1,1301),rep(-1,1301),rep(-1,1301)),dosum=c(rep(1,1301),rep(-1,1301),rep(1,1301),rep(-1,1301)),iopro=c(rep(1,1301),rep(1,1301),rep(0,1301),rep(0,1301)),donoun=c(rep(0,1301),rep(1,1301),rep(0,1301),rep(1,1301)),Type='AD')
pdata1$iodo<-factor(pdata1$iosum+pdata1$dosum)
levels(pdata1$iodo)<-c('0','0','1')
pdata1$iodo<-as.numeric(as.character(pdata1$iodo))
pdata1$x<-(pdata1$Year-mean(joint.data$Year))/sd(joint.data$Year)
pdata2<-data.frame(Year=rep(seq(650,1950),4),iosum=c(rep(1,1301),rep(1,1301),rep(-1,1301),rep(-1,1301)),dosum=c(rep(1,1301),rep(-1,1301),rep(1,1301),rep(-1,1301)),iopro=c(rep(1,1301),rep(1,1301),rep(0,1301),rep(0,1301)),donoun=c(rep(0,1301),rep(1,1301),rep(0,1301),rep(1,1301)),Type='DA')
pdata2$iodo<-factor(pdata2$iosum+pdata2$dosum)
levels(pdata2$iodo)<-c('0','0','1')
pdata2$iodo<-as.numeric(as.character(pdata2$iodo))
pdata2$x<-(pdata2$Year-mean(joint.data$Year))/sd(joint.data$Year)
pdata3<-data.frame(Year=rep(seq(650,1950),4),iosum=c(rep(1,1301),rep(1,1301),rep(-1,1301),rep(-1,1301)),dosum=c(rep(1,1301),rep(-1,1301),rep(1,1301),rep(-1,1301)),iopro=c(rep(1,1301),rep(1,1301),rep(0,1301),rep(0,1301)),donoun=c(rep(0,1301),rep(1,1301),rep(0,1301),rep(1,1301)),Type='Passive')
pdata3$iodo<-factor(pdata3$iosum+pdata3$dosum)
levels(pdata3$iodo)<-c('0','0','1')
pdata3$iodo<-as.numeric(as.character(pdata3$iodo))
pdata3$x<-(pdata3$Year-mean(joint.data$Year))/sd(joint.data$Year)
pdata4<-data.frame(Year=rep(seq(650,1950),4),iosum=c(rep(1,1301),rep(1,1301),rep(-1,1301),rep(-1,1301)),dosum=c(rep(1,1301),rep(-1,1301),rep(1,1301),rep(-1,1301)),iopro=c(rep(1,1301),rep(1,1301),rep(0,1301),rep(0,1301)),donoun=c(rep(0,1301),rep(1,1301),rep(0,1301),rep(1,1301)),Type='Passive To')
pdata4$iodo<-factor(pdata4$iosum+pdata4$dosum)
levels(pdata4$iodo)<-c('0','0','1')
pdata4$iodo<-as.numeric(as.character(pdata4$iodo))
pdata4$x<-(pdata4$Year-mean(joint.data$Year))/sd(joint.data$Year)

fullcoef<-as.list(coef(fita2b1g1d1o1))
fullpred<-as.data.frame(pred_ll(aTo=fullcoef$aTo,aAcc=fullcoef$aAcc,
                  aToAD=fullcoef$aToAD,aToDA=fullcoef$aToDA,aAccAct=fullcoef$aAccAct,aAccPas=fullcoef$aAccPas,aToPas=fullcoef$aToPas,
                  bTo=fullcoef$bTo,bAcc=fullcoef$bAcc,gTo=fullcoef$gTo,gAcc=fullcoef$gAcc,dTo=fullcoef$dTo,dAcc=fullcoef$dAcc,
                  orderTo=fullcoef$orderTo,orderAcc=fullcoef$orderAcc))
pdata1$full<-fullpred$p1
pdata2$full<-fullpred$p2
pdata3$full<-fullpred$p3
pdata4$full<-fullpred$p4

smallcoef<-as.list(coef(fita2b1gTdAoA))
smallpred<-as.data.frame(pred_ll(aTo=smallcoef$aTo,aAcc=smallcoef$aAcc,
                   aToAD=smallcoef$aToAD,aToDA=smallcoef$aToDA,aAccAct=smallcoef$aAccAct,aAccPas=smallcoef$aAccPas,aToPas=smallcoef$aToPas,
                   bTo=smallcoef$bTo,bAcc=smallcoef$bAcc,gTo=smallcoef$gTo,dAcc=smallcoef$dAcc,
                   orderAcc=smallcoef$orderAcc))
pdata1$small<-smallpred$p1
pdata2$small<-smallpred$p2
pdata3$small<-smallpred$p3
pdata4$small<-smallpred$p4

finalcoef<-as.list(coef(fita2b1gTd0oA))
finalpred<-as.data.frame(pred_ll(aTo=smallcoef$aTo,aAcc=smallcoef$aAcc,
                                 aToAD=smallcoef$aToAD,aToDA=smallcoef$aToDA,aAccAct=smallcoef$aAccAct,aAccPas=smallcoef$aAccPas,aToPas=smallcoef$aToPas,
                                 bTo=smallcoef$bTo,bAcc=smallcoef$bAcc,gTo=smallcoef$gTo,
                                 orderAcc=smallcoef$orderAcc))
pdata1$final<-finalpred$p1
pdata2$final<-finalpred$p2
pdata3$final<-finalpred$p3
pdata4$final<-finalpred$p4

pred.data<-as.data.frame(rbind(pdata1,pdata2,pdata3,pdata4))
pred.data$DO<-factor(pred.data$donoun)
levels(pred.data$DO)<-c('DOPronoun','DONoun')
pred.data$IO<-factor(pred.data$iopro)
levels(pred.data$IO)<-c('IONoun','IOPronoun')

real.data$DO<-factor(real.data$donoun)
levels(real.data$DO)<-c('DOPronoun','DONoun')
real.data$IO<-factor(real.data$iopro)
levels(real.data$IO)<-c('IONoun','IOPronoun')



joint.data$Bin<-cut(joint.data$Year,seq(675,1925,50),seq(700,1900,50))
joint.data$Bin<-as.numeric(as.character(joint.data$Bin))

new.data.1<-ddply(subset(joint.data,Cond==1),.(Type,Bin,IO,DO),summarize,new.val=sum(Value)/sum(!is.na(Value)),n=sum(!is.na(Value)))
new.data.2<-ddply(subset(joint.data,Cond==2),.(Type,Bin,IO,DO),summarize,new.val=sum(Value)/sum(!is.na(Value)),n=sum(!is.na(Value)))
new.data.3<-ddply(subset(joint.data,Cond==3),.(Type,Bin,IO,DO),summarize,new.val=sum(Value)/sum(!is.na(Value)),n=sum(!is.na(Value)))
new.data.3$Type<-factor(c('Passive'))
new.data.4<-ddply(subset(joint.data,Cond==4),.(Type,Bin,IO,DO),summarize,new.val=sum(Value)/sum(!is.na(Value)),n=sum(!is.na(Value)))
new.data.4$Type<-factor(c('Passive To'))
new.data<-as.data.frame(rbind(new.data.1,new.data.2,new.data.3,new.data.4))

real.data$Type<-factor(real.data$cond)
levels(real.data$Type)<-c('AD','DA','Passive','Passive To')

true.data$IO<-factor(true.data$iosum)
levels(true.data$IO)<-c('IONoun','IOPronoun')
true.data$DO<-factor(true.data$donoun)
levels(true.data$DO)<-c('DOPronoun','DONoun')
true.data$Type<-factor(true.data$cond)
levels(true.data$Type)<-c('AD','DA','Passive','Passive To')

years<-seq(min(true.data$Year),max(true.data$Year),1)
ggplot(real.data,aes(Year,y,colour=IO))+stat_smooth(method='loess')+geom_point(data=new.data,aes(Bin,new.val,size=log(n,5)))+facet_grid(DO~Type)+geom_line(aes(real.data$Year,rep(ord,length(real.data$Year)),colour='ord'))+geom_line(aes(real.data$Year,rep(1-ord,length(real.data$Year)),colour='1-ord'))

names=c('fita2b2g2d2o2','fita2b1g2d2o2','fita2b1g1d2o2','fita2b1g1d1o2','fita2b1g1d1o1','fita2b1gTd1o1','fita2b1gTdAo1','fita2b1gTdAoA','fita2b1gTdAo0','fita2b1gTd0o0','fita2b1g0d0o0','fita2b0g0d0o0','fita1b0g0d0o0')
afit<-anova(fita2b2g2d2o2,fita2b1g2d2o2,fita2b1g1d2o2,fita2b1g1d1o2,fita2b1g1d1o1,fita2b1gTd1o1,fita2b1gTdAo1,fita2b1gTdAoA,fita2b1gTdAo0,fita2b1gTd0o0,fita2b1g0d0o0,fita2b0g0d0o0,fita1b0g0d0o0)
tab<-as.matrix(cbind(afit[,1],afit[,2],afit[,3],afit[,4],afit[,5],p.adjust(afit[,5]),p.adjust(afit[,5])<=.05)*('*'))
rownames(tab)<-names
colnames(tab)<-c('Total Df','Deviance','Chisq','Df','Pr(>Chisq)','Adjusted Pr(>Chisq)','Significant')
options(scipen=5)
print(tab,na.print='',digits=4)
