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
joint.data$DO <- factor(joint.data$DO

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
ord = AD1500/Order1500

full_ll<-function(aToAD=.6,aToDA=.6,aAccAct=-3.1,aAccPas=-3.1,aToPas=.6,bToAD=3.9,bToDA=3.9,bAccAct=2.7,bAccPas=2.7,bToPas=3.9,gToAD=-1.4,gToDA=-1.4,gAccAct=-.7,gAccPas=-.7,gToPas=-1.4,dToAD=0,dToDA=0,dAccAct=0,dAccPas=0,dToPas=0,orderToAD=ord,orderToDA=ord,orderAccAct=ord,orderAccPas=ord,orderToPas=ord){
	p1<-((1-(1-orderToAD)*data1$iodo)/(1+exp(-(aToAD+bToAD*data1$x+gToAD*data1$iosum+dToAD*data1$iosum*data1$x))))
	p2<-((1-(1-orderToDA)*data2$iodo)/(1+exp(-(aToDA+bToDA*data2$x+gToDA*data2$iosum+dToDA*data2$iosum*data2$x))))*
	    ((1-(1-orderAccAct)*data2$iopro)-((1-(1-orderAccAct)*data2$iopro)/(1+exp(-(aAccAct+bAccAct*data2$x+gAccAct*data2$iosum+dAccAct*data2$iosum*data2$x)))))*data2$donoun
	p3<-((1-(1-orderAccPas)*data3$iopro)/(1+exp(-(aAccPas+bAccPas*data3$x+gAccPas*data3$iosum+dAccPas*data3$iosum*data3$x))))*data3$donoun
	p4<-((1-(1-orderToPas)*data4$iopro)/(1+exp(-(aToPas+bToPas*data4$x+gToPas*data4$iosum+dToPas*data4$iosum*data4$x))))
	sum1<--sum(stats::dbinom(data1$y, 1, p1,log=TRUE))
	sum2<--sum(stats::dbinom(data2$y, 1, p2,log=TRUE))
	sum3<--sum(stats::dbinom(data3$y, 1, p3,log=TRUE))
	sum4<--sum(stats::dbinom(data4$y, 1, p4,log=TRUE))
	results<-sum(sum1,sum2,sum3,sum4)
	if (is.finite(results)) { 
		if (is.nan(results)){
			print(c('NAN',results,aToAD,aToDA,aAccAct,aAccPas,aToPas,bToAD,bToDA,bAccAct,bAccPas,bToPas,gToAD,gToDA,gAccAct,gAccPas,gToPas,dToAD,dToDA,dAccAct,dAccPas,dToPas,orderToAD,orderToDA,orderAccAct,orderAccPas,orderToPas))
			results=NA
			
		}
	}
	else{
		print(c(results,aToAD,aToDA,aAccAct,aAccPas,aToPas,bToAD,bToDA,bAccAct,bAccPas,bToPas,gToAD,gToDA,gAccAct,gAccPas,gToPas,dToAD,dToDA,dAccAct,dAccPas,dToPas,orderToAD,orderToDA,orderAccAct,orderAccPas,orderToPas))
		results=NA
	} 
	results
}

Order1500 <- margin.table(table(subset(joint.data,(Type=='AD'|Type=='DA')&Year>1500&IO=='IONoun'&DO=='DONoun')$Type))
AD1500 <- margin.table(table(subset(joint.data,Type=='AD'&Year>1500&IO=='IONoun'&DO=='DONoun')$Type))
ord = AD1500/Order1500

fitfull<-mle2(full_ll,default.start=TRUE,optimizer='optim',method='L-BFGS-B',control=list(trace=4,REPORT=1,maxit=1000),
			          lower = list(aToAD=-Inf,aToDA=-Inf,aAccAct=-Inf,aAccPas=-Inf,aToPas=-Inf,bToAD=-Inf,bToDA=-Inf,bAccAct=-Inf,bAccPas=-Inf,bToPas=-Inf,gToAD=-Inf,gToDA=-Inf,gAccAct=-Inf,gAccPas=-Inf,gToPas=-Inf,dToAD=-Inf,dToDA=-Inf,dAccAct=-Inf,dAccPas=-Inf,dToPas=-Inf,orderToAD=0.0001,orderToDA=0.0001,orderAccAct=0.0001,orderAccPas=0.0001,orderToPas=0.0001),
                                  upper = list(aToAD=Inf,aToDA=Inf,aAccAct=Inf,aAccPas=Inf,aToPas=Inf,bToAD=Inf,bToDA=Inf,bAccAct=Inf,bAccPas=Inf,bToPas=Inf,gToAD=Inf,gToDA=Inf,gAccAct=Inf,gAccPas=Inf,gToPas=Inf,dToAD=Inf,dToDA=Inf,dAccAct=Inf,dAccPas=Inf,dToPas=Inf,orderToAD=0.9999,orderToDA=0.9999,orderAccAct=0.9999,orderAccPas=0.9999,orderToPas=0.9999))

