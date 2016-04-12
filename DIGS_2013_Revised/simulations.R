library(plyr)
library(ggplot2)
library(epicalc)
library(knitr)
library(nnet)
library(splines)
library(MASS)
library(xtable)
library(bbmle)

x<-seq(-10,10,.1)
ylog<-1/(1+exp(-(x)))
yder<-exp(-x)/((exp(-x)+1)^2)
g <- data.frame(x=x,ylog=ylog,yder=yder)
ggplot(g,aes(x,ylog,linetype='Logistic Function'))+geom_line()+geom_line(aes(x,yder,linetype='First Derivative'))+ylab('Probability')+scale_linetype_discrete(name="")

dit <- read.csv('adj.tsv',sep='\t')
oedit <- read.csv('OEadj.tsv',sep='\t')

levels(dit$Clause)[1]<-'ABS'
levels(oedit$Clause)[1]<-'ABS'

real<-subset(as.data.frame(rbind(dit,oedit)),!is.na(Verb))

real$NGenre<-real$Genre
levels(real$NGenre)[levels(real$NGenre)=='A']<-"FORMAL"
levels(real$NGenre)[levels(real$NGenre)=='B']<-"FORMAL"
levels(real$NGenre)[levels(real$NGenre)=='c']<-"INFORMAL"
levels(real$NGenre)[levels(real$NGenre)=='C']<-"FORMAL"
levels(real$NGenre)[levels(real$NGenre)=='D']<-"INFORMAL"
levels(real$NGenre)[levels(real$NGenre)=='E']<-"FORMAL"
levels(real$NGenre)[levels(real$NGenre)=='F']<-"FORMAL"
levels(real$NGenre)[levels(real$NGenre)=='G']<-"FORMAL"
levels(real$NGenre)[levels(real$NGenre)=='H']<-"FORMAL"
levels(real$NGenre)[levels(real$NGenre)=='I']<-"FORMAL"
levels(real$NGenre)[levels(real$NGenre)=='l']<-"INFORMAL"
levels(real$NGenre)[levels(real$NGenre)=='L']<-"FORMAL"
levels(real$NGenre)[levels(real$NGenre)=='M']<-"FORMAL"
levels(real$NGenre)[levels(real$NGenre)=='O']<-"FORMAL"
levels(real$NGenre)[levels(real$NGenre)=='P']<-"FORMAL"
levels(real$NGenre)[levels(real$NGenre)=='Q']<-"FORMAL"
levels(real$NGenre)[levels(real$NGenre)=='R']<-"FORMAL"
levels(real$NGenre)[levels(real$NGenre)=='t']<-"INFORMAL"
levels(real$NGenre)[levels(real$NGenre)=='T']<-"FORMAL"
levels(real$NGenre)[levels(real$NGenre)=='U']<-"FORMAL"
levels(real$NGenre)[levels(real$NGenre)=='W']<-"FORMAL"
levels(real$NGenre)[levels(real$NGenre)=='X']<-"WEIRD"
levels(real$NGenre)[levels(real$NGenre)=='Y']<-"WEIRD"
levels(real$NGenre)[levels(real$NGenre)=='Z']<-"WEIRD"

real$NDat<-real$Dat

levels(real$NDat)[levels(real$NDat)=='DatDefinite']=c('DatNoun')
levels(real$NDat)[levels(real$NDat)=='DatIndefinite']=c('DatNoun')
levels(real$NDat)[levels(real$NDat)=='DatName']=c('DatNoun')
levels(real$NDat)[levels(real$NDat)=='DatConj']=c('DatNoun')
levels(real$NDat)[levels(real$NDat)=='DatNull']=c('DatNull')
levels(real$NDat)[levels(real$NDat)=='DatDPronoun']=c('DatNoun')
levels(real$NDat)[levels(real$NDat)=='DatWHEmpty']=c('DatEmpty')
levels(real$NDat)[levels(real$NDat)=='DatWHIndefinite']=c('DatNoun')
levels(real$NDat)[levels(real$NDat)=='DatWHPronoun']=c('DatPronoun')
levels(real$NDat)[levels(real$NDat)=='DatWPIndefinite']=c('DatNoun')
levels(real$NDat)[levels(real$NDat)=='DatWPPronoun']=c('DatPronoun')

real$DatWH<-real$Dat

levels(real$DatWH)[levels(real$DatWH)=='DatDefinite']=c('DatNotWH')
levels(real$DatWH)[levels(real$DatWH)=='DatIndefinite']=c('DatNotWH')
levels(real$DatWH)[levels(real$DatWH)=='DatName']=c('DatNotWH')
levels(real$DatWH)[levels(real$DatWH)=='DatConj']=c('DatNotWH')
levels(real$DatWH)[levels(real$DatWH)=='DatNull']=c('DatNotWH')
levels(real$DatWH)[levels(real$DatWH)=='DatDPronoun']=c('DatNotWH')
levels(real$DatWH)[levels(real$DatWH)=='DatEmpty']=c('DatNotWH')
levels(real$DatWH)[levels(real$DatWH)=='DatPronoun']=c('DatNotWH')
levels(real$DatWH)[levels(real$DatWH)=='DatWHEmpty']=c('DatWH')
levels(real$DatWH)[levels(real$DatWH)=='DatWHIndefinite']=c('DatWH')
levels(real$DatWH)[levels(real$DatWH)=='DatWHPronoun']=c('DatWH')
levels(real$DatWH)[levels(real$DatWH)=='DatWPIndefinite']=c('DatWH')
levels(real$DatWH)[levels(real$DatWH)=='DatWPPronoun']=c('DatWH')


real$NAcc<-real$Acc

levels(real$NAcc)[levels(real$NAcc)=='AccDefinite']=c('AccNoun')
levels(real$NAcc)[levels(real$NAcc)=='AccIndefinite']=c('AccNoun')
levels(real$NAcc)[levels(real$NAcc)=='AccName']=c('AccNoun')
levels(real$NAcc)[levels(real$NAcc)=='AccConj']=c('AccNoun')
levels(real$NAcc)[levels(real$NAcc)=='AccNull']=c('AccNull')
levels(real$NAcc)[levels(real$NAcc)=='AccDPronoun']=c('AccNoun')
levels(real$NAcc)[levels(real$NAcc)=='AccPronoun']=c('AccPronoun')
levels(real$NAcc)[levels(real$NAcc)=='AccEmpty']=c('AccEmpty')
levels(real$NAcc)[levels(real$NAcc)=='AccCP']=c('AccCP')
levels(real$NAcc)[levels(real$NAcc)=='AccWHEmpty']=c('AccEmpty')
levels(real$NAcc)[levels(real$NAcc)=='AccWHIndefinite']=c('AccNoun')
levels(real$NAcc)[levels(real$NAcc)=='AccWHPronoun']=c('AccPronoun')

real$AccWH<-real$Acc

levels(real$AccWH)[levels(real$AccWH)=='AccDefinite']=c('AccNotWH')
levels(real$AccWH)[levels(real$AccWH)=='AccIndefinite']=c('AccNotWH')
levels(real$AccWH)[levels(real$AccWH)=='AccName']=c('AccNotWH')
levels(real$AccWH)[levels(real$AccWH)=='AccConj']=c('AccNotWH')
levels(real$AccWH)[levels(real$AccWH)=='AccNull']=c('AccNotWH')
levels(real$AccWH)[levels(real$AccWH)=='AccDPronoun']=c('AccNotWH')
levels(real$AccWH)[levels(real$AccWH)=='AccPronoun']=c('AccNotWH')
levels(real$AccWH)[levels(real$AccWH)=='AccEmpty']=c('AccNotWH')
levels(real$AccWH)[levels(real$AccWH)=='AccCP']=c('AccNotWH')
levels(real$AccWH)[levels(real$AccWH)=='AccWHEmpty']=c('AccWH')
levels(real$AccWH)[levels(real$AccWH)=='AccWHIndefinite']=c('AccWH')
levels(real$AccWH)[levels(real$AccWH)=='AccWHPronoun']=c('AccWH')

real$NNom<-real$Nom

levels(real$NNom)[levels(real$NNom)=='NomDefinite']=c('NomNoun')
levels(real$NNom)[levels(real$NNom)=='NomIndefinite']=c('NomNoun')
levels(real$NNom)[levels(real$NNom)=='NomConj']=c('NomNoun')
levels(real$NNom)[levels(real$NNom)=='NomName']=c('NomNoun')
levels(real$NNom)[levels(real$NNom)=='NomNull']=c('NomNull')
levels(real$NNom)[levels(real$NNom)=='NomEmpty']=c('NomEmpty')
levels(real$NNom)[levels(real$NNom)=='NomDPronoun']=c('NomPronoun')
levels(real$NNom)[levels(real$NNom)=='NomWHEmpty']=c('NomEmpty')
levels(real$NNom)[levels(real$NNom)=='NomWHIndefinite']=c('NomNoun')
levels(real$NNom)[levels(real$NNom)=='NomWHPronoun']=c('NomPronoun')

real$NomWH<-real$Nom

levels(real$NomWH)[levels(real$NomWH)=='NomDefinite']=c('NomNotWH')
levels(real$NomWH)[levels(real$NomWH)=='NomIndefinite']=c('NomNotWH')
levels(real$NomWH)[levels(real$NomWH)=='NomConj']=c('NomNotWH')
levels(real$NomWH)[levels(real$NomWH)=='NomName']=c('NomNotWH')
levels(real$NomWH)[levels(real$NomWH)=='NomNull']=c('NomNotWH')
levels(real$NomWH)[levels(real$NomWH)=='NomEmpty']=c('NomNotWH')
levels(real$NomWH)[levels(real$NomWH)=='NomDPronoun']=c('NomNotWH')
levels(real$NomWH)[levels(real$NomWH)=='NomWHEmpty']=c('NomWH')
levels(real$NomWH)[levels(real$NomWH)=='NomWHIndefinite']=c('NomWH')
levels(real$NomWH)[levels(real$NomWH)=='NomWHPronoun']=c('NomWH')


real$NVerb<-real$Verb

levels(real$NVerb)[levels(real$NVerb)=='ALLOT']<-c('PROMISE')
levels(real$NVerb)[levels(real$NVerb)=='APPOINT']<-c('PROMISE')
levels(real$NVerb)[levels(real$NVerb)=='ASSIGN']<-c('PROMISE')
levels(real$NVerb)[levels(real$NVerb)=='AYEVEN']<-c('GIVE')
levels(real$NVerb)[levels(real$NVerb)=='BEHIEGHT']<-c('PROMISE')
levels(real$NVerb)[levels(real$NVerb)=='BEQUEATH']<-c('PROMISE')
levels(real$NVerb)[levels(real$NVerb)=='BETAKE']<-c('GIVE')
levels(real$NVerb)[levels(real$NVerb)=='CARRY']<-c('SEND')
levels(real$NVerb)[levels(real$NVerb)=='DELIVER']<-c('SEND')
levels(real$NVerb)[levels(real$NVerb)=='FEED']<-c('GIVE')
levels(real$NVerb)[levels(real$NVerb)=='GIVE']<-c('GIVE')
levels(real$NVerb)[levels(real$NVerb)=='GRANT']<-c('PROMISE')
levels(real$NVerb)[levels(real$NVerb)=='LEND']<-c('GIVE')
levels(real$NVerb)[levels(real$NVerb)=='OFFER']<-c('PROMISE')
levels(real$NVerb)[levels(real$NVerb)=='OWE']<-c('PROMISE')
levels(real$NVerb)[levels(real$NVerb)=='PAY']<-c('GIVE')
levels(real$NVerb)[levels(real$NVerb)=='PROFFER']<-c('PROMISE')
levels(real$NVerb)[levels(real$NVerb)=='PROMISE']<-c('PROMISE')
levels(real$NVerb)[levels(real$NVerb)=='RESTORE']<-c('GIVE')
levels(real$NVerb)[levels(real$NVerb)=='RETURN']<-c('SEND')
levels(real$NVerb)[levels(real$NVerb)=='SELL']<-c('GIVE')
levels(real$NVerb)[levels(real$NVerb)=='SEND']<-c('SEND')
levels(real$NVerb)[levels(real$NVerb)=='SERVE']<-c('GIVE')
levels(real$NVerb)[levels(real$NVerb)=='SHOW']<-c('GIVE')
levels(real$NVerb)[levels(real$NVerb)=='VOUCHSAFE']<-c('PROMISE')
levels(real$NVerb)[levels(real$NVerb)=='YIELD']<-c('GIVE')

real$NAdj<-factor(real$Adj)

levels(real$NAdj)<-c(levels(real$NAdj),'ProIntervene','NounIntervene')

real$NAdj[real$NAcc=='AccPronoun'&real$NAdj=='DOIntervene']<-'ProIntervene'
real$NAdj[real$NAdj=='DOIntervene']<-'NounIntervene'

real$NAdj[real$NAcc=='AccPronoun'&real$NAdj=='PreverbDOIntervene']<-'ProIntervene'
real$NAdj[real$NAdj=='PreverbDOIntervene']<-'NounIntervene'

real$NAdj[real$NNom=='NomPronoun'&real$NAdj=='NomIntervene']<-'ProIntervene'
real$NAdj[real$NAdj=='NomIntervene']<-'NounIntervene'

real$NAdj[real$NNom=='NomPronoun'&real$NAdj=='PreverbNomIntervene']<-'ProIntervene'
real$NAdj[real$NAdj=='PreverbNomIntervene']<-'NounIntervene'

real$NAdj<-factor(real$NAdj)

levels(real$NAdj)[levels(real$NAdj)=='Adjacent']='Adjacent'
levels(real$NAdj)[levels(real$NAdj)=='NegIntervene']='OtherInterveners'
levels(real$NAdj)[levels(real$NAdj)=='OtherInterveners']='OtherInterveners'
levels(real$NAdj)[levels(real$NAdj)=='PreverbAdjacent']='Adjacent'
levels(real$NAdj)[levels(real$NAdj)=='PreverbAdvIntervene']='OtherInterveners'
levels(real$NAdj)[levels(real$NAdj)=='PreverbFiniteIntervene']='OtherInterveners'
levels(real$NAdj)[levels(real$NAdj)=='PreverbNegIntervene']='OtherInterveners'
levels(real$NAdj)[levels(real$NAdj)=='ProIntervene']='ProIntervene'
levels(real$NAdj)[levels(real$NAdj)=='NounIntervene']='NounIntervene'

real$isTo<-factor(real$PP)
levels(real$isTo)<-c(0,NA,1,1,1,1)
real$isTo<-as.numeric(as.character(real$isTo))

areal<-subset(real,NGenre!='POETRY'&NGenre!='WEIRD'&NGenre!='TRANSLATION'&NDat!='DatNull'&NDat!='DatEmpty'&NAcc!='AccNull'&NAcc!='AccCP'&Pas=='ACT')

areal$IO<-factor(areal$NDat)
areal$DO<-factor(areal$NAcc)

areal$Envir<-factor(paste(areal$DatVerb,areal$AccVerb,areal$DatAcc))

levels(areal$Envir)[levels(areal$Envir)=="DatV AccV AccDat"]="Active Theme--Recipient Verb"
levels(areal$Envir)[levels(areal$Envir)=="DatV AccV DatAcc"]="Active Recipient--Theme Verb"
levels(areal$Envir)[levels(areal$Envir)=="DatV NA NA"]=NA
levels(areal$Envir)[levels(areal$Envir)=="DatV VAcc AccDat"]=NA
levels(areal$Envir)[levels(areal$Envir)=="DatV VAcc DatAcc"]="Active Recipient Topicalisation"
levels(areal$Envir)[levels(areal$Envir)=="NA AccV NA"]=NA
levels(areal$Envir)[levels(areal$Envir)=="NA VAcc NA"]=NA
levels(areal$Envir)[levels(areal$Envir)=="VDat AccV AccDat"]="Active Theme Topicalisation"
levels(areal$Envir)[levels(areal$Envir)=="VDat NA NA"]=NA
levels(areal$Envir)[levels(areal$Envir)=="VDat VAcc AccDat"]="Active Verb Theme--Recipient"
levels(areal$Envir)[levels(areal$Envir)=="VDat VAcc DatAcc"]="Active Verb Recipient--Theme"
levels(areal$Envir)[levels(areal$Envir)=="VDat VAcc NA"]=NA

thereal<-subset(real,NGenre!='POETRY'&NGenre!='WEIRD'&NGenre!='TRANSLATION'&NDat!='DatNull'&NDat!='DatEmpty'&NAcc=='AccNull'&NNom!='NomNull'&Pas=='PAS')
thereal$IO<-factor(thereal$NDat)
thereal$DO<-factor(thereal$NNom)
thereal$Envir<-factor(paste(thereal$DatVerb,thereal$NomVerb,thereal$NomDat))
levels(thereal$Envir)[levels(thereal$Envir)=="DatV NA NA"]=NA
levels(thereal$Envir)[levels(thereal$Envir)=="DatV NomV DatNom"]="Theme Passive Recipient Topicalisation"
levels(thereal$Envir)[levels(thereal$Envir)=="DatV NomV NomDat"]="Theme Passive Theme Topicalisation"
levels(thereal$Envir)[levels(thereal$Envir)=="DatV VNom DatNom"]="Theme Passive Recipient Topicalisation"
levels(thereal$Envir)[levels(thereal$Envir)=="DatV VNom NomDat"]=NA
levels(thereal$Envir)[levels(thereal$Envir)=="NA NA NA"]=NA
levels(thereal$Envir)[levels(thereal$Envir)=="NA NomV NA"]=NA
levels(thereal$Envir)[levels(thereal$Envir)=="NA VNom NA"]=NA
levels(thereal$Envir)[levels(thereal$Envir)=="VDat NA NA"]=NA
levels(thereal$Envir)[levels(thereal$Envir)=="VDat NomV NomDat"]="Theme Passive Theme Verb Recipient"
levels(thereal$Envir)[levels(thereal$Envir)=="VDat VNom DatNom"]="Theme Passive Verb Recipient--Theme"
levels(thereal$Envir)[levels(thereal$Envir)=="VDat VNom NomDat"]="Theme Passive Verb Theme--Recipient"

recreal<-subset(real,NGenre!='POETRY'&NGenre!='WEIRD'&NGenre!='TRANSLATION'&NDat=='DatNull'&NAcc!='AccCP'&NAcc!='AccNull'&NNom!='NomNull'&Pas=='PAS')
recreal$IO<-factor(recreal$NNom)
recreal$DO<-factor(recreal$NAcc)
recreal$Envir<-factor(paste(recreal$NomVerb,recreal$AccVerb,recreal$NomAcc))
levels(recreal$Envir)[levels(recreal$Envir)=="NA VAcc NA"]=NA
levels(recreal$Envir)[levels(recreal$Envir)=="NomV AccV AccNom"]='Recipient Passive Theme Topicalisation'
levels(recreal$Envir)[levels(recreal$Envir)=="NomV NA NA"]=NA
levels(recreal$Envir)[levels(recreal$Envir)=="NomV VAcc NomAcc"]="Recipient Passive Recipient Verb Theme"
levels(recreal$Envir)[levels(recreal$Envir)=="VNom VAcc NomAcc"]="Recipient Passive Verb Recipient--Theme"

nreal<-as.data.frame(rbind(areal,thereal,recreal))

greal<-subset(nreal,Envir == "Active Recipient Topicalisation"|Envir=="Active Theme Topicalisation"|Envir=="Active Verb Theme--Recipient" | Envir == "Active Verb Recipient--Theme" | Envir == "Theme Passive Theme Verb Recipient")

greal$Envir<-factor(greal$Envir)

levels(greal$Envir)[levels(greal$Envir)=="Active Recipient Topicalisation"]="(To) recipient, I gave theme"
levels(greal$Envir)[levels(greal$Envir)=="Active Theme Topicalisation"]="Theme, I gave (to) recipient"
levels(greal$Envir)[levels(greal$Envir)=="Active Verb Theme--Recipient"]="I gave theme (to) recipient"      
levels(greal$Envir)[levels(greal$Envir)=="Active Verb Recipient--Theme"]="I gave (to) recipient theme"
levels(greal$Envir)[levels(greal$Envir)=="Theme Passive Theme Verb Recipient"]="Theme was given (to) recipient"

greal$IO<-factor(greal$IO)
levels(greal$IO)<-c('Recipient Noun','Recipient Pronoun')

greal$Eras<-cut(greal$YoC,seq(850,2000,50),seq(875,1975,50))
greal$Eras<-as.numeric(as.character(greal$Eras))

levels(greal$DO)<-c('Theme Noun','Theme Empty','Theme Pronoun','Theme Noun','Theme Pronoun','Theme Empty')

greal<-subset(greal,Envir %in% c('I gave (to) recipient theme','I gave theme (to) recipient')&IO=='Recipient Noun'&DO=='Theme Noun'&NVerb!='SEND')

mreal<-subset(greal,NVerb!='SEND'&IO=='Recipient Noun'&DO=='Theme Noun')
mreal$SYear<-(mreal$YoC-mean(mreal$YoC))/sd(mreal$YoC)

dps<-ddply(mreal,.(Eras,Envir,IO),summarize,Val=sum(isTo)/sum(!is.na(isTo)),num=sum(!is.na(isTo)))

dps$num[dps$num<50]=1
dps$num[dps$num>=50]=2

g=ggplot(mreal,aes(YoC,isTo,linetype=Envir))+stat_smooth(method=loess,colour="black")+geom_point(data=dps,aes(Eras,Val,pch=Envir,size=factor(num)))+coord_cartesian(ylim=c(-0.1,1.1),xlim=c(800,1900))+scale_size_discrete(name="Number of Tokens (per 10 Years)",range=c(2,3))+scale_x_continuous(name="Year of Composition",breaks=seq(800,1900,100))+scale_y_continuous(name='Percent "To" Use',breaks=c(0,.25,.5,.75,1),labels=c("0%","25%","50%","75%","100%"))

suppressWarnings(print(g))

trmod<-glm(data=subset(mreal,Envir!='I gave (to) recipient theme'&YoC<=1750),isTo~YoC,family=binomial)

rtmod<-glm(data=subset(mreal,Envir=='I gave (to) recipient theme'),isTo~x+I(x^2),family=binomial)
pred.rt<-data.frame(YoC=seq(1000,2000,1))
pred.rt$isTo<-predict(rtmod,newdata=pred,type='response')
pred.rt$YoC[pred.rt$isTo==max(pred.rt$isTo)]

heavydf<-subset(mreal,YoC>1400&YoC<=1800&Envir=='I gave (to) recipient theme'&IO=='Recipient Noun'&DO=='Theme Noun')
levels(heavydf$AccSize)[levels(heavydf$AccSize)=="MORE"]="25"
heavydf$AccSize<-as.numeric(as.character(heavydf$AccSize))
intmod<-glm(data=heavydf,isTo~YoC*(AccSize+AccCP),family=binomial)
fullmod<-glm(data=heavydf,isTo~YoC+AccSize+AccCP,family=binomial)
heavymod<-glm(data=heavydf,isTo~AccSize+AccCP,family=binomial)
nullmod<-glm(data=heavydf,isTo~1,family=binomial)
atab<-anova(nullmod,heavymod,fullmod,intmod,test="LRT")
atab<-cbind(atab,AIC(nullmod,heavymod,fullmod,intmod)[,2])
atab<-cbind(atab,BIC(nullmod,heavymod,fullmod,intmod)[,2])
colnames(atab)[6]<-'AIC'
colnames(atab)[7]<-'BIC'
rownames(atab)<-c('Null Model','Only Heaviness of Theme','Year and Heaviness of Theme','Year and Heaviness of Theme Interaction')
print(xtable(atab,caption='Model Comparison Using Log-likelihood Ratio Test, AIC and BIC \\label{tab:modcomp}',table.placement='!h'))

rtreal<-subset(mreal,Envir=='I gave (to) recipient theme')

rtreal$Envir<-factor(rtreal$Envir)
rtpoints<-ddply(rtreal,.(Eras,Envir),summarize,prob=sum(isTo)/sum(!is.na(isTo)),num=sum(!is.na(isTo)))

trreal<-subset(mreal,Envir=='I gave theme (to) recipient')

trreal$Envir<-factor(trreal$Envir)
trpoints<-ddply(trreal,.(Eras,Envir),summarize,prob=sum(isTo)/sum(!is.na(isTo)),num=sum(!is.na(isTo)))

points<-as.data.frame(rbind(rtpoints,trpoints))

points$num[points$num>=50]<-50

nreal<-as.data.frame(rbind(rtreal,trreal))

#anova(pasmod1,pasmod2,test='LRT')

#rtopmod2<-glm(data=rtopreal,isTo~poly(YoC,2),family=binomial)
#rtopmod1<-glm(data=rtopreal,isTo~YoC,family=binomial)

#anova(rtopmod1,rtopmod2,test='LRT')


dataTR<-trreal
dataRT<-rtreal

dataRT.y<-dataRT$isTo
dataTR.y<-dataTR$isTo

dataRT.x<-dataRT$SYear
dataTR.x<-dataTR$SYear

aUp=coef(glm(data=trreal,isTo~SYear,family=binomial))[1]
bUp=coef(glm(data=trreal,isTo~SYear,family=binomial))[2]
a1=coef(glm(data=subset(rtreal,YoC<=1400),isTo~SYear,family=binomial))[1]
b1=coef(glm(data=subset(rtreal,YoC<=1400),isTo~SYear,family=binomial))[2]
a2=coef(glm(data=subset(rtreal,YoC>=1500),isTo~SYear,family=binomial))[1]
b2=coef(glm(data=subset(rtreal,YoC>=1500),isTo~SYear,family=binomial))[2]*-1

prob_ll_full<-function(aUp=0,bUp=0,a1=0,a2=0,b1=0,b2=0){
  pTR<-(1/(1+exp(-((aUp+bUp*dataTR.x)))))
  pRT<-(1/(1+exp(-((a1+b1*dataRT.x)))))*(1/(1+exp(-((a2-b2*dataRT.x)))))
  sumTR=-sum(stats::dbinom(dataTR.y, 1, pTR,log=TRUE))
  sumRT=-sum(stats::dbinom(dataRT.y, 1, pRT,log=TRUE))
  results<-sumTR+sumRT
  # print(c(aUp,bUp,a1,a2,b1,b2))
  # print(results)
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

prob_fit_full<-mle2(prob_ll_full,start=list(aUp=aUp,bUp=bUp,a1=a1,b1=b1,a2=a2,b2=b2),method="L-BFGS-B",
                    lower=c(aUp=-Inf,bUp=0,a1=-Inf,b1=0,a2=-Inf,b2=0),
                    control=list(trace=1,REPORT=5,maxit=1000))

prob_ll_downdiff<-function(aUp=0,bUp=0,a1=0,a2=0,b2=0){
  pTR<-(1/(1+exp(-((aUp+bUp*dataTR.x)))))
  pRT<-(1/(1+exp(-((a1+bUp*dataRT.x)))))*(1/(1+exp(-((a2-b2*dataRT.x)))))
  sumTR=-sum(stats::dbinom(dataTR.y, 1, pTR,log=TRUE))
  sumRT=-sum(stats::dbinom(dataRT.y, 1, pRT,log=TRUE))
  results<-sumTR+sumRT
  # print(c(aUp,bUp,a1,a2,b1,b2))
  # print(results)
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

prob_fit_downdiff<-mle2(prob_ll_downdiff,start=list(aUp=aUp,bUp=bUp,a1=a1,a2=a2,b2=b2),method="L-BFGS-B",
                        lower=c(aUp=-Inf,bUp=0,a1=-Inf,a2=-Inf,b2=0),
                        control=list(trace=1,REPORT=5,maxit=1000))

anova(prob_fit_downdiff,prob_fit_full)

prob_sim<-function(aUp=0,bUp=0,a1=0,a2=0,b1=0,b2=0){
  pTR<-(1/(1+exp(-((aUp+bUp*dataTR.x)))))
  pRT<-(1/(1+exp(-((a1+b1*dataRT.x)))))*(1/(1+exp(-((a2-b2*dataRT.x)))))
  simTR.y=stats::rbinom(length(pTR), 1, pTR)
  simRT.y=stats::rbinom(length(pRT), 1, pRT)
  sim_ll_full<-function(aUp=0,bUp=0,a1=0,a2=0,b1=0,b2=0){
    pTR<-(1/(1+exp(-((aUp+bUp*dataTR.x)))))
    pRT<-(1/(1+exp(-((a1+b1*dataRT.x)))))*(1/(1+exp(-((a2-b2*dataRT.x)))))
    sumTR=-sum(stats::dbinom(simTR.y, 1, pTR,log=TRUE))
    sumRT=-sum(stats::dbinom(simRT.y, 1, pRT,log=TRUE))
    results<-sumTR+sumRT
    # print(results)
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
  
  sim_ll_downdiff<-function(aUp=0,bUp=0,a1=0,a2=0,b2=0){
    pTR<-(1/(1+exp(-((aUp+bUp*dataTR.x)))))
    pRT<-(1/(1+exp(-((a1+bUp*dataRT.x)))))*(1/(1+exp(-((a2-b2*dataRT.x)))))
    sumTR=-sum(stats::dbinom(simTR.y, 1, pTR,log=TRUE))
    sumRT=-sum(stats::dbinom(simRT.y, 1, pRT,log=TRUE))
    results<-sumTR+sumRT
    # print(c(aUp,bUp,a1,a2,b1,b2))
    # print(results)
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
  
  full_mod<-mle2(sim_ll_full,start=list(aUp=aUp,bUp=bUp,a1=a1,b1=b1,a2=a2,b2=b2),method="L-BFGS-B",
                 lower=c(aUp=-Inf,bUp=0,a1=-Inf,b1=0,a2=-Inf,b2=0),
                 control=list(trace=1,REPORT=5,maxit=1000))
  downdiff_mod<-mle2(sim_ll_downdiff,start=list(aUp=aUp,bUp=bUp,a1=a1,a2=a2,b2=b2),method="L-BFGS-B",
                          lower=c(aUp=-Inf,bUp=0,a1=-Inf,a2=-Inf,b2=0),
                          control=list(trace=1,REPORT=5,maxit=1000))
  results<-anova(downdiff_mod,full_mod)[2,5]
  print(c(aUp,bUp,a1,a2,b1,b2))
  print(coef(full_mod))
  print(coef(downdiff_mod))
  results
}

aUp<-coef(prob_fit_downdiff)[1]
bUp<-coef(prob_fit_downdiff)[2]
a1<-coef(prob_fit_downdiff)[3]
a2<-coef(prob_fit_downdiff)[4]
b1<-bUp
b2<-coef(prob_fit_downdiff)[5]

newx<-4

newa<-a1*(newx/bUp)
newb<-bUp*(newx/bUp)

pred<-expand.grid(x=seq(-3,2,.01),Envir=c('RT','TR'))
pred.rt<-subset(pred,Envir=='RT')
pred.tr<-subset(pred,Envir=='TR')
pred.tr$y<-(1/(1+exp(-((aUp+bUp*pred.tr$x)))))
pred.rt$y<-(1/(1+exp(-((newa+newb*pred.rt$x)))))*(1/(1+exp(-((a2-b2*pred.rt$x)))))
pred<-as.data.frame(rbind(pred.rt,pred.tr))
ggplot(pred,aes(x,y,colour=Envir))+geom_line()

prob_sim(aUp=aUp,bUp=bUp,a1=newa,a2=a2,b1=newb,b2=b2)  


newresults<-expand.grid(d=seq(-2,2,.05),id=seq(1,5))

newresults$p.value<-rep(1,dim(newresults)[1])

for (i in 1:dim(newresults)[1]){
  print(i)
  newresults$p.value[i] <- prob_sim(aUp=aUp,bUp=bUp,a1=a1,a2=a2,b1=(bUp+newresults$d[i]),b2=b2)  
}


