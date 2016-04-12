library(dplyr)
library(ggplot2)
library(epicalc)
library(knitr)
library(nnet)
library(splines)
library(MASS)
library(xtable)
library(bbmle)
library(reshape2)

#Generate graph for first derivative model
x<-seq(-10,10,.01)
ylog<-1/(1+exp(-(x)))
yder<-exp(-x)/((exp(-x)+1)^2)
g <- data.frame(x=x,ylog=ylog,yder=yder)
ggplot(g,aes(x,ylog,linetype='Logistic Function'))+geom_line()+geom_line(aes(x,yder,linetype='First Derivative'))+ylab('Probability')+scale_linetype_discrete(name="")

#Load data
dit <- read.csv('adj.tsv',sep='\t')
oedit <- read.csv('OEadj.tsv',sep='\t')
oedit <- as.data.frame(cbind(oedit[,1],oedit[,3:51]))
colnames(oedit)[1]<-'ID'

#Fill in ABS IP labels
levels(dit$Clause)[1]<-'ABS'
levels(oedit$Clause)[1]<-'ABS'

#Remove irrelevant examples
real<-subset(as.data.frame(rbind(dit,oedit)),!is.na(Verb))

#Process data, combining more specific codes into more general ones, through line 205
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

#Remove unusual texts (e.g. poetry, poor translation), non-ditransitive tokens, and 
#passive tokens (since the paper only deals with the change in the active)

areal<-subset(real,NGenre!='POETRY'&NGenre!='WEIRD'&NGenre!='TRANSLATION'&NDat!='DatNull'&NDat!='DatEmpty'&NAcc!='AccNull'&NAcc!='AccCP'&Pas=='ACT')

#Code word order and similar effects for active sentences

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

#Give meaningful names to the contexts
levels(areal$Envir)[levels(areal$Envir)=="Active Recipient Topicalisation"]="(To) recipient, I gave theme"
levels(areal$Envir)[levels(areal$Envir)=="Active Theme Topicalisation"]="Theme, I gave (to) recipient"
levels(areal$Envir)[levels(areal$Envir)=="Active Verb Theme--Recipient"]="I gave theme (to) recipient"      
levels(areal$Envir)[levels(areal$Envir)=="Active Verb Recipient--Theme"]="I gave (to) recipient theme"
levels(areal$Envir)[levels(areal$Envir)=="Theme Passive Theme Verb Recipient"]="Theme was given (to) recipient"

#Create a factor for the IO variable
areal$IO<-factor(areal$IO)
levels(areal$IO)<-c('Recipient Noun','Recipient Pronoun')

#Create eras for data summary table and data points on graphs
areal$Eras<-cut(areal$YoC,seq(850,2000,50),seq(875,1975,50))
areal$Eras<-as.numeric(as.character(areal$Eras))

#Give sensible names to the DO variable
levels(areal$DO)<-c('Theme Noun','Theme Empty','Theme Pronoun','Theme Noun','Theme Pronoun','Theme Empty')

#Subset to only the relevant data, excluding topicalisation and pronominal data
mreal<-subset(areal,Envir %in% c('I gave (to) recipient theme','I gave theme (to) recipient')&IO=='Recipient Noun'&DO=='Theme Noun'&NVerb!='SEND')

#Create a standardised Year variable to help in model fitting
mreal$SYear<-(mreal$YoC-mean(mreal$YoC))/sd(mreal$YoC)

#Create models and table for evaluating the Heavy NP Shift explanation for the recipient--theme order
heavydf<-subset(mreal,YoC>1400&Envir=='I gave (to) recipient theme'&IO=='Recipient Noun'&DO=='Theme Noun')
levels(heavydf$AccSize)[levels(heavydf$AccSize)=="MORE"]="25"
heavydf$AccSize<-as.numeric(as.character(heavydf$AccSize))
fullmod<-glm(data=heavydf,isTo~YoC+AccSize+AccCP,family=binomial)
heavymod<-glm(data=heavydf,isTo~AccSize+AccCP,family=binomial)
nullmod<-glm(data=heavydf,isTo~1,family=binomial)
atab<-anova(nullmod,heavymod,fullmod,test="LRT")
atab<-cbind(atab,AIC(nullmod,heavymod,fullmod)[,2])
atab<-cbind(atab,BIC(nullmod,heavymod,fullmod)[,2])
colnames(atab)[6]<-'AIC'
colnames(atab)[7]<-'BIC'
rownames(atab)<-c('Only Heaviness of Theme','Year and Heaviness of Theme')
xtable(atab,digits=2,caption='Model Comparison Using Log-likelihood Ratio Test, AIC and BIC \\label{tab:modcomp}',table.placement='!h')

#Create summaries for the summary tables and graph data points
rtreal<-subset(mreal,Envir=='I gave (to) recipient theme')

rtreal$Envir<-factor(rtreal$Envir)

trreal<-subset(mreal,Envir=='I gave theme (to) recipient')

trreal$Envir<-factor(trreal$Envir)


nreal<-as.data.frame(rbind(rtreal,trreal))

#Create data sets for model fiting
dataTR<-trreal
dataRT<-rtreal

dataRT.y<-dataRT$isTo
dataTR.y<-dataTR$isTo

dataRT.x<-dataRT$SYear
dataTR.x<-dataTR$SYear

#Estimate starting values for model fits
aUp=coef(glm(data=trreal,isTo~SYear,family=binomial))[1]
bUp=coef(glm(data=trreal,isTo~SYear,family=binomial))[2]
a1=coef(glm(data=subset(rtreal,YoC<=1400),isTo~SYear,family=binomial))[1]
b1=coef(glm(data=subset(rtreal,YoC<=1400),isTo~SYear,family=binomial))[2]
a2=coef(glm(data=subset(rtreal,YoC>=1500),isTo~SYear,family=binomial))[1]
b2=coef(glm(data=subset(rtreal,YoC>=1500),isTo~SYear,family=binomial))[2]*-1

#Models for just the recipient--theme data
postma_full<-function(TRa=0,TRb=0,RTa=0,RTb=0,scale=0.25){
  pRT<-scale/(2+2*cosh((RTa+RTb*dataRT.x)))
  sumRT=-sum(stats::dbinom(dataRT.y, 1, pRT,log=TRUE))
  results<-sumRT
  if (is.finite(results)) { 
    if (is.nan(results)){
      results=NA
    }
  }
  else{
    results=NA
  } 
  print(c(TRa,TRb,RTa,RTb,scale))
  print(results)
  results
}

postma_fit<-mle2(postma_full,start=list(RTa=a1,RTb=b1,scale=2.1),method="L-BFGS-B",
                 lower=c(RTa=-Inf,RTb=0,scale=0),
                 upper=c(RTa=Inf,RTb=Inf,scale=4),
                 control=list(trace=0,REPORT=5,maxit=1000))


prob_ll<-function(a1=0,a2=0,b1=0,b2=0){
  pRT<-(1/(1+exp(-((a1+b1*dataRT.x)))))*(1/(1+exp(-((a2-b2*dataRT.x)))))
  sumRT=-sum(stats::dbinom(dataRT.y, 1, pRT,log=TRUE))
  results<-sumRT
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

prob_fit<-mle2(prob_ll,start=list(a1=a1,b1=b1,a2=a2,b2=b2),method="L-BFGS-B",
               lower=c(a1=-Inf,b1=0,a2=-Inf,b2=0),
               control=list(trace=1,REPORT=5,maxit=1000))
AIC(postma_fit,prob_fit)


#First derivative model
postma_full_ll<-function(TRa=0,TRb=0,RTa=0,RTb=0,scale=0.25){
  pTR<-1/(1+exp(-(TRa+TRb*dataTR.x)))
  pRT<-scale/(2+2*cosh((RTa+RTb*dataRT.x)))
  sumTR=-sum(stats::dbinom(dataTR.y, 1, pTR,log=TRUE))
  sumRT=-sum(stats::dbinom(dataRT.y, 1, pRT,log=TRUE))
  results<-sumTR+sumRT
  if (is.finite(results)) { 
    if (is.nan(results)){
      results=NA
    }
  }
  else{
    results=NA
  } 
  print(c(TRa,TRb,RTa,RTb,scale))
  print(results)
  results
}

postma_fit_full<-mle2(postma_full_ll,start=list(TRa=aUp,TRb=bUp,RTa=a1,RTb=b1,scale=2.1),method="L-BFGS-B",
                      lower=c(TRa=-Inf,TRb=0,RTa=-Inf,RTb=0,scale=0),
                      upper=c(TRa=Inf,TRb=Inf,RTa=Inf,RTb=Inf,scale=4),
                      control=list(trace=0,REPORT=5,maxit=1000))

# postma_orddiff_ll<-function(TRa=0,b=0,RTa=0,scale=0.25){
#   pTR<-1/(1+exp(-(TRa+b*dataTR.x)))
#   pRT<-scale/(2+2*cosh((RTa+b*dataRT.x)))
#   sumTR=-sum(stats::dbinom(dataTR.y, 1, pTR,log=TRUE))
#   sumRT=-sum(stats::dbinom(dataRT.y, 1, pRT,log=TRUE))
#   results<-sumTR+sumRT
#   if (is.finite(results)) { 
#     if (is.nan(results)){
#       results=NA
#     }
#   }
#   else{
#     results=NA
#   } 
#   print(c(TRa,b,RTa,scale))
#   print(results)
#   results
# }
# 
# postma_fit_orddiff<-mle2(postma_orddiff_ll,start=list(TRa=aUp,b=bUp,RTa=a1,scale=2.1),method="L-BFGS-B",
#                          lower=c(TRa=-Inf,b=0,RTa=-Inf,scale=0),
#                          upper=c(TRa=Inf,b=Inf,RTa=Inf,scale=4),
#                          control=list(trace=0,REPORT=5,maxit=1000))
# 
# postma_null_ll<-function(a=0,b=0,scale=0.25){
#   pTR<-1/(1+exp(-(a+b*dataTR.x)))
#   pRT<-scale/(2+2*cosh((a+b*dataRT.x)))
#   sumTR=-sum(stats::dbinom(dataTR.y, 1, pTR,log=TRUE))
#   sumRT=-sum(stats::dbinom(dataRT.y, 1, pRT,log=TRUE))
#   results<-sumTR+sumRT
#   if (is.finite(results)) { 
#     if (is.nan(results)){
#       results=NA
#     }
#   }
#   else{
#     results=NA
#   } 
#   print(c(a,b,scale))
#   print(results)
#   results
# }
# 
# postma_fit_null<-mle2(postma_null_ll,start=list(a=aUp,b=bUp,scale=2.1),method="L-BFGS-B",
#                       lower=c(a=-Inf,b=0,scale=0),
#                       upper=c(a=Inf,b=Inf,scale=4),
#                       control=list(trace=0,REPORT=5,maxit=1000))

#Probability Multiplication models
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

prob_ll_diffint<-function(aUp=0,b=0,a1=0,a2=0){
  pTR<-(1/(1+exp(-((aUp+b*dataTR.x)))))
  pRT<-(1/(1+exp(-((a1+b*dataRT.x)))))*(1/(1+exp(-((a2-b*dataRT.x)))))
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

prob_fit_diffint<-mle2(prob_ll_diffint,start=list(aUp=aUp,b=bUp,a1=a1,a2=a2),method="L-BFGS-B",
                       lower=c(aUp=-Inf,b=0,a1=-Inf,a2=-Inf),
                       control=list(trace=1,REPORT=5,maxit=1000))

prob_ll_orddiff<-function(aUp=0,b=0,a=0){
  pTR<-(1/(1+exp(-((aUp+b*dataTR.x)))))
  pRT<-(1/(1+exp(-((a+b*dataRT.x)))))*(1/(1+exp(-((a-b*dataRT.x)))))
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

prob_fit_orddiff<-mle2(prob_ll_orddiff,start=list(aUp=aUp,b=bUp,a=a1),method="L-BFGS-B",
                       lower=c(aUp=-Inf,b=0,a1=-Inf),
                       control=list(trace=1,REPORT=5,maxit=1000))

prob_ll_null<-function(aUp=0,bUp=0){
  pTR<-(1/(1+exp(-((aUp+bUp*dataTR.x)))))
  pRT<-(1/(1+exp(-((aUp+bUp*dataRT.x)))))*(1/(1+exp(-((aUp-bUp*dataRT.x)))))
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

prob_fit_null<-mle2(prob_ll_null,start=list(aUp=aUp,bUp=bUp),method="L-BFGS-B",
                    lower=c(aUp=-Inf,bUp=0),
                    control=list(trace=1,REPORT=5,maxit=1000))

#Generate predicted values for graphing
mreal$Envir<-factor(mreal$Envir)

pred<-expand.grid(x=seq(-3,2,.01),Envir=c(levels(mreal$Envir)[1],levels(mreal$Envir)[2]))
pred.rt<-subset(pred,Envir==levels(mreal$Envir)[2])
pred.rt$postma_fit_full<-coef(postma_fit_full)[5]/(2+2*cosh(coef(postma_fit_full)[3]+coef(postma_fit_full)[4]*pred.rt$x))
pred.rt$prob_fit_full<-(1/(1+exp(-((coef(prob_fit_full)[3]+coef(prob_fit_full)[5]*pred.rt$x)))))*
  (1/(1+exp(-((coef(prob_fit_full)[4]-coef(prob_fit_full)[6]*pred.rt$x)))))
pred.rt$prob_fit_downdiff<-(1/(1+exp(-((coef(prob_fit_downdiff)[3]+coef(prob_fit_downdiff)[2]*pred.rt$x)))))*
  (1/(1+exp(-((coef(prob_fit_downdiff)[4]-coef(prob_fit_downdiff)[5]*pred.rt$x)))))

pred.tr<-subset(pred,Envir==levels(mreal$Envir)[1])
pred.tr$postma_fit_full<-1/(1+exp(-(coef(postma_fit_full)[1]+coef(postma_fit_full)[2]*pred.rt$x)))
pred.tr$prob_fit_full<-(1/(1+exp(-((coef(prob_fit_full)[1]+coef(prob_fit_full)[2]*pred.rt$x)))))
pred.tr$prob_fit_downdiff<-(1/(1+exp(-((coef(prob_fit_downdiff)[1]+coef(prob_fit_downdiff)[2]*pred.rt$x)))))

pred<-melt(as.data.frame(rbind(pred.rt,pred.tr)),id=c('x','Envir'))


rtreal$Eras<-as.numeric(as.character(cut(rtreal$YoC,breaks=seq(800,2000,50),labels=seq(825,1975,50))))
trreal$Eras<-as.numeric(as.character(cut(trreal$YoC,breaks=seq(800,2000,50),labels=seq(825,1975,50))))

rtpoints<-group_by(rtreal,Eras,Envir)%>%summarise(prob=sum(isTo)/sum(!is.na(isTo)),num=sum(!is.na(isTo)))
trpoints<-group_by(trreal,Eras,Envir)%>%summarise(prob=sum(isTo)/sum(!is.na(isTo)),num=sum(!is.na(isTo)))

points<-as.data.frame(rbind(rtpoints,trpoints))

points$size<-cut(points$num,breaks=c(-1,49.5,1000),labels=c('Less than 50','50 or more'))

pdf('probgraph.pdf',width=8,height=4)

ggplot(subset(pred,variable%in%c('prob_fit_full')),aes((x*sd(mreal$YoC))+mean(mreal$YoC),value))+
  geom_line(aes(linetype=Envir))+geom_point(data=points,aes(Eras,prob,size=size,shape=Envir))+scale_size_manual(name='Token Number',values=c(2,4))+
  scale_shape_manual(name="Context",values=c('R','T'))+scale_linetype_manual(name="Context",values=c('solid','dashed'))+
  scale_y_continuous(name="Percent to use",breaks=c(0,.2,.4,.5,.6,.8,1),labels=c('0%','20%','40%','50%','60%','80%','100%'))+
  scale_x_continuous(name="Year of Composition",breaks=seq(800,2000,200),labels=seq(800,2000,200))

dev.off()


pdf('modelsgraph.pdf',width=12,height=4)

ggplot(pred,aes((x*sd(mreal$YoC))+mean(mreal$YoC),value))+
  geom_line(data=subset(pred,Envir=='I gave (to) recipient theme'&variable%in%c('postma_fit_full','prob_fit_full')),aes(linetype=variable))+
  geom_point(data=subset(points,Envir=='I gave (to) recipient theme'),aes(Eras,prob,size=size))+scale_size_manual(name='Token Number',values=c(2,4))+
  scale_linetype_discrete(name="Model Type",labels=c("First Derivative","Probability Multiplication"))+
  coord_cartesian(ylim=c(-0.01,1.01))+
  scale_y_continuous(name="Percent to use",breaks=c(0,.2,.4,.5,.6,.8,1),labels=c('0%','20%','40%','50%','60%','80%','100%'))+
  scale_x_continuous(name="Year of Composition",breaks=seq(800,2000,200),labels=seq(800,2000,200))

dev.off()

atab<-anova(prob_fit_null,prob_fit_orddiff,prob_fit_diffint,prob_fit_downdiff,prob_fit_full)
atab<-cbind(atab,AIC(prob_fit_null,prob_fit_orddiff,prob_fit_diffint,prob_fit_downdiff,prob_fit_full)[,1])
colnames(atab)[6]<-'AIC'
xtable(atab)

#Simulate data
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
  results
}

#Starting values for simulation
aUp<-coef(prob_fit_downdiff)[1]
bUp<-coef(prob_fit_downdiff)[2]
a1<-coef(prob_fit_downdiff)[3]
a2<-coef(prob_fit_downdiff)[4]
b2<-coef(prob_fit_downdiff)[5]

#Create simulation data frame
results<-expand.grid(d=seq(-4,4,.1),id=seq(1,100))

results$p.value<-rep(NA,dim(results)[1])

#Run simulations
for (i in 1:dim(results)[1]){
  print(i)
  try(results$p.value[i] <- prob_sim(aUp=aUp,bUp=bUp,a1=a1,a2=a2,b1=(bUp+results$d[i]),b2=b2))
}

#Save simulations
save(results,'simulation_results.RData')

ggplot(results,aes(d,p.value))+stat_summary(fun.y=mean,geom='point')

results$emp.comp<-results$p.value

emp.point<-anova(prob_fit_full,prob_fit_downdiff)[2,5]

results$emp.comp[results$emp.comp<emp.point]<-0
results$emp.comp[results$emp.comp>0]<-1

pdf("pvaluesims.pdf",width=8,height=6)

ggplot(results,aes(abs(d),emp.comp))+stat_summary(fun.y=mean,geom='point')+stat_smooth()+coord_cartesian(ylim=c(0,1))+
  scale_y_continuous(name="% simulation p-values higher than empirical p-value",labels=c("0%","25%","50%","75%","100%"))+
  scale_x_continuous(name="Absolute value difference between theme--recipient and recipient--theme slopes")
dev.off()
