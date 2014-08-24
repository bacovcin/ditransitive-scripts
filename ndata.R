setwd("C:/GitHub/realransitive-scripts-win/realransitive-scripts")
dit <- read.csv('adj.tsv',sep='\t')

real<-subset(dit,!is.na(Verb))

real$NGenre<-real$Genre
levels(real$NGenre)<-c("FORMAL","FORMAL","INFORMAL","FORMAL","FORMAL","INFORMAL","FORMAL","FORMAL","FORMAL","FORMAL","FORMAL","FORMAL","INFORMAL","FORMAL","FORMAL","FORMAL","FORMAL","TRANSLATION","POETRY","FORMAL","INFORMAL","FORMAL","FORMAL","TRANSLATION","WEIRD","WEIRD","WEIRD")

real$NDat<-real$Dat

levels(real$NDat)[levels(real$NDat)=='DatDefinite']=c('DatNoun')
levels(real$NDat)[levels(real$NDat)=='DatIndefinite']=c('DatNoun')
levels(real$NDat)[levels(real$NDat)=='DatName']=c('DatNoun')
levels(real$NDat)[levels(real$NDat)=='DatNull']=c('DatNull')
levels(real$NDat)[levels(real$NDat)=='DatDPronoun']=c('DatPronoun')

real$NAcc<-real$Acc

levels(real$NAcc)[levels(real$NAcc)=='AccDefinite']=c('AccNoun')
levels(real$NAcc)[levels(real$NAcc)=='AccIndefinite']=c('AccNoun')
levels(real$NAcc)[levels(real$NAcc)=='AccName']=c('AccNoun')
levels(real$NAcc)[levels(real$NAcc)=='AccNull']=c('AccNull')
levels(real$NAcc)[levels(real$NAcc)=='AccDPronoun']=c('AccPronoun')
levels(real$NAcc)[levels(real$NAcc)=='AccPronoun']=c('AccPronoun')
levels(real$NAcc)[levels(real$NAcc)=='AccEmpty']=c('AccEmpty')
levels(real$NAcc)[levels(real$NAcc)=='AccCP']=c('AccCP')

real$NNom<-real$Nom

levels(real$NNom)[levels(real$NNom)=='NomDefinite']=c('NomNoun')
levels(real$NNom)[levels(real$NNom)=='NomIndefinite']=c('NomNoun')
levels(real$NNom)[levels(real$NNom)=='NomName']=c('NomNoun')
levels(real$NNom)[levels(real$NNom)=='NomNull']=c('NomNull')
levels(real$NNom)[levels(real$NNom)=='NomDPronoun']=c('NomPronoun')
levels(real$NNom)[levels(real$NNom)=='NomEmptyWH']=c('NomWH')

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
levels(real$NVerb)[levels(real$NVerb)=='RETURN']<-c('GIVE')
levels(real$NVerb)[levels(real$NVerb)=='SELL']<-c('GIVE')
levels(real$NVerb)[levels(real$NVerb)=='SEND']<-c('SEND')
levels(real$NVerb)[levels(real$NVerb)=='SERVE']<-c('GIVE')
levels(real$NVerb)[levels(real$NVerb)=='VOUCHSAFE']<-c('PROMISE')
levels(real$NVerb)[levels(real$NVerb)=='YIELD']<-c('GIVE')

preal<-subset(real,NGenre!='POETRY'&NGenre!='WEIRD'&NGenre!='TRANSLATION'&Pas=='PAS'&is.na(DatAcc)&NAcc!='AccCP')
preal$NNom<-factor(preal$NNom)
preal$NDat<-factor(preal$NDat)
preal$NAcc<-factor(preal$NAcc)
preal$Pas<-factor(preal$Pas)


pacc <- subset(preal,NAcc!='AccNull'&NomVerb=='NomV'&AccVerb=='VAcc')
pacc$NOrder<-factor('Recipient--Theme')
pacc$IO<-factor(pacc$NNom)
pacc$DO<-factor(pacc$NAcc)

pdatsbj <- subset(preal,PP!='NoIO'&DatVerb=='DatV'&!(NomVerb=='NomV'&NomDat=='DatNom'))
pdatsbj$NOrder<-factor('Recipient--Theme')
pdatsbj$IO<-factor(pdatsbj$NDat)
pdatsbj$DO<-factor(pdatsbj$NNom)

pdatobj <- subset(preal,PP!='NoIO'&DatVerb=='VDat')
pdatobj$NOrder<-factor('Theme--Recipient')
pdatobj$IO<-factor(pdatobj$NDat)
pdatobj$DO<-factor(pdatobj$NNom)

pnomsbj <- subset(preal,PP!='NoIO'&is.na(DatVerb)&NomVerb=='NomV')
pnomsbj$NOrder<-factor('Theme--Recipient')
pnomsbj$IO<-factor(pnomsbj$NDat)
pnomsbj$DO<-factor(pnomsbj$NNom)

pnomobj <- subset(preal,PP!='NoIO'&is.na(DatVerb)&NomVerb=='NomV')
pnomobj$NOrder<-factor('Recipient--Theme')
pnomobj$IO<-factor(pnomobj$NDat)
pnomobj$DO<-factor(pnomobj$NNom)

pas<-as.data.frame(rbind(pacc,pdatsbj,pdatobj,pnomsbj,pnomobj))

pas$OrdVal<-factor(pas$NOrder)
levels(pas$OrdVal)<-c(1,0)
pas$OrdVal<-as.numeric(as.character(pas$OrdVal))

pas$PPVal<-factor(pas$PP)
levels(pas$PPVal)<-c(0,0,1,1)
pas$PPVal<-as.numeric(as.character(pas$PPVal))

pas$NIO<-factor(pas$IO)
levels(pas$NIO)<-c('IONoun','IONoun','IOPronoun','IONoun','IONoun','IOPronoun','IONull','IOEmpty','IOEmpty','IONoun','IOPronoun')

pas$NDO<-factor(pas$DO)
levels(pas$NDO)<-c('DONoun','DONoun','DONoun','DONoun','DOPronoun','DOEmpty','DONull','DOEmpty','DONoun','DOPronoun')

rpas<-subset(pas,NIO!='IONull'&NDO!='DONull'&NIO!='IOEmpty')
rpas$NAdj<-factor(rpas$Adj)
levels(rpas$NAdj)<-c('Adjacent','NotAdjacent','NotAdjacent','Adjacent','Adjacent')
rpas$NAdj[is.na(rpas$NAdj)]=c('Adjacent')

library(plyr)
rpas$qcen=cut(rpas$YoC,seq(1200,1925,25),seq(1212,1912,25))
rpas$qcen<-as.numeric(as.character(rpas$qcen))

accfull = subset(rpas,NDO!='DOEmpty')
new0 = ddply(accfull,.(qcen,NIO,NDO),summarize,val=sum(OrdVal)/sum(!is.na(OrdVal)),num=sum(!is.na(OrdVal)))
ggplot(accfull,aes(YoC,OrdVal))+stat_smooth()+facet_grid(NIO~NDO)+coord_cartesian(ylim=c(-0.01,1.01))+geom_point(data=new0,aes(qcen,val,size=num))

new1 = ddply(rpas,.(qcen,NIO,NOrder),summarize,val=sum(PPVal)/sum(!is.na(PPVal)),num=sum(!is.na(PPVal)))
ggplot(rpas,aes(YoC,PPVal))+stat_smooth()+facet_grid(NIO~NOrder)+coord_cartesian(ylim=c(-0.01,1.01))+geom_point(data=new1,aes(qcen,val,size=num))

theme = subset(rpas,NOrder=='Theme--Recipient')
new2 = ddply(theme,.(qcen,NIO,NAdj),summarize,val=sum(PPVal)/sum(!is.na(PPVal)),num=sum(!is.na(PPVal)))
ggplot(theme,aes(YoC,PPVal,colour=NIO))+stat_smooth()+facet_grid(NIO~NAdj)+coord_cartesian(ylim=c(-0.01,1.01))+geom_point(data=new2,aes(qcen,val,size=num))

areal<-subset(real,NGenre!='POETRY'&NGenre!='WEIRD'&NGenre!='TRANSLATION'&Pas=='ACT'&NAcc!='AccCP'&NAcc!='AccNull'&NAcc!='AccEmpty'&NAcc!='AccWHEmpty'&NDat!='DatNull'&NDat!='DatEmpty'&NDat!='DatWHEmpty'&PP!='NoIO'&DatVerb=='VDat'&AccVerb=='VAcc')

areal$IO<-factor(areal$NDat)
areal$NIO<-factor(areal$IO)
levels(areal$NIO)<-c('IONoun','IONoun','IOPronoun','IOPronoun','IOPronoun')

areal$DO<-factor(areal$NAcc)
areal$NDO<-factor(areal$DO)
levels(areal$NDO)<-c('DONoun','DONoun','DOPronoun','DONoun','DOPronoun')

areal$PP<-factor(areal$PP)

areal$PPVal<-factor(areal$PP)
levels(areal$PPVal)<-c(0,1,1)
areal$PPVal<-as.numeric(as.character(areal$PPVal))

areal$NOrder<-factor(areal$DatAcc)
levels(areal$NOrder)<-c('Theme--Recipient','Recipient--Theme')

areal$OrdVal<-factor(areal$NOrder)
levels(areal$OrdVal)<-c(0,1)
areal$OrdVal<-as.numeric(as.character(areal$OrdVal))

areal$qcen <- cut(areal$YoC,seq(1200,1925,25),seq(1212,1912,25))
areal$qcen <- as.numeric(as.character(areal$qcen))

areal$NAdj<-factor(areal$Adj)
levels(areal$NAdj)<-c('Adjacent','DOInt','Adjacent','NomInt','NotAdjacent','Adjacent')

areal$NAdj[areal$NAdj=='DOInt'&areal$NDO=='DOPronoun']<-c('Adjacent')
areal$NAdj[areal$NAdj=='DOInt'&areal$NDO!='DOPronoun']<-c('NotAdjacent')

areal$NAdj[areal$NAdj=='NomInt'&areal$NNom=='NomPronoun']<-c('Adjacent')
areal$NAdj[areal$NAdj=='NomInt'&areal$NNom!='NomPronoun']<-c('NotAdjacent')

areal$NAdj<-factor(areal$NAdj)

newdit<-as.data.frame(rbind(rpas,areal))

new3 = ddply(newdit,.(qcen,Pas,NAdj,NOrder,NIO),summarize,val=sum(PPVal)/sum(!is.na(PPVal)),num=sum(!is.na(PPVal)))
new3$num[new3$num>50]<-c(50)
ggplot(newdit,aes(YoC,PPVal,colour=NIO,linetype=Pas))+stat_smooth(method=loess)+facet_grid(NAdj~NOrder)+coord_cartesian(ylim=c(-0.01,1.01))+geom_point(data=new3,aes(qcen,val,size=num,pch=Pas))


fullacc<-subset(newdit,NDO!='DOEmpty')
new4 = ddply(fullacc,.(qcen,Pas,NIO,NDO),summarize,val=sum(OrdVal)/sum(!is.na(OrdVal)),num=sum(!is.na(OrdVal)))
new4$num[new4$num>=100]<-c(100)
ggplot(fullacc,aes(YoC,OrdVal,colour=Pas))+stat_smooth(method=loess)+facet_grid(NIO~NDO)+coord_cartesian(ylim=c(-0.01,1.01))+geom_point(data=new4,aes(qcen,val,size=num))


save(real,file='newadj.RData')
