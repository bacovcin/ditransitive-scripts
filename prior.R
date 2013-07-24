library(mgcv)

standardize = function(dataMat){
  m = mean(dataMat)
  sd = sd(dataMat)
  zDataMat = (dataMat-m)/sd
  return(zDataMat)
}

load('dit.RData')

pall<-subset(dit,(Pas=='REC'|Pas=='THEME')&(DO=='DONoun'|DO=='DOPronoun'))
thirt<-subset(pall,YoC<=1350&Pas!='REC')
pall<-as.data.frame(rbind(subset(pall,YoC>1350),thirt))
rm(thirt)
pall$DO<-factor(pall$DO)
pall$IO<-factor(pall$IO)
pasdata<-data.frame(Year=pall$YoC,Value=pall$PasValue,Type=c('Pas'),IO=pall$IO,DO=pall$DO)

given<-subset(dit,(NVerb=='PROMISE'|NVerb=='GIVE'|NVerb=='OLDENG')&Pas=='ACT')

given.da<-subset(given,NOrder=='DA')
given.ad<-subset(given,NOrder=='AD')
given.dtop<-subset(given,NOrder=='TOP'&DatOrder=='DN')
levels(given.dtop$NOrder)<-c('AD','DA','MONO','DAT-TOP')
given.atop<-subset(given,NOrder=='TOP'&DatOrder=='ND')
levels(given.atop$NOrder)<-c('AD','DA','MONO','ACC-TOP')


da <- data.frame(Year=given.da$YoC,Value=given.da$PPValue,Type=c('DA'),IO=given.da$IO,DO=given.da$DO)
ad <- data.frame(Year=given.ad$YoC,Value=given.ad$PPValue,Type=c('AD'),IO=given.ad$IO,DO=given.ad$DO)
atop <- data.frame(Year=given.atop$YoC,Value=given.atop$PPValue,Type=c('ACC-TOP'),IO=given.atop$IO,DO=given.atop$DO)
dtop <- data.frame(Year=given.dtop$YoC,Value=given.dtop$PPValue,Type=c('DAT-TOP'),IO=given.dtop$IO,DO=given.dtop$DO)
joint.data <- as.data.frame(rbind(da,ad,pasdata,atop,dtop))
joint.data$Cond<-joint.data$Type

levels(joint.data$Cond)<-c('2','1','3','5','4')
joint.data$x <- standardize(joint.data$Year)

old.data1 <- subset(joint.data,Cond==1&IO=='IONoun')
data1 <- data.frame(x=old.data1$x,y=old.data1$Value,Cond=1)
old.data2 <- subset(joint.data,Cond==2&IO=='IONoun')
data2 <- data.frame(x=old.data2$x,y=old.data2$Value,Cond=2)
old.data3 <- subset(joint.data,Cond==3)
data3 <- data.frame(x=old.data3$x,y=old.data3$Value,Cond=3)
old.data4 <- subset(joint.data,Cond==4&IO=='IONoun')
data4 <- data.frame(x=old.data4$x,y=old.data4$Value,Cond=4)
old.data2 <- subset(joint.data,Cond==5&IO=='IONoun')
data5 <- data.frame(x=old.data2$x,y=old.data2$Value,Cond=5)

real.data<-rbind(data1,data2,data3,data4,data5)
real.data$Cond<-as.numeric(as.character(real.data$Cond))


gmodel = gam(data=data2,formula=y~s(x,k=10))
new.data = data.frame(x=data2$x)
new.data$y <- predict(gmodel,newdata=new.data)

data2.b <- subset(data2,x>new.data$x[new.data$y==max(new.data$y)])
gmodel2 = gam(data=data.frame(x=data2.b$x,y=data2.b$y),formula=y~s(x,k=10))
new.datag = data.frame(x=data2.b$x)
new.datag$y <- predict(gmodel,newdata=new.datag)

smallest <- 1
for(i in 1:floor(length(data2.b$x)/100)) {
  min <- (min(data2.b$x)-((max(data2.b$x)-min(data2.b$x))/100))+(((max(data2.b$x)-min(data2.b$x))/100)*i)
  max <- min + ((max(data2.b$x)-min(data2.b$x))/100)
  data <- subset(new.data,x>min&x<=max)
  if(mean(data$y)<smallest) smallest <- mean(data$y) else break
}
max <- (min(data2.b$x)-((max(data2.b$x)-min(data2.b$x))/100))+(((max(data2.b$x)-min(data2.b$x))/100)*i)-((max(data2.b$x)-min(data2.b$x))/200)

data2.b <- subset(data2.b,x>max)
heavym <- 1 - mean(subset(new.data,x>min(data2.b$x))$y)
heavys <-abs(max(subset(new.data,x>min(data2.b$x))$y)-min(subset(new.data,x>min(data2.b$x))$y))/2

g1 <- glm(data=data1,y~x,family=binomial)
g4 <- glm(data=data3,y~x,family=binomial)
g5 <- glm(data=data4,y~x,family=binomial)
g6 <- glm(data=data5,y~x,family=binomial)


aMu <- mean(c(summary(g1)$coefficients[,1][1],summary(g5)$coefficients[,1][1],summary(g6)$coefficients[,1][1]))
bMu <-mean(c(summary(g1)$coefficients[,1][2],summary(g5)$coefficients[,1][2],summary(g6)$coefficients[,1][2]))
aSig <- mean(c(summary(g1)$coefficients[,2][1],summary(g5)$coefficients[,2][1],summary(g6)$coefficients[,2][1]))*3
bSig <- mean(c(summary(g1)$coefficients[,2][2],summary(g5)$coefficients[,2][2],summary(g6)$coefficients[,2][2]))*2


stan_dat1 <- list(N = length(real.data$x),y=real.data$y,x=real.data$x,cond=real.data$Cond,
                  aMu=c(summary(g1)$coefficients[,1][1],aMu,summary(g4)$coefficients[,1][1],summary(g4)$coefficients[,1][1],summary(g5)$coefficients[,1][1],summary(g6)$coefficients[,1][1]),
                  bMu=c(summary(g1)$coefficients[,1][2],bMu,summary(g4)$coefficients[,1][2],summary(g4)$coefficients[,1][2],summary(g5)$coefficients[,1][2],summary(g6)$coefficients[,1][2]),
                  heavyMu=heavym,
                  aSigma=c(summary(g1)$coefficients[,2][1],aSig,summary(g4)$coefficients[,2][1]*3,summary(g4)$coefficients[,2][1],summary(g5)$coefficients[,2][1],summary(g6)$coefficients[,2][1]),
                  bSigma=c(summary(g1)$coefficients[,2][2],bSig,summary(g4)$coefficients[,2][2]*2,summary(g4)$coefficients[,2][2],summary(g5)$coefficients[,2][1],summary(g6)$coefficients[,2][1]),
                  heavySigma=heavys)

prior <- stan_dat1
save(prior,file='prior.RData')
