library(mgcv)

standardize = function(dataMat){
  m = mean(dataMat)
  sd = sd(dataMat)
  zDataMat = (dataMat-m)/sd
  return(zDataMat)
}
load('~/Git/ditransitive-scripts/dit.RData')

dit1 <- subset(dit,DO=='DOPronoun'&IO=='IONoun')
dit2 <- subset(dit,DO=='DONoun'&IO=='IOPronoun')
dit3 <- subset(dit,DO=='DONoun'&IO=='IONoun')

dit <- as.data.frame(rbind(dit1,dit2,dit3))
rm(dit1)
rm(dit2)
rm(dit3)

pall<-subset(dit,(Pas=='REC'|Pas=='THEME')&(DO=='DONoun'|DO=='DOPronoun')&NVerb!='TELL')
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

joint.data$IO <- factor(joint.data$IO)
joint.data$DO <- factor(joint.data$DO)

ggplot(data=subset(joint.data,Cond!='3'&Cond!='4'&Cond!='5'),aes(Year,Value,colour=IO))+stat_smooth(method=loess)+facet_wrap(~Cond)

joint.data$io <- factor(joint.data$IO)
levels(joint.data$io) <- c(1,2)

joint.data$Cond<-as.numeric(as.character(joint.data$Cond))
joint.data$io<-as.numeric(as.character(joint.data$io))
real.data<-data.frame(x=standardize(joint.data$Year),y=joint.data$Value,cond=joint.data$Cond,io=joint.data$io)

data2 = subset(real.data,cond==2)
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

data1 <- subset(real.data,cond==1&io==2)
gmodel1 = loess(data=data.frame(x=data1$x,y=data1$y),y~x)
new.datag = data.frame(x=data1$x)
new.datag$y <- predict(gmodel1,newdata=new.datag)

prom <- max(new.datag$y)
pros <- 0.2

g11 <- glm(data=subset(real.data,cond==1,io=1),y~x,family=binomial)
g12 <- glm(data=subset(real.data,cond==1,io=2),y~x,family=binomial)

g41 <- glm(data=subset(real.data,cond==3,io=1),y~x,family=binomial)
g42 <- glm(data=subset(real.data,cond==3,io=2),y~x,family=binomial)

g51 <- glm(data=subset(real.data,cond==4,iodo=1),y~x,family=binomial)
g52 <- glm(data=subset(real.data,cond==4,iodo=2),y~x,family=binomial)

g61 <- glm(data=subset(real.data,cond==5,iodo=1),y~x,family=binomial)
g62 <- glm(data=subset(real.data,cond==5,iodo=2),y~x,family=binomial)

aMu1 <- mean(c(summary(g11)$coefficients[,1][1],summary(g51)$coefficients[,1][1],summary(g61)$coefficients[,1][1]))
bMu1 <-mean(c(summary(g11)$coefficients[,1][2],summary(g51)$coefficients[,1][2],summary(g61)$coefficients[,1][2]))
aMu2 <- mean(c(summary(g11)$coefficients[,1][1],summary(g51)$coefficients[,1][1],summary(g61)$coefficients[,1][1]))
bMu2 <-mean(c(summary(g11)$coefficients[,1][2],summary(g51)$coefficients[,1][2],summary(g61)$coefficients[,1][2]))
aSig1 <- mean(c(summary(g12)$coefficients[,2][1],summary(g52)$coefficients[,2][1],summary(g62)$coefficients[,2][1]))*3
bSig1 <- mean(c(summary(g12)$coefficients[,2][2],summary(g52)$coefficients[,2][2],summary(g62)$coefficients[,2][2]))*2
aSig2 <- mean(c(summary(g12)$coefficients[,2][1],summary(g52)$coefficients[,2][1],summary(g62)$coefficients[,2][1]))*3
bSig2 <- mean(c(summary(g12)$coefficients[,2][2],summary(g52)$coefficients[,2][2],summary(g62)$coefficients[,2][2]))*2


stan_dat1 <- list(N = length(real.data$x),y=real.data$y,x=real.data$x,cond=real.data$cond,io=real.data$io,
                  aMu1=c(summary(g11)$coefficients[,1][1],aMu1,summary(g41)$coefficients[,1][1],summary(g41)$coefficients[,1][1],summary(g51)$coefficients[,1][1],summary(g61)$coefficients[,1][1]),
                  bMu1=c(summary(g11)$coefficients[,1][2],bMu1,summary(g41)$coefficients[,1][2],summary(g41)$coefficients[,1][2],summary(g51)$coefficients[,1][2],summary(g61)$coefficients[,1][2]),
                  aMu2=c(summary(g12)$coefficients[,1][1],aMu2,summary(g42)$coefficients[,1][1],summary(g42)$coefficients[,1][1],summary(g52)$coefficients[,1][1],summary(g62)$coefficients[,1][1]),
                  bMu2=c(summary(g12)$coefficients[,1][2],bMu2,summary(g42)$coefficients[,1][2],summary(g42)$coefficients[,1][2],summary(g52)$coefficients[,1][2],summary(g62)$coefficients[,1][2]),
		  proMu=prom,
                  heavyMu=heavym,
                  aSigma1=c(summary(g11)$coefficients[,2][1],aSig1,summary(g41)$coefficients[,2][1]*3,summary(g41)$coefficients[,2][1],summary(g51)$coefficients[,2][1],summary(g61)$coefficients[,2][1]),
                  bSigma1=c(summary(g11)$coefficients[,2][2],bSig1,summary(g41)$coefficients[,2][2]*2,summary(g41)$coefficients[,2][2],summary(g51)$coefficients[,2][1],summary(g61)$coefficients[,2][1]),
                  aSigma2=c(summary(g12)$coefficients[,2][1],aSig2,summary(g42)$coefficients[,2][1]*3,summary(g42)$coefficients[,2][1],summary(g52)$coefficients[,2][1],summary(g62)$coefficients[,2][1]),
                  bSigma2=c(summary(g12)$coefficients[,2][2],bSig2,summary(g42)$coefficients[,2][2]*2,summary(g42)$coefficients[,2][2],summary(g52)$coefficients[,2][1],summary(g62)$coefficients[,2][1]),
		  proSigma=pros,
                  heavySigma=heavys)

prior <- stan_dat1
save(prior,file='nprior.RData')
