library(mgcv)

load('dit.RData')

pall<-subset(dit,(Pas=='REC'|Pas=='THEME')&(DO=='DONoun'|DO=='DOPronoun'))
pall$DO<-factor(pall$DO)
pall$IO<-factor(pall$IO)
pasdata<-data.frame(Year=pall$YoC,Value=pall$PasValue,Type=c('Pas'),IO=pall$IO,DO=pall$DO)

given<-subset(dit,(NVerb=='PROMISE'|NVerb=='GIVE'|NVerb=='OLDENG')&Pas=='ACT')

given.da<-subset(given,NOrder=='DA')
given.ad<-subset(given,NOrder=='AD')
given.top<-subset(given,NOrder=='TOP')

da <- data.frame(Year=given.da$YoC,Value=given.da$PPValue,Type=c('DA'),IO=given.da$IO,DO=given.da$DO)
ad <- data.frame(Year=given.ad$YoC,Value=given.ad$PPValue,Type=c('AD'),IO=given.ad$IO,DO=given.ad$DO)
top <- data.frame(Year=given.top$YoC,Value=given.top$PPValue,Type=c('TOP'),IO=given.top$IO,DO=given.top$DO)
joint.data <- as.data.frame(rbind(da,ad,pasdata,top))
joint.data$Cond<-joint.data$Type

levels(joint.data$Cond)<-c('3','1','4','2')

old.data1 <- subset(joint.data,Cond==1&IO=='IONoun')
data1 <- data.frame(x=old.data1$Year,y=old.data1$Value,Cond=1)
old.data2 <- subset(joint.data,Cond==2&IO=='IONoun')
data2 <- data.frame(x=old.data2$Year,y=old.data2$Value,Cond=2)
old.data3 <- subset(joint.data,Cond==3&IO=='IONoun')
data3 <- data.frame(x=old.data3$Year,y=old.data3$Value,Cond=3)
old.data4 <- subset(joint.data,Cond==4)
data4 <- data.frame(x=old.data4$Year,y=old.data4$Value,Cond=4)

real.data<-rbind(data1,data2,data3,data4)
real.data$Cond<-as.numeric(as.character(real.data$Cond))

gmodel = gam(data=data3,formula=y~s(x,k=10))
new.data = data.frame(x=data3$x)
new.data$y <- predict(gmodel,newdata=new.data)

data3.b <- subset(data3,x>new.data$x[new.data$y==max(new.data$y)])
gmodel2 = gam(data=data.frame(x=data3.b$x,y=data3.b$y),formula=y~s(x,k=10))
new.datag = data.frame(x=data3.b$x)
new.datag$y <- predict(gmodel,newdata=new.datag)

smallest <- 1
for(i in 1:floor(length(data3.b$x)/100)) {
  min <- (min(data3.b$x)-100)+(100*i)
  max <- min + 100
  data <- subset(new.data,x>min&x<=max)
  if(mean(data$y)<smallest) smallest <- mean(data$y) else break
}
max <- (min(data3.b$x)-100)+(100*i)-50

data3.b <- subset(data3.b,x>max)
heavym <- 1 - mean(subset(new.data,x>min(data3.b$x))$y)
heavys <-abs(max(subset(new.data,x>min(data3.b$x))$y)-min(subset(new.data,x>min(data3.b$x))$y))/2

g1 <- glm(data=data1,y~x,family=binomial)
g2 <- glm(data=data2,y~x,family=binomial)
g5 <- glm(data=data4,y~x,family=binomial)

aMu <- mean(c(summary(g1)$coefficients[,1][1],summary(g2)$coefficients[,1][1]))
bMu <-mean(c(summary(g1)$coefficients[,1][2],summary(g2)$coefficients[,1][2]))
aSig <- max(c(summary(g2)$coefficients[,2][1],summary(g1)$coefficients[,2][1]))*3
bSig <- max(c(summary(g2)$coefficients[,2][2],summary(g1)$coefficients[,2][2]))*2


stan_dat1 <- list(N = length(real.data$x),y=real.data$y,x=real.data$x,cond=real.data$Cond,
                  aMu=c(summary(g1)$coefficients[,1][1],summary(g2)$coefficients[,1][1],aMu,summary(g5)$coefficients[,1][1],summary(g5)$coefficients[,1][1]),
                  bMu=c(summary(g1)$coefficients[,1][2],summary(g2)$coefficients[,1][2],bMu,summary(g5)$coefficients[,1][2],summary(g5)$coefficients[,1][2]),
                  heavyMu=heavym,
                  aSigma=c(summary(g1)$coefficients[,2][1],summary(g2)$coefficients[,2][1],aSig,summary(g5)$coefficients[,2][1]*3,summary(g5)$coefficients[,2][1]),
                  bSigma=c(summary(g1)$coefficients[,2][2],summary(g2)$coefficients[,2][2],bSig,summary(g5)$coefficients[,2][2]*2,summary(g5)$coefficients[,2][2]),
                  heavySigma=heavys)

prior <- stan_dat1
save(prior,file='prior.RData')
