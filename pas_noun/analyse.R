library(rstan)
library(ggplot2)

load('~/Git/ditransitive-scripts/nsep.RData')
print(sep)
sepm<-as.matrix(sep)
summary(sepm[,7])
rbind(sepm[1,],sepm[2,],sepm[3,],sepm[4,],sepm[5,])
b2all <- sepm[,8]
b2sub <- b2all[b2all<=6.6&b2all>=5.7]
ggplot(data=data.frame(y=sepm[,7],x=seq(1,length(sepm[,7]))),aes(x,y,colour='b1'))+geom_point()+geom_point(data=data.frame(y=sepm[,8],x=seq(1,length(sepm[,8]))),aes(x,y,colour='b2'))+geom_line(data=data.frame(x=seq(1,length(sepm[,8])),y=4.4),aes(x,y,colour='HDIline'))+geom_line(data=data.frame(x=seq(1,length(sepm[,8])),y=7.3),aes(x,y,colour='HDIline'))
ggplot(data=data.frame(y=sepm[,10],x=seq(1,length(sepm[,10]))),aes(x,y,colour='b4'))+geom_point()+geom_point(data=data.frame(y=sepm[,9],x=seq(1,length(sepm[,9]))),aes(x,y,colour='b3'))+geom_line(data=data.frame(x=seq(1,length(sepm[,8])),y=2.6),aes(x,y,colour='HDIline'))+geom_line(data=data.frame(x=seq(1,length(sepm[,8])),y=5.0),aes(x,y,colour='HDIline'))
mean(sepm[,1])