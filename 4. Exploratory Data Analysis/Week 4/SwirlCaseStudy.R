
library(swirl)
swirl()
dim(pm0)
head(pm0)

cnames<-strplit(cnames,"|",fixed=TRUE)
cnames<-strsplit(cnames,"|",fixed=TRUE)

names(pm0)<-make.names(cnames[[1]][wcol])

x0<-pm0$Sample.Value

mean(is.na(x0))
names(pm1)<-make.names(cnames[[1]][wcol])

x1<-pm1$Sample.Value
mean(is.na(x1))

boxplot(x0,x1)
boxplot(log10(x0),log10(x1))
negative <- x1<0
sum(negative,na.rm=TRUE)
mean(negative,na.rm=TRUE)
dates<-pm1$Date

dates<-as.Date(as.character(dates), "%Y%m%d")

hist(dates[negatives],"month")
hist(dates[negative],"month")

both<-intersect(site0,site1)

cnt0<-subset(pm0, State.Code==36 & county.site %in% both)
cnt1<-subset(pm1, State.Code==36 & county.site %in% both)

sapply(split(cnt0,cnt0$county.site), nrow)
sapply(split(cnt1,cnt1$county.site), nrow)

pm0sub<-subset(cnt0,County.Code==63 & Site.ID==2008)
pm1sub<-subset(cnt1,County.Code==63 & Site.ID==2008)
x0sub<-pm0sub$Sample.Value
x1sub<-pm1sub$Sample.Value

dates0<-as.Date(as.character(pm0sub$Date),"%Y%m%d")
dates1<-as.Date(as.character(pm1sub$Date),"%Y%m%d")

par(mfrow=c(1,2), mar=c(4,4,2,1))

plot(dates0,x0sub,pch=20)
abline(h=median(x0sub,na.rm=TRUE),lwd=2)

plot(dates1,x1sub,pch=20)
abline(h=median(x1sub,na.rm=TRUE),lwd=2)

rng<-range(x0sub,x1sub,na.rm=TRUE)

mn0<-with(pm0,tapply(Sample.Value, State.Code,mean, na.rm=TRUE))
mn1<-with(pm1,tapply(Sample.Value, State.Code,mean, na.rm=TRUE))

d0<-data.frame(names(mn0),mn0)
d0<-data.frame(state=names(mn0),mean=mn0)
d1<-data.frame(state=names(mn1),mean=mn1)
mrg<-merge(d0,d1,by="state")
mrg<-merge(d0,d1,by=state)

with(mrg,plot(rep(1,52),mrg[,2],xlim=c(.5,2.5)))
with(mrg,points(rep(2,52),mrg[,3]))
segments(rep(1,52),mrg[,2],rep(2,52),mrg[,3])
mrg[mrg$mean.x < mrg$mean.y,]

savehistory(file="SwirlCaseStudy.R")
