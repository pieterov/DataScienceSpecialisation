
setwd('..')

setwd('~/Documents/Github Repos/DataScienceSpecialisation/4. Exploratory Data Analysis/week 4')

pm0<- read.table("RD_501_88101_1999-0.txt", comment.char="#", header = FALSE, sep="|", na.string="")
cnames<-readLines("RD_501_88101_1999-0.txt",1)
cnames <- strsplit(cnames,"|", fixed = TRUE)
names(pm0)<-cnames[[1]]
names(pm0)<-make.names(cnames[[1]])

x0<-pm0$Sample.Value
mean(is.na(x0))
pm1<- read.table("RD_501_88101_2012-0.txt", comment.char="#", header = FALSE, sep="|", na.string="")

names(pm0)<-make.names(cnames[[1]])
names(pm1)<-make.names(cnames[[1]])

x1<-pm0$Sample.Value
x1<-pm1$Sample.Value

mean(is.na(x1))
boxplot(x0,x1)
boxplot(log10(x0),log10(x1))

negative<- x1<0

sum(negative, na.rm=TRUE)
mean(negative, na.rm=TRUE)
dates<-pm1$Date

dates <- as.Date(as.character(dates), "%Y%m%d")

hist(dates,"month")
hist(dates[negative],"month")

site0<-unique(subset(pm0, State.Code ==36, c(County.Code, Site.ID)))
site1<-unique(subset(pm1, State.Code ==36, c(County.Code, Site.ID)))

site0<-paste(site0[,1],site0[,2],sep=".")
site1<-paste(site1[,1],site1[,2],sep=".")

both<-intersect(site0,site1)
both

pm0$county.site<-with(pm0,paste(County.Code, Site.ID, sep="."))
pm1$county.site<-with(pm1,paste(County.Code, Site.ID, sep="."))

pm0<-subset(pm0, select = -country.site)
pm1<-subset(pm1, select = -country.site)

cnt0<-subset(pm0, State.Code==36 & county.site %in% both)
cnt1<-subset(pm1, State.Code==36 & county.site %in% both)

split(cnt0, cnt$county.site)
split(cnt0, cnt0$county.site)

sapply(split(cnt0, cnt0$county.site),nrow)
sapply(split(cnt1, cnt1$county.site),nrow)

pm1sub<-subset(pm1, State.Code==36 & County.Code == 63 & Site.ID == 2008)
pm0sub<-subset(pm0, State.Code==36 & County.Code == 63 & Site.ID == 2008)

dates1 <- pm1sub$Date
x1sub<-pm1sub$Sample.Value
plot(dates1,x1sub)

dates1 <- as.Date(as.character(dates1), "%Y%m%d")
plot(dates1,x1sub)

dates0 <- pm0sub$Date
x0sub<-pm0sub$Sample.Value
dates0 <- as.Date(as.character(dates0), "%Y%m%d")
plot(dates0,x0sub)

par(mfrow=c(1,2), mar=c(4,4,2,1))
plot(dates0,x0sub, pch=20)
abline(h=median(x0sub,na.rm=TRUE))

plot(dates1,x1sub, pch=20)
abline(h=median(x1sub,na.rm=TRUE))

range(x0sub,x1sub, na.rm=TRUE)
rng<-range(x0sub,x1sub, na.rm=TRUE)

par(mfrow=c(1,2), mar=c(4,4,2,1))
plot(dates0,x0sub, pch=20, ylim=rng)
abline(h=median(x0sub,na.rm=TRUE))

plot(dates1,x1sub, pch=20, ylim=rng)
abline(h=median(x1sub,na.rm=TRUE))

mn0<-with(pm0,tapply(Sample.Value, State.Code, mean, na.rm=T))
mn1<-with(pm1,tapply(Sample.Value, State.Code, mean, na.rm=T))

d0<-data.frame(state=names(mn0), mean=mn0)
d1<-data.frame(state=names(mn1), mean=mn1)

unique(pm0$State.Code)

mrg<-merge(d0, d1, by="state")

par(mfrow=c(1,1), mar=c(4,4,2,1))
with(mrg,plot(rep(1999,52), mrg[,2], xlim=c(1998,2013)))
with(mrg,points(rep(2012,52), mrg[,3]))

segments(rep(1999,52), mrg[,2],rep(2012,52),mrg[,3])

savehistory(file = "Case Study.Rhistory")
