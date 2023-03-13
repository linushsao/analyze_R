#
#pkg.url <-"https://cran.r-project.org/src/contrib/Archive/PairTrading/PairTrading_1.0.tar.gz"
#pkg.url <-"https://cran.r-project.org/src/contrib/Archive/tseries/tseries_0.10-52.tar.gz"
#install.packages(pkg.url, repos=NULL, type="source")
#
rm(list=ls())

library(quantmod)
library(PerformanceAnalytics)

period <- "2022-01-01::2023-12-31"
path <-"~/Downloads"
main.path <-paste0(path, "/FIMTXN.TF.csv")
sub.path <-paste0(path, "/FIZEN.TF.csv")

main.price <-read.csv(main.path, header=F)[,-c(1,2,4,5,11,12)]
sub.price  <-read.csv(sub.path, header=F)[,-c(1,2,4,5,11,12)]
h.name <-c("date","open","high","low","close","volume")
names(main.price) <-h.name
names(sub.price) <-h.name
head(main.price)
head(sub.price)
str(main.price)

main.index <-as.character(main.price$date)
sub.index <-as.character(sub.price$date)

main.xts <-xts(main.price, order.by=as.Date(main.index,"%Y%m%d"))[,-1]
sub.xts <-xts(sub.price, order.by=as.Date(sub.index,"%Y%m%d"))[,-1]
head(main.xts,1)
head(sub.xts,1)
tail(main.xts$close,2)
tail(sub.xts$close,2)
#price.TWII <-getSymbols("^TWII", auto.assign=F)
#candleChart(a["2022-06-01::2023-12-31"], up.col="red", dn.col="green")
#addBBands(n=20, sd=1)
main.log <-log(main.xts$close)
sub.log <-log(sub.xts$close)
head(main.log)
head(sub.log)

all.data <-na.omit(merge(main.xts$close,sub.xts$close,main.log,sub.log))
names(all.data) <-c("m.close","s.close","m.log","s.log")
head(all.data,2)
plot.zoo(all.data)

main.price=50
sub.price=500

main.ret <- diff(all.data$m.log)
sub.ret <- diff(all.data$s.log)
cor(merge(main.ret,sub.ret))

names(main.ret) <- "m.ret"
names(sub.ret) <- "s.ret"
head(main.ret)
head(sub.ret)

all.data <-merge(all.data,main.ret,sub.ret)
head(all.data,2)
all.data <-na.omit(all.data)
head(all.data,2)
all.data$main.cum <-cumprod(1+all.data$m.ret)
all.data$sub.cum <-cumprod(1+all.data$s.ret)

all.data$diff.rcum <-all.data$sub.cum-all.data$main.cum
head(all.data,1)
par(mfrow=c(2,1))
plot(all.data[,c(7,8)],col=c("black","red"))
plot(all.data$diff.rcum,col="blue")

len <- nrow(all.data)
len

sd.n <- 20

all.data$diff.rcum.sma <- SMA(all.data$diff.rcum, sd.n)
all.data$sd <-rep(0,len)
all.data$band <-rep(0,len)

for(i in c(sd.n:len))
{
        m.start <- 1+(i-sd.n)
        m.end <- sd.n+(i-sd.n)
        all.data$sd[i] <- sd(all.data$diff.rcum[m.start:m.end])
}

all.data <-all.data[-c(1:sd.n),]
all.data$band <-(all.data$diff.rcum-all.data$diff.rcum.sma)/all.data$sd

head(all.data,2)
tail(all.data,2)
summary(all.data)


plot.zoo(all.data[,c(7,8,9,12)])
summary(all.data)

#<<產生策略之開平倉訊號>>
all.data$signal <-rep(0,nrow(all.data))
head(all.data,2)

#依標差倍數
level.cr <- 2 
level.cl <- 0.5

tital <-"收斂法"
for(i in c(2:nrow(all.data)))
{
        #上穿上建倉線
        if(all.data$band[i-1]<level.cr && all.data$band[i]>level.cr)
        {all.data$signal[i] =1
        #下穿上平倉線
        }else if(all.data$band[i-1]>level.cl && all.data$band[i]<level.cl)
        {all.data$signal[i] =0
        #下穿下建倉線
        }else if(all.data$band[i-1]>level.cr*-1 && all.data$band[i]<level.cr*-1)
        {all.data$signal[i] =-1
        #上穿下平倉線
        }else if(all.data$band[i-1]<level.cl*-1 && all.data$band[i]>level.cl*-1)
                {all.data$signal[i] =0
        }else
                {all.data$signal[i] <-all.data$signal[i-1]}

}

tital <- "擴張法"
for(i in c(2:nrow(all.data)))
{
        if(all.data$band[i-1]<level.cl && all.data$band[i]>level.cl)
        {all.data$signal[i] =1
        }else if(all.data$band[i-1]>level.cr && all.data$band[i]<level.cr)
                {all.data$signal[i] =0
        }else if(all.data$band[i-1]>level.cl*-1 && all.data$band[i]<level.cl*-1)
                {all.data$signal[i] =-1
        }else if(all.data$band[i-1]<level.cr*-1 && all.data$band[i]>level.cr*-1)
                {all.data$signal[i] =0
        }else
                {all.data$signal[i] <-all.data$signal[i-1]}

}

summary(all.data)

par(mfrow=c(3,1))
plot(all.data[,c(7,8)],col=c("black","red"))
plot(all.data$diff.rcum,col="blue")
plot(merge(all.data$band,all.data$signal,0,level.cr,level.cr*-1,level.cl,level.cl*-1),col=c("black","blue","gray","red","red","red","red"))

head(all.data,2)
tail(all.data,2)
summary(all.data)

adj.ratio.sub <- (sub.price*all.data$s.close)/(main.price*all.data$m.close)

all.data$signal <- lag(all.data$signal)
all.data <- na.omit(all.data)
all.data$total.ret <-(all.data$m.ret*all.data$signal*1)+(all.data$s.ret*adj.ratio.sub*all.data$signal*-1)
all.data$m.cum.ret <-all.data$m.ret*all.data$signal*1
all.data$s.cum.ret <-all.data$s.ret*all.data$signal*-1

par(mfrow=c(1,1))

chartSeries(main.xts[period],up.col="red",dn.col="green")
plot.zoo(all.data[,c(7,8)])
head(main.xts,2)

all.data$total.ret.cum <- cumprod(1+all.data$total.ret)

#產生分析圖
par(mfrow=c(4,1))

plot(all.data[,c(7,8)][period],c("black","red"))
plot(all.data$diff.rcum[period],col="black")

plot(merge(all.data$band,all.data$signal,0,level.cr,level.cr*-1,level.cl,level.cl*-1)[period],col=c("black","blue","gray","red","red","red","red"))
plot(all.data$band[period],col="black")

plot(all.data$total.ret.cum[period],col="red")
plot(all.data$total.ret[period],col="blue")
plot(merge(all.data$band,all.data$signal,0,level.cr,level.cr*-1,level.cl,level.cl*-1)[period],col=c("black","blue","gray","red","red","red","red"))

head(all.data,1)

x11()
par(mfrow=c(1,1))
charts.PerformanceSummary(all.data$total.ret[period],main=tital)
charts.PerformanceSummary(all.data$m.cum.ret[period],main=tital)
charts.PerformanceSummary(all.data$s.cum.ret[period],main=tital)

x11()
chartSeries(main.xts[period],up.col="red",dn.col="green")
addBBands(n=sd.n,sd=1)
