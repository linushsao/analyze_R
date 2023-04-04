#
#pkg.url <-"https://cran.r-project.org/src/contrib/Archive/PairTrading/PairTrading_1.0.tar.gz"
#pkg.url <-"https://cran.r-project.org/src/contrib/Archive/tseries/tseries_0.10-52.tar.gz"
#install.packages(pkg.url, repos=NULL, type="source")
#
rm(list=ls())

library(lubridate)
library(quantmod)
library(PerformanceAnalytics)

m.path="~/src/analyze_R"
lib.path=paste(m.path,"lib",sep="/")
source(paste(lib.path,"misc.R",sep="/"))
source(paste(lib.path,"dataFeed.R",sep="/"))
source(paste(lib.path,"xq_tools.R",sep="/"))

#
sd.ratioSTAT=0.675
sd.ratioUNSTAT=1.645
sd.ratioEXSTAT=1.959

path <-"~/src/analyze_R/s.data"
main.path <-paste(path, "recorder_1D_FIMTXN_1.TF.log.csv",sep="/")
sub.path <-paste(path, "recorder_1D_FIZEN_1.TF.log.csv",sep="/")

main.price <-read.csv(main.path, header=F)[-1,-c(1,3)]
sub.price  <-read.csv(sub.path, header=F)[-1,-c(1,3)]

col2numeric <- function(x){
        for(i in 1:ncol(x)){
                x[,i] <- as.numeric(x[,i])
        }
        return(x)
}

h.name <-c("date","open","high","low","close","volume")
names(main.price) <-h.name
names(sub.price) <-h.name
#head(main.price)
#head(sub.price)
#str(main.price)
#str(sub.price)
#class(main.price)

main.price[,c(2:6)] <- col2numeric(main.price[,c(2:6)])
sub.price[,c(2:6)] <- col2numeric(sub.price[,c(2:6)])


main.xts <-xts(main.price[,-1], order.by=as.Date(main.price$date,"%Y%m%d"))
sub.xts <-xts(sub.price[,-1], order.by=as.Date(sub.price$date,"%Y%m%d"))
#head(main.xts,1)
#head(sub.xts,1)
#tail(main.xts,2)
#tail(sub.xts,2)
main.xts$log <-log(main.xts$close)
main.xts$diff <- main.xts$close-main.xts$open
sub.xts$log <-log(sub.xts$close)
head(main.xts,1)
#head(sub.xts)

all.data <-na.omit(merge(main.xts[,c(1:4,6,7)],sub.xts[,c(4,6)]))
names(all.data) <-c("m.open","m.high","m.low","m.close","m.log","m.diff" ,"s.close","s.log")
head(all.data,2)
#plot.zoo(all.data)

all.data$m.ret <- diff(all.data$m.log)
all.data$s.ret <- diff(all.data$s.log)
all.data <- na.omit(all.data)
#cor(all.data[,c(5,6)])

all.data$main.cum <-cumprod(1+all.data$m.ret)
all.data$sub.cum <-cumprod(1+all.data$s.ret)

all.data$diff.rcum <-all.data$sub.cum-all.data$main.cum

len <- nrow(all.data)
#len

sd.n <- 20
all.data$diff.rcum.sma <- SMA(all.data$diff.rcum, sd.n)
all.data$mclose.sma <- SMA(all.data$m.close,sd.n)
all.data$sd <-rep(0,len)
all.data$m.sd <-rep(0,len)
all.data$diff.sd <-rep(0,len)
all.data$diff.band <-rep(0,len)
all.data$band <-rep(0,len)
all.data$cross.stat  <- rep(0,len)
all.data$pt.line <- rep(0,len)
all.data$std.line <- rep(0,len)
all.data$cl.line <- rep(0,len)

all.data$std.cross.up <- rep(0,len)
all.data$std.cross.dn <- rep(0,len)
all.data$std.cross.2up <- rep(0,len)
all.data$std.cross.2dn <- rep(0,len)
all.data$std.cross.exup <- rep(0,len)
all.data$std.cross.exdn <- rep(0,len)

#head(all.data,1)
for(i in c(sd.n:len))
{
        m.start <- 1+(i-sd.n)
        m.end <- sd.n+(i-sd.n)
        all.data$sd[i] <- sd(all.data$diff.rcum[m.start:m.end])
        all.data$m.sd[i] <- sd(all.data$m.close[m.start:m.end])
}

all.data <-all.data[-c(1:sd.n),]
all.data$band <-(all.data$diff.rcum-all.data$diff.rcum.sma)/all.data$sd
all.data$band.sma <- SMA(all.data$band,sd.n)
all.data$m.close.sma <- SMA(all.data$m.close,sd.n)
all.data$m.ret <- diff(all.data$m.log)
all.data$diff.ma <- SMA(all.data$m.diff,sd.n)
#head(all.data,2)
#tail(all.data,2)
#summary(all.data)

all.data$uSTAT <- all.data$m.close.sma +sd.ratioSTAT*all.data$m.sd
all.data$dSTAT <- all.data$m.close.sma -sd.ratioSTAT*all.data$m.sd
all.data$u2STAT <- all.data$m.close.sma +sd.ratioUNSTAT*all.data$m.sd
all.data$d2STAT <- all.data$m.close.sma -sd.ratioUNSTAT*all.data$m.sd
all.data$exuSTAT <- all.data$m.close.sma +sd.ratioEXSTAT*all.data$m.sd
all.data$exdSTAT <- all.data$m.close.sma -sd.ratioEXSTAT*all.data$m.sd

#summary(all.data)
all.data <- na.omit(all.data)
#head(all.data.hist,2)
head(all.data,1)

main.data <-all.data 
head(main.data,1)

m.len <- nrow(main.data)

for(i in 3:m.len){

        print(paste(i,m.len,sep='/'))
                
        if(i >=sd.n){
                m.start <- 1+(i-sd.n)
                m.end <- i
                main.data$diff.sd[i] <- sd(main.data$m.diff[m.start:m.end])
                if(main.data$diff.sd[i] ==0){
                        main.data$diff.band[i] <- 0
                }else{
                        main.data$diff.band[i] <- (main.data$m.diff[i]-main.data$diff.ma[i])/main.data$diff.sd[i]
                }        
        }


                cross.up <-ifelse((main.data$band.sma[i-1]<0
                                   && main.data$band.sma[i]>0),
                                  1,0) 
                cross.dn <-ifelse((main.data$band.sma[i-1]>0
                                   && main.data$band.sma[i]<0),
                                  -1,0) 
                cross.status<- cross.up+cross.dn
                if(cross.status ==0){
                        main.data$cross.stat[i] <- main.data$cross.stat[i-1]
                }else{main.data$cross.stat[i] <- cross.status}

                main.data$std.cross.up[i]  <- to.cross(main.data[i,c(1:5)],base.line=main.data$uSTAT[i])  
                main.data$std.cross.dn[i] <- to.cross(main.data[i,c(1:5)],base.line=main.data$dSTAT[i])
                main.data$std.cross.2up[i]  <- to.cross(main.data[i,c(1:5)],base.line=main.data$u2STAT[i])  
                main.data$std.cross.2dn[i] <- to.cross(main.data[i,c(1:5)],base.line=main.data$d2STAT[i])
                main.data$std.cross.exup[i]  <- to.cross(main.data[i,c(1:5)],base.line=main.data$exuSTAT[i])  
                main.data$std.cross.exdn[i] <- to.cross(main.data[i,c(1:5)],base.line=main.data$exdSTAT[i])

                #產生策略判斷結果
                ##預設值
                main.data$pt.line[i] <- main.data$pt.line[i-1]
                main.data$std.line[i] <- main.data$std.line[i-1]
                main.data$cl.line[i] <- main.data$cl.line[i-1]

                ###PT順向趨勢
                if(main.data$cross.stat[i-2]<=0
                   && main.data$cross.stat[i-1] >0){
                        main.data$pt.line[i] <- ifelse(main.data$m.high[i-1] >main.data$m.close.sma[i-1],
                                                       main.data$m.high[i-1],
                                                       main.data$m.close.sma[i-1])         
                }else if(main.data$cross.stat[i-2]>=0
                         && main.data$cross.stat[i-1] <0){
                        main.data$pt.line[i] <- ifelse(main.data$m.low[i-1] <main.data$m.close.sma[i-1],
                                                       main.data$m.low[i-1]*-1,
                                                       main.data$m.close.sma[i-1]*-1)         
                }

                ###STDev順向趨勢
                if(main.data$std.cross.up[i-2] <=0
                   && main.data$std.cross.up[i-1] >0){
                        main.data$std.line[i] <- main.data$m.high[i-1]         
                }else if(main.data$std.cross.dn[i-2]>=0
                         && main.data$std.cross.dn[i-1] <0){
                        main.data$std.line[i] <- main.data$m.low[i-1]*-1         
                }

                ###平進循環線
                ###標差進場線空轉多
                if(main.data$std.line[i-1] >0){
                        if(main.data$m.open[i-1] <=main.data$std.line[i-1]
                                && main.data$m.close[i-1] >main.data$std.line[i-1]){
                                main.data$cl.line[i] <- main.data$m.high[i-1]
                        }else if(main.data$std.cross.exup[i-1] >0){
                                main.data$cl.line[i] <- main.data$m.high[i-1]
                        }else if(main.data$std.cross.2up[i-1] <0){
                                main.data$cl.line[i] <- main.data$u2STAT[i-1]
                        }
                }

                ###標差進場線多轉空
                if(main.data$std.line[i-1] <0){
                        if(main.data$m.open[i-1] >=main.data$std.line[i-1]
                                && main.data$m.close[i-1] <main.data$std.line[i-1]){
                                main.data$cl.line[i] <- main.data$m.low[i-1]
                        }else if(main.data$std.cross.exdn[i-1] <0){
                                main.data$cl.line[i] <- main.data$m.low[i-1]
                        }else if(main.data$std.cross.2dn[i-1] >0){
                                main.data$cl.line[i] <- main.data$d2STAT[i-1]
                        }
                }
                #複核
                if((main.data$std.line[i]>0 && coredata(main.data$cl.line[i]) <coredata(main.data$cl.line[i-1]))
                        || (main.data$std.line[i]<0 && coredata(main.data$cl.line[i]) >coredata(main.data$cl.line[i-1]))){
                        main.data$cl.line[i] <- main.data$cl.line[i-1]
                }
        }

        #summary(main.data$pt.line)
        #summary(main.data$cross.stat)
        #head(main.data,2)
        #tail(main.data,2)
        main.data$m.retscum <- cumprod(1 +(main.data$m.ret*main.data$cross.stat))-1
        #summary(main.data$m.retscum)

        bandSMA.hist <- to.hist(main.data$band.sma)
        retscum.hist <- to.hist(main.data$m.retscum)
        diff.band.hist <- to.hist(main.data$diff.band)
        
f.index <- index(main.data)

getans <- readline(prompt="date_db reset(N/y)?")
if(getans =='y'){
        m_env(mode='init_reset')
        set.conf(name='f.period.start',value=as.character(f.index[1]))
        set.conf(name='f.section',value=120)
        set.conf(name='f.period.jump',value=0)
        print("date_db reset finished.")
        }

repeat{

        my.period <- my.periodGen(x=f.index)

        chartSeries(main.data[,c(1:5)][my.period],
                    up.col='red',
                    dn.col='green',
                    TA=list("addTA(main.data$m.close.sma,col='yellow',on=1,lwd=2)",
                            "addTA(main.data$uSTAT,col='red',on=1,lty='dashed',lwd=2)",
                            "addTA(main.data$dSTAT,col='green',on=1,lty='dashed',lwd=2)",
                            "addTA(main.data$u2STAT,col='red',on=1)",
                            "addTA(main.data$d2STAT,col='green',on=1)",
                            "addTA(main.data$exuSTAT,col='red',on=1,lty='dashed')",
                            "addTA(main.data$exdSTAT,col='green',on=1,lty='dashed')",
                            "addTA(abs(main.data$pt.line),col='orange',on=1,lwd=2)",
                            "addTA(abs(main.data$std.line),col='white',on=1,lwd=3)",
                            "addTA(abs(main.data$cl.line),col='purple',on=1,lwd=2)",
                            "addTA(bandSMA.hist,col=c('red','green'),type=c('h','h'))",
                            "addTA(merge(diff.band.hist,sd.ratioSTAT,sd.ratioSTAT*-1),col=c('red','green','darkred','darkgreen'),type=c('h','h','l','l'))",
                            "addTA(retscum.hist,col=c('red','green'),type=c('h','h'))"

                    )
        )
}

getans <- readline()

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
