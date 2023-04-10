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

col2numeric <- function(x){
        for(i in 1:ncol(x)){
                x[,i] <- as.numeric(x[,i])
        }
        return(x)
}

#
sd.n <- 20
sd.ratioSTAT=0.675
sd.ratioUNSTAT=1.645
sd.ratioEXSTAT=1.959

path <-"~/src/analyze_R/s.data"
default.f.id <- "FIMTXN"
pre.f.id <- get.conf(name='f.id')
if(is.null(pre.f.id)){pre.f.id <-default.f.id }
f.id<- readline(prompt=paste0("ID for research(default: ",pre.f.id ," )"))
if(f.id==""){f.id <- pre.f.id}

if(f.id ==default.f.id)
{
        main.path <-paste(path, "recorder_1D_FIMTXN_1.TF.log.csv",sep="/")
        main.price <-read.csv(main.path, header=F)[-1,-c(1,3)]

        h.name <-c("date","open","high","low","close","volume")
        names(main.price) <-h.name
        head(main.price)

        main.price[,c(2:6)] <- col2numeric(main.price[,c(2:6)])


        main.xts <-xts(main.price[,-1], order.by=as.Date(main.price$date,"%Y%m%d"))

}else{
        main.xts <- getSymbols(f.id,auto.assign=F)[,-6]
} 
head(main.xts,1)

all.data <-na.omit(merge(main.xts[,c(1:4)]))
names(all.data) <-c("m.open","m.high","m.low","m.close")
head(all.data,2)

all.data$m.log <-log(all.data$m.close)
all.data$m.diff <- all.data$m.close-all.data$m.open
all.data$m.ret <- diff(all.data$m.log)
all.data <- na.omit(all.data)

#連續獲利率
all.data$m.cum <-cumprod(1+all.data$m.ret)
all.data$m.cum.sma <- SMA(all.data$m.cum,sd.n)
all.data$m.cum.sd <- 0
all.data$m.cum.band <- 0
all.data$m.cumstd.line <- 0
all.data$m.cumstd.line.status <- 0
#k線圖附加指標線
all.data$m.sd <- 0
all.data$m.diff.sd <-0
all.data$m.diff.band <- 0
#平進循環進線
all.data$REcumband.line <- 0
#指標線穿越狀況
all.data$std.cross.up <- 0
all.data$std.cross.dn <- 0
all.data$std.cross.2up <- 0
all.data$std.cross.2dn <- 0
all.data$std.cross.exup <- 0
all.data$std.cross.exdn <- 0
#計算k線圖之標差線所需之標差值
len <- nrow(all.data)

for(i in c(sd.n:len))
{
        print(paste("sd_",i,len,sep='/'))

        m.start <- 1+(i-sd.n)
        m.end <- sd.n+(i-sd.n)
        all.data$m.sd[i] <- sd(all.data$m.close[m.start:m.end])
}

all.data <-all.data[-c(1:sd.n),]
all.data$m.ret <- diff(all.data$m.log)
all.data$m.diff.sma <- SMA(all.data$m.diff,sd.n)
all.data$m.close.sma <- SMA(all.data$m.close,sd.n)
#計算標差線
all.data$uSTAT <- all.data$m.close.sma +sd.ratioSTAT*all.data$m.sd
all.data$dSTAT <- all.data$m.close.sma -sd.ratioSTAT*all.data$m.sd
all.data$u2STAT <- all.data$m.close.sma +sd.ratioUNSTAT*all.data$m.sd
all.data$d2STAT <- all.data$m.close.sma -sd.ratioUNSTAT*all.data$m.sd
all.data$exuSTAT <- all.data$m.close.sma +sd.ratioEXSTAT*all.data$m.sd
all.data$exdSTAT <- all.data$m.close.sma -sd.ratioEXSTAT*all.data$m.sd

#head(all.data,1)

main.data <-na.omit(all.data) 
#head(main.data,1)

m.len <- nrow(main.data)
for(i in 3:m.len){

        print(paste("main_",i,m.len,sep='/'))
        #計算1.漲跌幅標差率2.自然漲跌幅標差        
        if(i >=sd.n){
                m.start <- 1+(i-sd.n)
                m.end <- i
                main.data$m.diff.sd[i] <- sd(main.data$m.diff[m.start:m.end])
                main.data$m.cum.sd[i] <- sd(main.data$m.cum[m.start:m.end])

                if(main.data$m.diff.sd[i] ==0){
                        main.data$m.diff.band[i] <- 0
                        }else{
                                main.data$m.diff.band[i] <- (main.data$m.diff[i]-main.data$m.diff.sma[i])/main.data$m.diff.sd[i]
                }

                if(main.data$m.cum.sd[i] ==0){
                        main.data$m.cum.band[i] <- 0
                        }else{
                                main.data$m.cum.band[i] <- (main.data$m.cum[i]-main.data$m.cum.sma[i])/main.data$m.cum.sd[i]
                } 
        }

                #k棒穿越標差線之狀況
                main.data$std.cross.up[i]  <- to.cross(main.data[i,c(1:5)],base.line=main.data$uSTAT[i])  
                main.data$std.cross.dn[i] <- to.cross(main.data[i,c(1:5)],base.line=main.data$dSTAT[i])
                main.data$std.cross.2up[i]  <- to.cross(main.data[i,c(1:5)],base.line=main.data$u2STAT[i])  
                main.data$std.cross.2dn[i] <- to.cross(main.data[i,c(1:5)],base.line=main.data$d2STAT[i])
                main.data$std.cross.exup[i]  <- to.cross(main.data[i,c(1:5)],base.line=main.data$exuSTAT[i])  
                main.data$std.cross.exdn[i] <- to.cross(main.data[i,c(1:5)],base.line=main.data$exdSTAT[i])

                #產生策略判斷結果
                ##預設值
                main.data$m.cumstd.line[i] <- main.data$m.cumstd.line[i-1]
                main.data$REcumband.line[i] <- main.data$REcumband.line[i-1]

                ###CUMStd順向趨勢
                ####標差倍數線經過0
                if(main.data$m.cum.band[i-2] <=0
                   && main.data$m.cum.band[i-1] >0){
                        main.data$m.cumstd.line[i] <- main.data$m.high[i-1]         
                }else if(main.data$m.cum.band[i-2]>=0
                         && main.data$m.cum.band[i-1] <0){
                        main.data$m.cumstd.line[i] <- main.data$m.low[i-1]*-1         
                }

                main.data$m.cumstd.line.status[i] <- pn(main.data$m.cumstd.line[i])

                ###平進循環線
                ###進場線為多頭
                if(main.data$m.cumstd.line[i-1] >0){
                        if(main.data$m.open[i-1] <=main.data$m.cumstd.line[i-1]
                                && main.data$m.close[i-1] >main.data$m.cumstd.line[i-1]){
                                main.data$REcumband.line[i] <- main.data$m.high[i-1]
                        }else if(main.data$std.cross.exup[i-1] >0
                                 || main.data$std.cross.2up[i-1] >0){
                                main.data$REcumband.line[i] <- main.data$m.high[i-1]
                        }
                        
                        main.data$REcumband.line[i] <- ifelse(coredata(main.data$REcumband.line[i]) <coredata(main.data$REcumband.line[i-1]),
                                                        coredata(main.data$REcumband.line[i-1]),
                                                        coredata(main.data$REcumband.line[i]))
                }

                ###標差線為空頭
                if(main.data$m.cumstd.line[i-1] <0){
                        if(main.data$m.open[i-1] >=abs(main.data$m.cumstd.line[i-1])
                                && main.data$m.close[i-1] <abs(main.data$m.cumstd.line[i-1])){
                                main.data$REcumband.line[i] <- main.data$m.low[i-1]
                        }else if(main.data$std.cross.exdn[i-1] <0
                                 || main.data$std.cross.2dn[i-1] <0){
                                main.data$REcumband.line[i] <- main.data$m.low[i-1]
                        }
                        
                        main.data$REcumband.line[i] <- ifelse(coredata(main.data$REcumband.line[i]) >coredata(main.data$REcumband.line[i-1]),
                                                        coredata(main.data$REcumband.line[i-1]),
                                                        coredata(main.data$REcumband.line[i]))

                }
}

        #計算平進循環線之獲利標差率
        main.data$m.recumRET <- cumprod(1 +(main.data$m.ret*main.data$m.cumstd.line.status))-1
        main.data$sma.m.recumRET <- SMA(main.data$m.recumRET,sd.n)
        main.data$stdw.m.recumRET <- 0
        main.data$stdband.m.recumRET <- 0

        for(i in sd.n:m.len){
                print(paste('recumRET',i-(sd.n-1),i,sep='/'))
                #計算獲利率之標準差
                main.data$stdw.m.recumRET[i] <-sd(main.data$m.recumRET[(i-(sd.n-1)):i]) 
                #計算獲利率之標差倍數
                main.data$stdband.m.recumRET[i] <- (main.data$m.recumRET[i]-main.data$sma.m.recumRET[i])/main.data$stdw.m.recumRET[i]
        }
        #
        #轉換成柱圖
        #retscum.hist <- to.hist(main.data$m.recumRET)
        stdband.m.recumRET.hist <- to.hist(main.data$stdband.m.recumRET)
        stdband.cumstd.hist <- to.hist(main.data$m.cum.band)
#
#main.data <- na.omit(main.data)

save.image(file=paste0(m.path,"/RData/",f.id,"_stdevRETBand.RData"))
print("完成作業空間儲存...")
load(paste0(m.path,"/RData/",f.id,"_stdevRETBand.RData"))

#
f.index <- index(main.data)

getans <- readline(prompt="date_db reset(Y/n)?")
if(getans !='n'){
        m_env(mode='init_reset')
        set.conf(name='f.period.start',value=as.character(f.index[1]))
        set.conf(name='f.section',value=140)
        set.conf(name='f.period.jump',value=0)
        print("date_db reset finished.")
        }

repeat{

        my.period <- my.periodGen(x=f.index)

        graphics.off()

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
                            "addTA(abs(main.data$m.cumstd.line),col='gray',on=1,lwd=2)",
                            "addTA(abs(main.data$REcumband.line),col='purple',on=1,lwd=2)",
                            "addTA(merge(stdband.m.recumRET.hist,
                                                sd.ratioSTAT,
                                                sd.ratioSTAT*-1,
                                                sd.ratioUNSTAT,
                                                sd.ratioUNSTAT*-1),
                                        col=c('red',
                                              'green',
                                               'red',
                                               'green',
                                               'darkred',
                                               'darkgreen'),
                                        type=c('h','h','l','l','l','l'))",

                            "addTA(merge(stdband.cumstd.hist,
                                                sd.ratioSTAT,
                                                sd.ratioSTAT*-1,
                                                sd.ratioUNSTAT,
                                                sd.ratioUNSTAT*-1),
                                        col=c('red',
                                               'green',
                                               'red',
                                               'green',
                                               'darkred',
                                               'darkgreen'),
                                        type=c('h','h','l','l','l','l'))"
                                
                                )
        )
}
