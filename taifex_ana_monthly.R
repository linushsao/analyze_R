#下載並分析期交所資料格式
#
#初始化並載入必要函式庫
rm(list=ls())

library(lubridate)
library(quantmod)
library(PerformanceAnalytics)
path.m="~/src/analyze_R"
path.lib=paste(path.m,"lib",sep="/")

source(paste(path.lib,"misc.R",sep="/"))
source(paste(path.lib,"xq_tools.R",sep="/"))

#
f.all <- c(rep(0,3))
f.all
#f.all[1]="2022_fut.csv"
f.all[1]="2023_01.csv"
f.all[2]="2023_02.csv"
f.all[3]="2023_03.csv"
#
f.fix="_utf8"

for(i in 1:length(f.all))
{
        f.n <- paste0(f.all[i],f.fix)
        f.import=paste(path.m,"s.data_raw",f.n,sep="/")
        f.import

        f.csv <- read.csv(f.import,header=F)[-1,c(1,2,3,4,5,6,7,10,18)]
        f.csv <- f.csv[f.csv$V18=="一般",]
        f.csv$V3 <- gsub(" ","",f.csv$V3)
        f.filter <- paste0("20230",i)
        f.csv <- f.csv[f.csv$V3==f.filter,]
        f.csv <- f.csv[,-c(2,9)]

        f.index <- f.csv[,1]
        f.data <- f.csv[,-c(1,2)]

        head(f.index)
        head(f.data)
        summary(f.data)

        for(j in 1:ncol(f.data))
        {
                f.data[,j] <- as.numeric(f.data[,j])
        }

        f.xts <- xts(f.data,order.by=as.Date(f.index))
        summary(f.xts)

        f.xts <- na.omit(f.xts)
        f.colname <- c("open","high","low","close","volume")
        names(f.xts) <-f.colname

        #chartSeries(f.xts,up.col="red",dn.col="green")
        #a <- readline(prompt="press anykey to continue...")

        if(i==1)
        {
                f.final=f.xts
        }else{
                f.final <- rbind(f.final,f.xts)
        }
}


#繪圖
chartSeries(f.final,up.col="red",dn.col="green")
#plot(f.xts["2023-03-03 08:45:00::2023-03-03 09:00:00"])


