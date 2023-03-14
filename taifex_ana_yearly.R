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
f.all=c("2022_fut.csv")
#
f.fix="_utf8"
f.n <- paste0(f.all[1],f.fix)
f.import=paste(path.m,"s.data_raw",f.n,sep="/")
f.import

f.raw <- read.csv(f.import,header=F)
f.csv <- f.raw[-1,c(1,2,3,4,5,6,7,10,18)]

head(f.csv,2)
f.csv$V2 <- gsub(" ","",f.csv$V2)
f.csv <- f.csv[f.csv$V2=="MTX",]
f.csv <- f.csv[f.csv$V18=="一般",]
f.csv$V3 <- gsub(" ","",f.csv$V3)
f.colname <- c("date","id","group","open","high","low","close","volume","type")
names(f.csv) <- f.colname
head(f.csv,2)

for(i in 1:nrow(f.csv))
{
        f.month <- month(f.csv$date[i])
        f.year  <- year(f.csv$date[i])

        if(f.month<10)
        {f.month.fix <- paste0("0",f.month)
                }else{
                f.month.fix <- as.character(f.month)
                }

        f.filter <- paste0(f.year,f.month.fix)

        if(f.csv$group[i] !=f.filter)
        {f.csv[i,] <- NA} 

}

f.csv <- na.omit(f.csv)
head(f.csv)

f.data <- f.csv[,c(4,5,6,7,8)]
f.index <- as.Date(f.csv[,1])
head(f.data)
summary(f.data)
head(f.index)
summary(f.index)

for(i in 1:ncol(f.data)){f.data[,i] <- as.numeric(f.data[,i])}

f.xts <- xts(f.data,order.by=f.index)

#繪圖
chartSeries(f.xts,up.col="red",dn.col="green")
#plot(f.xts["2023-03-03 08:45:00::2023-03-03 09:00:00"])


