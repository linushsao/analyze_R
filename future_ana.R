#
#if(外穿越(K,定態線)& pn(穿越點)!=pn(穿越點)[1]){進場線 <- 外極值(K)}
#if(外穿越(K,進場線)){建倉()}
#repeat{
#       if(內回檔(K,躍遷線)){
#               平倉()
#               續場線 <- 穿越點(K,躍遷線)
#               }
#
#        if(外穿越(K,續場線)){建倉()}
#}

rm(list=ls())
#
library(lubridate)
library(quantmod)
#library(PerformanceAnalytics)

m.path="~/src/analyze_R"
lib.path=paste(m.path,"lib",sep="/")
source(paste(lib.path,"misc.R",sep="/"))
source(paste(lib.path,"dataFeed.R",sep="/"))
source(paste(lib.path,"xq_tools.R",sep="/"))

#
default.f.id <- "FIMTXN"
pre.f.id <- get.conf(name='f.id')

if(is.null(pre.f.id)){pre.f.id <-default.f.id }
f.id<- readline(prompt=paste0("ID for research(default: ",pre.f.id ," )"))
if(f.id==""){f.id <- pre.f.id}

if(f.id ==default.f.id)
{
        f.raw <- data.Mgr(x=2)#30分k期貨
        f.index <- datetime2xts(f.raw$date,f.raw$time+1)
        f.xts <- xts(f.raw[,-c(1:2)],order.by=f.index)
}else{
        f.raw <- getSymbols(f.id,auto.assign=F)[,-6]
        f.xts <- f.raw
        names(f.xts) <- c('open','high','low','close','volume')
        #str(f.raw)
        #head(f.raw)
        #summary(f.xts)
        #head(f.xts,2)
} 

set.conf(name='f.id',value=f.id)
#head(f.raw)
#head(f.xts)
#summary(f.xts)
#nrow(f.xts)

#[策略估算]
##預設基本參數
###30分k
my.period=38 
my.section=10
sd.ratio=0.675
sd.ratioExtra=1.645
sd.ratioOutter=2.575

f.period     <- readline(prompt=paste0("period <",my.period,">? "))
f.section    <- readline(prompt=paste0("section<",my.section,">? "))
f.ratio      <- readline(prompt=paste0("ratio<",sd.ratio,">? "))
f.ratioExtra <- readline(prompt=paste0("ratioExtra<",sd.ratioExtra,">? "))
f.ratioOutter <- readline(prompt=paste0("ratioOutter<",sd.ratioOutter,">? "))

if(f.period !=""){my.period <- as.numeric(f.period)}
if(f.section !=""){my.section <- as.numeric(f.section)}
if(f.ratio !=""){sd.ratio <- as.numeric(f.ratio)}
if(f.ratioExtra !=""){sd.ratioExtra <- as.numeric(f.ratioExtra)}
if(f.ratioOutter !=""){sd.ratioOutter <- as.numeric(f.ratioOutter)}

#基礎欄位初始化
f.xts$signal <- 0
f.xts$folio <- 0
f.xts$sd <- 0

f.xts$diff.sd <- 0
f.xts$diff.band <- 0
f.xts$diff.closeopen <- f.xts$close-f.xts$open
f.xts$diff.baseline <- SMA(f.xts$diff.closeopen,n=my.period)

f.xts$base.line <- SMA(f.xts$close,n=my.period)        
f.xts$rate <- ROC(f.xts$close,n=1,type="discrete")
f.xts$time <- paste0(hour(index(f.xts)),minute(index(f.xts)))
f.xts$upper.line <- 0 
f.xts$lower.line <- 0
f.xts$uupper.line <- 0 
f.xts$llower.line <- 0
f.xts$ot.upper.line <- 0 
f.xts$ot.lower.line <- 0


f.xts$if.cross <- 0
f.xts$cr.line <- 0 #進場線
f.xts$crsub.line <- 0 #再次進場線
f.xts$crot.line <- 0 #ot出場線

f.xts$crossing.crline <- 0 

f.xts$upper.cross <- 0 
f.xts$lower.cross <- 0
f.xts$uupper.cross <- 0 
f.xts$llower.cross <- 0
f.xts$ot.upper.cross <- 0 
f.xts$ot.lower.cross <- 0

f.xts$section.high <- 0
f.xts$section.low <- 0
f.xts$openjump <- 0

f.xts <- na.omit(f.xts)
my.row=nrow(f.xts)

head(f.xts)
tail(f.xts)
my.row

#產生基礎指標線資料
print("產生基礎指標線資料")
for(i in my.period:my.row)
{

        print(paste(i,my.row,sep="/"))
        #基礎指標線計算
        f.xts$sd[i] <-sd(f.xts$close[(i-(my.period-1)):i]) 
        f.xts$diff.sd[i] <- sd(f.xts$diff.closeopen[(i-(my.period-1)):i])
        f.xts$diff.band[i] <- (f.xts$diff.closeopen[i]-f.xts$diff.baseline[i])/f.xts$diff.sd[i]

        f.xts$upper.line[i] <- f.xts$base.line[i] + f.xts$sd[i]*sd.ratio
        f.xts$lower.line[i] <- f.xts$base.line[i] - f.xts$sd[i]*sd.ratio
        f.xts$uupper.line[i] <- f.xts$base.line[i] + f.xts$sd[i]*sd.ratioExtra
        f.xts$llower.line[i] <- f.xts$base.line[i] - f.xts$sd[i]*sd.ratioExtra
        f.xts$ot.upper.line[i] <- f.xts$base.line[i] + f.xts$sd[i]*sd.ratioOutter
        f.xts$ot.lower.line[i] <- f.xts$base.line[i] - f.xts$sd[i]*sd.ratioOutter

        #計算區間極值線
        section.high <- c(f.xts$high[(i-my.section):(i-1)])
        section.low  <- c(f.xts$low[(i-my.section):(i-1)])
        f.xts$section.high[i] <- max(section.high)
        f.xts$section.low[i] <- min(section.low)


}

#依策略產生進場點
print("依策略產生進場點")
for(i in my.period:my.row)
{

        print(paste(i,my.row,sep="/"))
        #穿越相關判斷
        ##穿越定態線
        f.xts$upper.cross[i]  <- to.cross(f.xts[i,c(1:5)],base.line=f.xts$upper.line[i])  
        f.xts$lower.cross[i] <- to.cross(f.xts[i,c(1:5)],base.line=f.xts$lower.line[i])
        ##穿越躍遷線
        f.xts$uupper.cross[i]  <- to.cross(f.xts[i,c(1:5)],base.line=f.xts$uupper.line[i])
        f.xts$llower.cross[i] <- to.cross(f.xts[i,c(1:5)],base.line=f.xts$llower.line[i])
        ##穿越ot線
        f.xts$ot.upper.cross[i]  <- to.cross(f.xts[i,c(1:5)],
                                        base.line=f.xts$ot.upper.line[i],
                                        mode=2)
        f.xts$ot.lower.cross[i] <- to.cross(f.xts[i,c(1:5)],
                                        base.line=f.xts$ot.lower.line[i],
                                        mode=2)

        #[進場條件]
        #<多空外穿越內標差線>
        #多：前k上穿上標差線＋k超越前k高點 || 開盤跳多
        #空：前k下穿下標差線＋k低於前k低點 || 開盤跳空
        #之前不應有同向外穿越內標差線
        #出場條件
        #進場k：Stdmin
        #後續k：進場標差線
        #[出場條件]
        #1.內穿越內/外標差線(進場k以後)
        #2.反穿越標心線(進場k)
        #3.反穿越進場線(進場k以後)
        #4.隔天開盤跳開(無論方向)
        #5.同向內標差線與進場線起點相對位置，決定出場線為內標差線/外標差線？

        #<開盤跳開>
        if(f.xts$time[i] ==845)
        {
                #開盤反向外穿越外標差線
                if(f.xts$close[i-1]>f.xts$base.line[i-1]
                        && f.xts$open[i]<f.xts$base.line[i])
                {f.xts$openjump[i] =-1}
                if(f.xts$close[i-1]<f.xts$upper.line[i-1]
                        && f.xts$open[i]>f.xts$base.line[i])
                {f.xts$openjump[i] =1}
        }

        #過前高or落前低
        high.over <- FALSE
        low.under <- FALSE
        if(coredata(f.xts$high[i])>coredata(f.xts$high[i-1])){high.over <- TRUE}
        if(coredata(f.xts$low[i])<coredata(f.xts$low[i-1])){low.under <- TRUE}

        #產生綜合判斷
        #設定預設值
        f.xts$signal[i] <-f.xts$signal[i-1] 
        f.xts$cr.line[i] <-f.xts$cr.line[i-1] 

        ##1.產生多空主(進)場線
        ####k同時穿越同向內外標差線
        if(f.xts$upper.cross[i-1] ==1                           
                && f.xts$uupper.cross[i-1] ==1)
        {
                f.xts$cr.line[i] <- f.xts$uupper.line[i-1] #紀錄進場點，正負表多空策略
        ####K穿越內標差線
        }else if(f.xts$upper.cross[i-1] ==1)
                {
                        f.xts$cr.line[i] <- f.xts$high[i-1] #紀錄進場點，正負表多空策略
        ####跳空開
        }else if(f.xts$openjump[i] ==1)
                {
                        f.xts$cr.line[i] <- max(f.xts$section.high[i-1],
                                                f.xts$upper.line[i-1]) #紀錄進場點，正負表多空策略
        }


        if(f.xts$lower.cross[i-1] ==-1                           
                && f.xts$llower.cross[i-1] ==-1)
        {
                f.xts$cr.line[i] <- f.xts$llower.line[i-1]*-1 
        }else if(f.xts$lower.cross[i-1]==-1)
                {
                        f.xts$cr.line[i] <- f.xts$low[i-1]*-1
        }else if(f.xts$openjump[i] ==-1)
                {
                        f.xts$cr.line[i] <- min(f.xts$section.low[i-1]*-1,
                                                f.xts$lower.line[i-1])
        }

        #過濾重複產生之同向進場線
        if((pn(f.xts$cr.line[i]) ==pn(f.xts$cr.line[i-1]))
                && (coredata(f.xts$cr.line[i]) !=coredata(f.xts$cr.line[i-1]))
                && (coredata(f.xts$openjump[i] ==0)) #無跳空
                && ((f.xts$cr.line[i-1] >0
                                && abs(f.xts$cr.line[i-1]) >f.xts$base.line[i-1])
                        || (f.xts$cr.line[i-1] <0
                                && abs(f.xts$cr.line[i-1]) <f.xts$base.line[i-1])
                        )
                )
        {f.xts$cr.line[i]  <-f.xts$cr.line[i-1]}

        #產生階梯線
        ##預設值
        ###出現新的反向主場線->重設階梯線=主場線
        if(coredata(f.xts$cr.line[i-1]) !=0
                && coredata(f.xts$cr.line[i-1]) !=coredata(f.xts$cr.line[i])
                )
        {
                f.xts$crsub.line[i] <- f.xts$cr.line[i]
                f.xts$crot.line[i]  <- f.xts$cr.line[i]
                ###否則階梯線同前值
                }else{
                        f.xts$crsub.line[i] <- f.xts$crsub.line[i-1]
                        f.xts$crot.line[i] <- f.xts$crot.line[i-1]
                        }
        ##估算階梯線
        ###多方主場線期間
        if(f.xts$cr.line[i] >0)
        {
                #回檔躍遷線，則以該躍遷點為基準產生階梯線
                if(f.xts$uupper.cross[i-1] ==-1
                        || (f.xts$uupper.cross[i-2] ==1
                                && f.xts$close[i-1] >f.xts$uupper.line[i-1])
                        || (f.xts$high[i-2] >f.xts$uupper.line[i-2]
                                && f.xts$close[i-1] <f.xts$uupper.line[i-1]
                                && f.xts$close[i-1] <f.xts$open[i-1])
                        || (f.xts$close[i-1] >f.xts$open[i-1]
                                && f.xts$open[i-1] <f.xts$ot.upper.line[i-1]
                                && f.xts$high[i-1] >f.xts$ot.upper.line[i-1]
                                )
                )
                {
                        f.xts$crsub.line[i] <- f.xts$high[i-1]*pn(f.xts$cr.line[i])

                        }

        }

        if(f.xts$cr.line[i] <0)
        {
                if(f.xts$llower.cross[i-1] ==1
                        || (f.xts$llower.cross[i-2] ==-1
                                && f.xts$close[i-1] >f.xts$llower.line[i-1])
                        || (f.xts$low[i-2] <f.xts$llower.line[i-2]
                                && f.xts$close[i-1] >f.xts$llower.line[i-1]
                                && f.xts$close[i-1] >f.xts$open[i-1])
                        || (f.xts$close[i-1] <f.xts$open[i-1]
                                && f.xts$open[i-1] >f.xts$ot.lower.line[i-1]
                                && f.xts$low[i-1] <f.xts$ot.lower.line[i-1])

                )
                {
                        f.xts$crsub.line[i] <- f.xts$low[i-1]*pn(f.xts$cr.line[i])

                        }

        }

        #檢查產生之階梯線
        if((f.xts$crsub.line[i] >0
                        && f.xts$crsub.line[i-1] >0
                        && coredata(f.xts$crsub.line[i]) <coredata(f.xts$crsub.line[i-1]))
                || (f.xts$crsub.line[i] <0
                        && f.xts$crsub.line[i-1] <0
                        && abs(coredata(f.xts$crsub.line[i])) >abs(coredata(f.xts$crsub.line[i-1])))
                )
        {f.xts$crsub.line[i] <- f.xts$crsub.line[i-1]
                }
        #產生估計報酬率所需之進出場多空訊號
        ##檢查k棒是否穿越主場線
        check.CrCrossing  <- to.cross(f.xts[i,c(1:5)],base.line=abs(f.xts$cr.line[i]))
        f.xts$crossing.crline[i] <- ifelse(check.CrCrossing !=0
                                           , check.CrCrossing
                                           , f.xts$crossing.crline[i-1] )

        ###正負符號同向，代表正確之多空穿越方向
        if((f.xts$crossing.crline[i]*f.xts$cr.line[i]>0))
        {
                if(high.over && f.xts$crossing.crline[i] ==1 ){f.xts$signal[i] <- 1}
                if(low.under && f.xts$crossing.crline[i] ==-1 ){f.xts$signal[i] <- -1}
        }
}

#head(f.xts)
#tail(f.xts)
#summary(f.xts)
save.image(file=paste0(m.path,"/RData/",f.id,"_xq_data.RData"))
print("完成作業空間儲存...")
load(paste0(m.path,"/RData/",f.id,"_xq_data.RData"))

#估算區間獲利率
f.index <- index(f.xts) #取出日期時間

f.period.default.jump =0
f.default.section =4 #一週
f.period.default.start=as.Date(f.index[1], format="%Y%m%d")

repeat
{
        repeat
        {
                repeat
                {
                        f.period.start <- readline(prompt=paste0("period start<",
                                                                 f.period.default.start,
                                                                 " ",weekdays(f.period.default.start),
                                                                 "> or to (M)onday? "))
                        if(f.period.start =="M" || f.period.start =="m")
                        {f.period.default.start <- to.weekdays(f.period.default.start)
                                }else if(f.period.start =="")
                                        {break}
                                else{f.period.default.start <- as.Date(f.period.start) 
                                        }
                }
              
                f.period.default.stop=f.period.default.start +days(f.default.section)
                f.period.stop <- readline(prompt=paste0("period stop<",
                                                        f.period.default.stop,
                                                        " ",weekdays(f.period.default.stop),
                                                        ">? "))
                f.section      <- readline(prompt=paste0("period section<",f.default.section,">? "))
                f.period.jump <- readline(prompt=paste0("section jump to Next<",f.period.default.jump,">? "))

                if(f.period.start ==""){f.period.start =f.period.default.start}
                if(f.section      ==""){f.section      =f.default.section}
                if(f.period.jump ==""){f.period.jump=f.period.default.jump}

                f.period.start <- as.Date(f.period.start)
                if( f.period.stop =="")
                {f.period.stop  <- f.period.start +days(as.numeric(f.section))
                        }else{f.period.stop <- as.Date(f.period.stop)}

                if(as.numeric(f.period.jump) !=0)
                {
                        f.period.start <- f.period.start + days(as.numeric(f.period.jump))
                        f.period.stop  <- f.period.stop  + days(as.numeric(f.period.jump))
                        
                        f.period.default.start <- f.period.start
                        f.period.default.stop <- f.period.stop
                        f.period.default.jump =f.period.jump
                }

                f.period <-period.gen(f.period.start,
                                      f.period.stop,
                ) 

                print(paste(f.period.start, weekdays(f.period.start)))
                print(paste(f.period.stop,weekdays(f.period.stop)))
                print(f.period)
        
                getans <- readline(prompt="ready to virtualize(Y/n)? ") 
                if(getans =='' || getans =='Y'
                        || getans =='y'){break}
                
        }
        #後續計算欄位重設
        folio.count  <- f.xts[f.period]
        #head(folio.count)
        #tail(folio.count)
        #
        folio.count$folio <- folio.count$rate *folio.count$signal
        folio.count$drawdown <-my.ddm(folio.count$folio) 
        folio.count$cumfolio <- cumprod(1+folio.count$folio)

        folio.count$sma.drawdown <- SMA(folio.count$drawdown, my.period)
        folio.count$max.drawdown <- 0
        folio.count$cr.drawdown <- 0
        folio.count$stcr.drawdown <- 0
        folio.count$stdw.drawdown <-0 
        folio.count$stdband.drawdown <- 0

        for(i in 1:nrow(folio.count))
        {
                #最大回轍
                if(i==1)
                {folio.count$max.drawdown[i] <-folio.count$drawdown[i]
                        }else{
                                folio.count$max.drawdown[i] <- min(folio.count$max.drawdown[i-1],
                                                                        folio.count$drawdown[i])
                        } 
                if(i==1)
                {folio.count$cr.drawdown[i] <- folio.count$drawdown[i]
                        }else{
                                #預設值
                                folio.count$cr.drawdown[i] <- folio.count$cr.drawdown[i-1]
                         
                                if(coredata(folio.count$signal[i]) !=coredata(folio.count$signal[i-1]))
                                {folio.count$cr.drawdown[i] <- folio.count$drawdown[i]}
                        }

                if(i>my.period)
                {
                        folio.count$stdw.drawdown[i] <-sd(folio.count$drawdown[(i-(my.period-1)):i]) 
                        folio.count$stdband.drawdown[i] <- (folio.count$drawdown[i]-folio.count$sma.drawdown[i])/folio.count$stdw.drawdown[i]

                }

                if(i>my.period+1)
                {
                        folio.count$stcr.drawdown[i] <- folio.count$stcr.drawdown[i-1]
                        if(coredata(folio.count$signal[i]) !=coredata(folio.count$signal[i-1]))
                        {       folio.count$stcr.drawdown[i] <- folio.count$stdband.drawdown[i]
                                }

                }
        }

        folio.count$sma.stdband.drawdown <- SMA(folio.count$stdband.drawdown,my.section)
        #head(folio.count)
        #tail(folio.count)
        #summary(folio.count)

        folio.count <- na.omit(folio.count)
        #head(folio.count)
        folio.hist.cumfolio <- to.hist(folio.count$cumfolio-1)
        folio.hist.band <- to.hist(SMA(folio.count$diff.band,my.section))

        #繪製圖表
        graphics.off()

        chartSeries(folio.count[,c(1:5)], up.col='red', dn.col='green'
                    ,TA=list("addTA(abs(folio.count$cr.line),on=1,lwd=3,col='orange')"
                             ,"addTA(abs(folio.count$crsub.line),on=1,lwd=2,col='orange')"
                             ,"addTA(folio.count$ot.upper.line,on=1,col='gray')"
                             ,"addTA(folio.count$ot.lower.line,on=1,col='gray')"

                             ,"addTA(folio.count$upper.line,on=1,col='gray')"
                             ,"addTA(folio.count$lower.line,on=1,col='gray')"
                             ,"addTA(folio.count$uupper.line,on=1,col='red',lty='dashed')"
                             ,"addTA(folio.count$llower.line,on=1,col='red',lty='dashed')"
                             ,"addTA(folio.count$base.line,on=1,col='yellow')"
                             ,"addTA(folio.hist.cumfolio,col=c('red','green'),type=c('h','h'))"
                             ,"addTA(merge(folio.hist.band,folio.count$signal*0.5,sd.ratio,sd.ratio*-1),col=c('red','green','orange','darkred','darkgreen'),type=c('h','h','l','l','l'))"
                             ,"addTA(merge(folio.count$drawdown,
                                                folio.count$sma.drawdown,
                                                folio.count$cr.drawdown)
                                        ,col=c('green',
                                               'yellow',
                                               'white')
                                        ,lty=c('solid',
                                               'solid',
                                               'dashed'))"
                                ,"addTA(merge(folio.count$stdband.drawdown,
                                                folio.count$sma.stdband.drawdown,
                                                folio.count$stcr.drawdown,
                                                sd.ratioExtra,
                                                sd.ratio,
                                                0,
                                                sd.ratio*-1,
                                                sd.ratioExtra*-1),
                                                col=c('yellow',
                                                      'orange',
                                                      'white',
                                                      'red',
                                                      'red',
                                                      'white',
                                                      'green',
                                                      'green'),
                                                lty=c('solid',
                                                      'solid',
                                                      'dashed',
                                                      'dashed',
                                                      'solid',
                                                      'solid',
                                                      'solid',
                                                      'dashed'))"
                    )
        )

        #x11()
        #charts.PerformanceSummary(folio.count$folio)

        max.rate <- max(folio.count$cumfolio-1)
        max.rate.point <- max.rate*tail(folio.count$close,1)
        max.ddm <- min(folio.count$drawdown)
        max.ddm.point <- max.ddm*tail(folio.count$close,1)
        
        stand.drawdown <- tail(folio.count$sma.drawdown,1)
        stdw.drawdown <- tail(folio.count$stdw.drawdown,1)

        stand.close <- tail(folio.count$close,1)
        upper.drawdown <- (stand.drawdown +stdw.drawdown*sd.ratio)*stand.close
        mid.drawdown   <- stand.drawdown*stand.close
        lower.drawdown <- (stand.drawdown -stdw.drawdown*sd.ratio)*stand.close

        print(paste0("Max.Portfolio :",round(max.rate,digits=6)*100,"% / ",
                                        round(max.rate.point,digits=0)))
        print(paste0("Max.Drawdown  :",round(max.ddm,digits=6)*100,"% / ",
                                        round(max.ddm.point,digits=0)))
        print(paste0("Info.StdevDDM  :",round(upper.drawdown,digits=0)," <",
                                        round(mid.drawdown,digits=0),"/",
                                        round(stdw.drawdown*stand.close,digits=0),"> ",
                                        round(lower.drawdown,digits=0)))

}

