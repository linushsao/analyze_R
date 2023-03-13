#轉換日期＋分鐘資料為xts
#

#

library(quantmod)

datetime2xts <- function(x.date,x.time)
{

        rep2char <- function(x)
        {
                tmp <- ""

                if(length(x) !=0)
                {
                        for(i in 1:length(x))
                        {
                                tmp <- paste0(tmp,x[i])
                        }
                }
                return(tmp)
        }

        f.time <- x.time
        f.rep <- 6-nchar(f.time)
        f.rep

        for(i in 1:length(f.time))
        {
                if(f.rep[i] !=0)
                {
                        f.time[i] <- paste0(rep2char(rep(0,f.rep[i])), 
                                                f.time[i])
                }
        }


        file.ct <- paste(x.date,f.time,sep=" ")
        file.ct <- as.POSIXct(file.ct,format="%Y%m%d %H%M%S")

        return(file.ct)
}


#轉換xq匯出之價位資料成r分析所需欄位
#
xqlog2csv <- function(f.data, t.shift=0)
{

        if(t.shift !=0){f.data$time <- f.data$time+1}

        index.xts <-datetime2xts(f.data$date,f.data$time)
        #f.data.xts <- xts(f.data[,-c(1,2)],order.by=index.xts)
        result <- merge(index.xts,f.data[,-c(1,2)])
        return(result)
}

