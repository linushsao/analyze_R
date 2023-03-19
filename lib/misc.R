#
#
to.hist <- function(x)
{
        to.positive <- x[x>0]
        to.negitive <- x[x<0]
        return(merge(to.positive,to.negitive))
}

to.cross <- function(ohcl,base.line)
{
        op=1
        hi=2
        lo=3
        cl=4
        result=0

        if(ohcl[,op]<base.line
                && ohcl[,hi]>base.line
                && coredata(ohcl[,cl])>coredata(ohcl[,op]))
        {result=1 }
        if(ohcl[,op]>base.line
                && ohcl[,lo]<base.line
                && coredata(ohcl[,cl])<coredata(ohcl[,op]))
        {result=-1 }

        return(result)
}

convert.Taifex.daily <- function(f.path)
{

        f.csv <- read.csv(f.path, header=T)[,-c(3,7,8,9)]
        names(f.csv) <- c("date","id","time","price","volume")
        f.csv$id <- gsub(" ","",f.csv$id)

        result <- f.csv[f.csv$id=="MTX",]

        return(result)
}


#library(lubridate)
convert.Taifex <- function(f.path)
{

        f.csv <- read.csv(f.path, header=F)
        f.csv.1 <- f.csv[f.csv$V18=="一般",]
        f.csv.1 <- f.csv.1[f.csv.1$V2=="MTX",]
        f.csv.1 <- f.csv.1[-1,c(1,2,3,4,5,6,7,10,18)]

        f.csv.1$V3 <- gsub(" ","",f.csv.1$V3)

        f.csv.1$V3 <- ifelse(nchar(f.csv.1$V3)>6,NA,f.csv.1$V3)
        f.csv.1 <- na.omit(f.csv.1)

        f.csv.1$month <- as.numeric(month(f.csv.1$V1))
        f.csv.1$c.month <- as.numeric(substr(f.csv.1$V3,5,6))

        f.csv.1$filter <- f.csv.1$c.month-f.csv.1$month 
        f.csv.1$mark <- NA

        f.csv.1$mark <- ifelse(f.csv.1$filter==1,1,f.csv.1$mark)
        f.csv.1$mark <- ifelse(f.csv.1$filter==-11,1,f.csv.1$mark)
        f.csv.1 <- na.omit(f.csv.1)[,c(1,4:8)]

        names(f.csv.1) <- c("date","open","high","low","close","volume")

        return(f.csv.1)
}

tf2num <- function(x)
{

        result=0
        
        if(class(x)=="logical")
        {
                if(x){result=1
                }else{result=-1}
                
        }

        return(result)
}

pn <- function(x)
{
        result=0
        
        if(x>0){result=1}
        if(x<0){result=-1}

        return(result)
}


period.gen <-function(x1,x2,sepr="::")
{
        #產生有開始和結束的日期/時間遴選字串
        result <- paste(x1,x2,sep=sepr)
        return(result)
}


my.ddm <- function(rate)
{

        cumfolio <- cumprod(1+rate)
        cumfolio.max <- cummax(cumfolio)
        #drawdown
        drawdown <- (cumfolio -cumfolio.max)/cumfolio.max

        return(drawdown)
}

to.weekdays <- function(x,to.day='Monday')
{
        repeat
        {
                x <- x+days(1)
                if(weekdays(x) ==to.day)
                {break}
        }
        return(x)
}

