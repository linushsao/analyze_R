#
#source("~/src/lib/xq_tools.R")
#
data.Mgr <- function(x)
{
        m.path="~/src/analyze_R"

        #if(x==1)
        #{
        #        f.path <- "~/src/FIMTXN.TF.csv"
        #        f.data <- read.csv(f.path, header=F, sep=",")
        #        f.index <- as.Date(as.character(f.data[,3]), format="%Y%m%d")

        #        f.xts <- xts(f.data[,c(6:10)], order.by=f.index)
        #        names(f.xts) <- c("open","high","low","close","volume")
        #}

        if(x==2)
        {

                f.path=paste(m.path,"s.data",sep="/")
                f.name="recorder_30M_FIMTXN_1.TF.log.csv"
                f.import <- paste(f.path,f.name,sep="/")
                
                f.xts <- read.csv(f.import, header=T)[,-1]
        }
        
        if(x==3)
        {

                f.path=paste(m.path,"s.data/2330.TW.csv",sep="/")
                f.xts <- read.csv(f.path, header=T)[,-1]
        }


return(f.xts)

}
