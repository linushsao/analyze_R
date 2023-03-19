#轉換xq擷取的期貨資料
#
f.pwd <- "~/src/analyze_R"
source(paste(f.pwd,"lib/xq_tools.R",sep="/"))
#
f.folder.in <- paste(f.pwd,"s.data_raw",sep="/")
f.folder.out <- paste(f.pwd,"s.data",sep="/")
f.list <- list.files(path=f.folder.in)

for(f.name in f.list)
{
        f.fullname.in <- paste(f.folder.in,f.name,sep="/")
        f.data <- read.csv(f.fullname.in, header=F)[,-c(1,2,5,11,12)]

        names(f.data) <- c("date","time","open","high","low","close","volume")
        f.fullname.out <- paste0(f.folder.out,"/",f.name,".csv")
        write.csv(f.data,f.fullname.out,row.names=T )
}
