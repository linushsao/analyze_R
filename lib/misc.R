#
#
to.hist <- function(x)
{
        to.positive <- x[x>0]
        to.negitive <- x[x<0]
        return(merge(to.positive,to.negitive))
}

to.cross <- function(ohcl,base.line,mode=1)
{
        op=1
        hi=2
        lo=3
        cl=4
        result=0

        if(mode==1)
        {
                if(ohcl[,op]<base.line
                   && ohcl[,hi]>base.line
                   && coredata(ohcl[,cl])>coredata(ohcl[,op]))
                {result=1 }
                if(ohcl[,op]>base.line
                   && ohcl[,lo]<base.line
                   && coredata(ohcl[,cl])<coredata(ohcl[,op]))
                {result=-1 }

        }else if(mode==2)
        {
                if(ohcl[,op]<base.line
                   && ohcl[,hi]>base.line)
                {result=1 }
                if(ohcl[,op]>base.line
                   && ohcl[,lo]<base.line)
                {result=-1 }

        }

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



my.periodGen <- function(x,section=7,jump){

        #估算區間獲利率
        #f.index <- index(f.xts) #取出日期時間
        f.index <- x

        #f.period.default.jump =jump
        #f.default.section =section 
        #f.period.default.start=as.Date(f.index[1], format="%Y%m%d")
        #set.conf(name='f.section',value=section)
        #set.conf(name='f.period.jump',value=jump)

        repeat
        {

                        f.period.default.start=as.Date(get.conf(name='f.period.start'))
                        f.period.default.jump =as.numeric(get.conf(name='f.period.jump'))
                        f.default.section =as.numeric(get.conf(name='f.section'))

                        repeat
                        {
                                f.period.start <- readline(prompt=paste0("period start<",
                                                                         f.period.default.start,
                                                                         " ",weekdays(f.period.default.start),
                                                                         "> or to (M)onday or to (F)irstDay? "))
                                if(f.period.start =="M" || f.period.start =="m")
                                {f.period.default.start <- to.weekdays(f.period.default.start)
                                }else if(f.period.start =="F" || f.period.start =="f")
                                {f.period.default.start <-f.index[1] 
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

                                #f.period.default.start <- f.period.start
                                #f.period.default.stop <- f.period.stop
                                #f.period.default.jump =f.period.jump
                        }

                        #檢查日期是否超出資料範圍
                        if(f.period.start <f.index[1]){
                                f.period.start <-f.index[1]
                        }


                        if(f.period.stop >f.index[length(f.index)]){
                                f.period.stop <- f.index[length(f.index)]
                        }
                        
                        #
                        f.period.default.start <- f.period.start
                        f.period.default.stop <- f.period.stop
                        f.period.default.jump <- f.period.jump

                        f.period <-period.gen(f.period.start,
                                              f.period.stop,
                        ) 

                        set.conf(name='f.period.start',value=as.character(f.period.stop))
                        set.conf(name='f.section',value=f.section)
                        set.conf(name='f.period.jump',value=f.period.jump)

                        print(paste(f.period.start, weekdays(f.period.start)))
                        print(paste(f.period.stop,weekdays(f.period.stop)))

                        print(f.period)

                        getans <- readline(prompt="ready to virtualize(Y/n)? ") 
                        if(getans =='' || getans =='Y'
                           || getans =='y'){
                                break}

                }

        return(f.period)

}
#' A enchaned env Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

m_env <- function(name=NULL,value=NULL,mode="init_list",dataset="env") {
    
    #dataset: 
    #1. = default,means handle environment variables
    #2. dataset could be defined to another custom dataset name        
    #         
    FLAG_add <- TRUE
    name.name <- "name"
    name.value <- "value"
    
    # function <<
    
    env.add.colname <- function(v){
        config <- read.csv(conf_name,header=FALSE)[-c(1),-c(1)] 
        new.col <- rep("_", length(config[,1]))
        names(new.col) <- name
        config <- cbind(config, new.col)
        result <- config
        write.csv(result,file=name)
        
        return(result)
    }
    
    env.db.merge <- function(be.merged,name){
        #backup original conf
        file.copy(name,paste(name,format(Sys.time(), "%b-%d_%X"),sep="."))
        
        be.merged.file <- read.csv(be.merged,header=FALSE)[-c(1),-c(1)] 
        config <- read.csv(conf_name,header=FALSE)[-c(1),-c(1)] 
        config <- rbind(config,be.merged.file)
        result <- config
        names(result) <- c(name.name ,name.value)
        
        write.csv(result,file=name)
        
        return(result)
    }
    
    env.db.replace <- function(be.replaced,name){
        #backup original conf
        file.copy(name,paste(name,format(Sys.time(), "%b-%d_%X"),sep="."))
        
        config <- read.csv(be.replaced,header=FALSE)[-c(1),-c(1)] 
        result <- config
        names(result) <- c(name.name ,name.value)
        
        write.csv(result,file=name)
        
        return(result)
    }
    
    env.reset <- function(v,name){
        #backup original conf
        file.copy(name,paste(name,format(Sys.time(), "%b-%d_%X"),sep="."))
        
        unlink(name)
        file.create(name)
        
        result <- data.frame(a=c(v[1]),b=c(v[2]))
        names(result) <- c(name.name ,name.value)
        write.csv(result,file=name)
        
        return(result)
    }
    
    env.read <- function(x){
        config <- read.csv(conf_name,header=FALSE)[-c(1),-c(1)] 
        r.name <- as.vector(t(config[,1]))
        r.value <- as.vector(t(config[,2]))
        for( y1 in 1:length(r.name)) {
            if( x == r.name[y1] ) { 
                result <- r.value[y1]
                return(result)
            }
        }
    }
    
    env.modify <- function(){
        config <- read.csv(conf_name,header=FALSE)[-c(1),-c(1)] 
        filter <- as.vector(t(config[,1]))
        for( y1 in 1:length(filter)) {
            if( name == filter[y1] ) { 
                temp <- as.vector(t(config[,2]))
                temp[y1] <- value
                config[,2] <- temp
                write.csv(config,file=conf_name)
                FLAG_add <- FALSE
            }
        }
        if( FLAG_add ) { # add record
            temp <- data.frame(a=c(name),b=c(value))
            names(config) <- c(name.name ,name.value)
            names(temp) <- c(name.name ,name.value)
            config <- rbind(config,temp)
            write.csv(config,file=conf_name)
        }
        return(config)
    }
    
    env.del <- function(config) {
        config <- read.csv(conf_name,header=FALSE)[-c(1),-c(1)] 
        filter <- as.vector(t(config[,1]))
        for( y1 in 1:length(filter)) {
            if( name == filter[y1]  ) { 
                config[y1,] <- NA
                config <- na.omit(config)
                write.csv(config,file=conf_name)
            }
        }
    }
    
    env.dataset.path.exist <- function(name.env.file_path)
    {
        if (! file.exists(name.env.file_path) ) { # dataset existed
            env.reset(c(env.file_name,env.file_value),name=name.env.file_path) 
            
        }
        
        result <- name.env.file_path
        return(result)
    }
    
    # main function
    env.file_name <- paste(dataset ,"_file" ,sep="")
    env.file_value <- m_paste(c(".",dataset,".Configure"),op="")
    extension <- ".csv"
    default.env.file_path <- m_paste(c(getwd(), "/",env.file_value,extension),op="")
    
    conf_name <- env.dataset.path.exist(default.env.file_path)
    
    if(mode == "init_reset") {
        config <- env.reset(c(env.file_name,env.file_value),name=conf_name ) #add first record
        result <- config
    }else if(mode == "init_list") {
        #do nothing
    }else if(mode == "add.colname") {
        config <- env.add.colname(name)
        result <- config
    }else if(mode == "db_list") {
        result  <- dataset
        return(result)
        break
    }else if(mode == "db_merge") {
        be.merged <- value
        print(c("1",be.merged ,conf_name))
        config <- env.db.merge(be.merged, name=conf_name)
        result <- config
    }else if(mode == "db_replace") {
        be.replaced <- value
        config <- env.db.replace(be.replaced, name=conf_name)
        result <- config
    }else if(mode == "w") {
        config <- env.modify()
    }else if(mode == "r") {
        return(env.read(name))
        break
    }else if(mode == "d") {
        config <- env.del(config)
    }
    
    config <- read.csv(conf_name,header=FALSE)[-c(1),-c(1)]
    rownames(config) <- NULL
    names(config) <- c("name","value")
    result <- config
    return(result)
}

#for testing
# a <- "TESTING"
# 
# stop()
# # env(name="raw.data.listname",mode="r")
# # 
# # name="ORDER"
# # value="FRUIT"
# # mode="w"
# # v <- c(env.file_name,env.file_value)
# rm(list=ls())
# # v[2]
# m_env()
# m_env(mode="db_list")
# 
# m_env(value="/home/linus/ProjectStock/all_stocks/.env.Configure.csv",mode="db_merge")
# m_env(value="/home/linus/ProjectStock/all_stocks/.env.Configure.csv",mode="db_replace")
# 
# m_env(dataset="env")
# m_env(mode="db_list")
# m_env(dataset="new")
# m_env(mode="db_list",dataset="new")
# 
# m_env(mode="init_reset")
# m_env(mode="init_reset",dataset="new")
# 
# (m_env(name="list",value="NO",mode="w"))
# (m_env(name="list",value="YES",mode="w"))
# (m_env(name="ORDER",value="FRUIT",mode="w"))
# (m_env(name="ORDER",value="BANANA",mode="w"))
# 
# (m_env(name="ORDER",value="BANANA",mode="d"))
# (m_env(name="list",mode="d"))
# 
# m_env(dataset="new")
# (m_env(name="list",value="NO",mode="w",dataset="new"))
# (m_env(name="list",mode="r",dataset="new"))
# (m_env(name="list",value="YES",mode="w",dataset="new"))
# (m_env(name="ORDER",value="FRUIT",mode="w",dataset="new"))
# (m_env(name="ORDER",value="BANANA",mode="w",dataset="new"))
# 
# (m_env(name="ORDER",value="BANANA",mode="d",dataset="new"))
# (m_env(name="list",mode="d",dataset="new"))
# 
# (m_env(name="index.yahoo",mode="r",dataset="dataset.MGR"))
# # 
# 
# head(m_env(name="prefix.raw.data.name",mode="d"))
# head(m_env(mode="init_list"))
# 
# head(m_env(mode="init_reset"))

#other function

get.conf <- function(name,dataset="env", default.conf=NULL, splite.char=FALSE, splite.num=FALSE){
    #     browser()
    result <- m_env(name=name,mode="r",dataset=dataset)
    if( is.null(result) ) { result <- default.conf  }
    
    return(result)
}

set.conf <- function(name, value, dataset="env"){
    
    result <- m_env(name=name, value=value, mode="w",dataset=dataset)
    return(TRUE)
}

rm.conf <- function(name, dataset="env"){
    
    result <- m_env(name=name, mode="d",dataset=dataset)
    return(TRUE)
}





#' A multi paste Function
#'
#' This function allows you to multi paste vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

 m_paste <- function(x,op=""){
    temp <- ""
     for(i in 1:length(x)){
        temp <- ifelse(i == 1,x[i],paste(temp,x[i],sep=op))
        }
    return(temp)
 }
