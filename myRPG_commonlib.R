#
#
randomize.df <- function(x){

        result <- x[sample(1:nrow(x)),]
        return(result)
}

vector2char <- function(x){
        
        tmp <- ''
        for(i in 1:length(x)){
                tmp <- paste0(tmp,x[i])
        }

        return(tmp)
}

num2txt.bits <- function(x,bit=2){
                
                len <- length(x)
                result <- c(rep('',len))

                for(i in 1:len){

                        tmp <- as.character(x[i])
                        t.len <- nchar(tmp)
                        if(t.len<bit ){
                                tmp <- paste0(vector2char(rep('0',bit-t.len)),tmp)
                        }
                        result[i] <-tmp 
                }

                return(result)
        }

#num2txt.bits(x=c(1:10))

display.map <- function(x){

        print(x,quote=FALSE)
}

get.map.metadata <- function(level=NULL,md=NULL,info=NULL){

        result <- NULL
        
        #0:世界地圖層級
        if(level ==0){
                if(md=='obj.data'){
                        result <- world.obj.data
                }
                if(md=='map'){
                        result <- world.map
                }
                if(md=='title'){
                        result <- world.map.name
                }
        #1:地圖裡面的子層級(城鎮...)
        }else if(level ==1){
                                obj.data.curr <- towns.map.obj[towns.map.obj$t.id==getBuild$id,]
                                map.curr <-towns.map[,,getBuild$id] 
                                map.curr.name <- getBuild$name

                        if(md=='obj.data'){
                                result <- towns.map.obj[towns.map.obj$t.id==info$id,]
                        }
                        if(md=='map'){
                                result <- towns.map[,,info$id]
                        }
                        if(md=='title'){
                                result <- info$name
                        }

        }

        return(result)
}

push.pull <- function(vec,num,md=NULL,def.num=0){

        len <- length(vec)

        #push into -->
        if(md==1){
                for(i in (len-1):1){
                        vec[i+1] <- vec[i]
                }
                vec[1] <-num 
        }
        #pull out.of <--
        if(md==-1){
                for(i in 2:len){
                        vec[i-1] <- vec[i]
                }
                vec[len] <-def.num 
        }

        return(vec)
}


df.lookup <- function(db=NULL,patten=NULL,index=NULL,output=NULL){

        result <- NULL
        for(i in 1:nrow(db)){
                if(db[i,index]==patten){result <- db[i,output]}
        }

        return(result)
}

df.insertROW <- function(db=NULL,v=NULL){
        
        db<- na.omit(rbind(db,v))
        rownames(db) <- NULL
        return(db)
}

screen.operator <- function(mode='clear'){

        if(mode=='clear'){cat(rep("\n",20))}
}
#用來產生勇者剛進入地圖之起始點座標
player.pos.gen <- function(map.y,map.x,mode='world'){

        pos.x <- NULL
        pos.y <- NULL

        if(mode == 'world'){
                #世界模式>>正中心
                pos.x <- floor(map.x*0.5) 
                pos.y <- floor(map.y*0.5)
        
        }else if(mode == 'village'){
                 #村鎮模式>>正下方
                pos.x <- floor(map.x*0.5) 
                pos.y <- map.y-1
        }
      
        result <- data.frame(y=pos.y,
                             x=pos.x)

        return(result)
}


get.buildINFO <- function(y=NULL,x=NULL){

        result.df <- NULL
        check.symbol <- map.curr[y,x]
        #檢查地圖所屬之物件列表
        for(i in 1:nrow(obj.data.curr)){
                if(obj.data.curr$x[i] ==x
                   && obj.data.curr$y[i] ==y){
                        result.df <- data.frame(
                                        id=i,
                                        symbol=obj.data.curr$symbol[i],
                                        name=paste0(obj.data.curr$name[i]),
                                        sub.group=obj.data.curr$sub.group[i]
                        )
                        #names(result.df) <- c('id','name')
                }        
        }

        if(is.null(result.df)){
                for(i in 1:ncol(towns.material)){
                        if(check.symbol==towns.material$symbol[i])
                                result.df <- data.frame(
                                                id=towns.material$type[i],
                                                symbol=towns.material$symbol[i],
                                                name=towns.material$name[i],
                                                sub.group=towns.material$sub.group[i]

                                )

                }

        }
        
        return(result.df)

}

pos.check <- function(pos.x,pos.y,move.x,move.y,scr.size,map){

        map.x <- ncol(map)
        map.y <- nrow(map)
        check.symbol <- map[pos.y+move.y,pos.x+move.x]

        #邊界檢查
        if(pos.x+move.x >map.x){
                pos.x <- map.x
                move.x <- 0
        }else if(pos.x+move.x <1){
                pos.x <- 1
                move.x <- 0
        }else if(pos.y+move.y >map.y){
                pos.y <- map.y
                move.y <- 0
        }else if(pos.y+move.y <1){
                pos.y <- 1
                move.y <- 0
        #聚落/建築物檢查
        }else if(check.symbol %in% towns.def.symbol 
                || check.symbol %in% towns.def.obj$symbol
                || check.symbol %in% towns.material$symbol){

                move.x <- 0
                move.y <- 0
        }

        return(c(pos.x+move.x,pos.y+move.y))
}

screen.generator <- function(map,pos.x,pos.y,scr.size){

        offset.size <- floor(scr.size*0.5)
        map.x <- ncol(map)
        map.y <- nrow(map)
        scr.playerX <- 0
        scr.playerY <- 0

        conorLU.x <- pos.x -offset.size
        conorLU.y <- pos.y -offset.size
        conorRD.x <- pos.x +offset.size
        conorRD.y <- pos.y +offset.size

        if(conorLU.x <1 || conorLU.y <1){

                conorLU.x <- ifelse(conorLU.x <1,
                                        1,
                                        conorLU.x)
                conorLU.y <- ifelse(conorLU.y <1,
                                        1,
                                        conorLU.y)

                conorRD.x <- conorLU.x +scr.size-1
                conorRD.y <- conorLU.y +scr.size-1
        }


        if(conorRD.x >map.x || conorRD.y >map.y){

                conorRD.x <- ifelse(conorRD.x >map.x,
                                        map.x,
                                        conorRD.x)
                conorRD.y <- ifelse(conorRD.y >map.y,
                                        map.y,
                                        conorRD.y)

                conorLU.x <- conorRD.x -scr.size+1
                conorLU.y <- conorRD.y -scr.size+1
        }
      
        cname <- c(conorLU.x:conorRD.x)
        rname <- c(conorLU.y:conorRD.y)

        scr.map <- map[rname,cname]

        if(pos.x >=offset.size+1 
                && pos.x <=map.x-offset.size){
                scr.playerX <- offset.size+1 #固定在預設位置<中心>
        }else{
                if(pos.x <offset.size+1){scr.playerX  <-pos.x}
                if(pos.x >map.x-offset.size){scr.playerX  <-scr.size-(map.x-pos.x)}
        }
 
        if(pos.y >=offset.size+1 
                && pos.y <=map.y-offset.size){
                scr.playerY <- offset.size+1
        }else{
                if(pos.y <offset.size+1){scr.playerY  <-pos.y}
                if(pos.y >map.y-offset.size){scr.playerY  <-scr.size-(map.y-pos.y)}
        }
       
        scr.map[scr.playerY,scr.playerX] <- player.symbol

        rownames(scr.map) <- num2txt.bits(rname)
        colnames(scr.map) <- num2txt.bits(cname)

        return(scr.map)
}

level.count <- function(x){

        level <- (x/100)^0.5
        return(level)
}

player.dataMGR <- function(info,path,mo){

        if(mo=='w'){
                write.csv(info,path,row.names=F)
                return(TRUE)
        }else if(mo=='r'){
                f.data <- as.data.frame(t(read.csv(path,header=F)))[,-1]
                names(f.data) <- cname
                return(f.data)
        }
}
monster.gen <- function(lv){

        monster.lv.limited <- lv+1
        repeat{
                monster.lv <-sample(1:monster.lv.limited)[1] 
                if(monster.lv <=monster.lv.limited
                        & monster.lv >0)
                {break}
        }

        monster.hp <- cumsum(sample(1:5))[3] *monster.lv
        monster.train <- round(monster.hp *(runif(1)+1), 0)
        monster.sp <- 0
        monster.equ <- 0
        monster.att <- 0
        return(c(monster.lv,monster.hp,monster.train,monster.sp,monster.equ,monster.att))
}

