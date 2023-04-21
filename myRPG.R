#
#

#init
rm(list=ls())
#通用函式
print('[SYS]載入通用函式...')


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

        for(i in 1:nrow(towns.main.data.curr)){
                if(towns.main.data.curr$x[i] ==x
                   && towns.main.data.curr$y[i] ==y){
                        result.df <- data.frame(
                                        id=i,
                                        symbol=towns.main.data.curr$symbol[i],
                                        name=paste0(towns.main.data.curr$type[i],towns.main.data.curr$name[i])
                        )
                        #names(result.df) <- c('id','name')
                }        
        }
        
        return(result.df)

}

#pos.check <- function(pos.x,pos.y,move.x,move.y,scr.size,map.x,map.y){
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
        }else if(check.symbol %in% towns.symbol 
                || check.symbol %in% towns.build.store.{
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

        rownames(scr.map) <- rname
        colnames(scr.map) <- cname

        

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

#主程式

#初始設定
print('[SYS]產生世界初始設定...')

map.all.file <- 'map.all.csv'
player.file <- 'player.info.csv'
player.data <- data.frame(
        hp =100,
        mp =100,
        sp =10,
        equ=10,
        att=10,
        lv =0,
        coin =0,
        train =0
)

monster.group <- c('史來姆','郊狼','大型毒蜘蛛','鬃狗','毒蠍子')
cname <- c('hp','mp','sp','equ','att','lv','coin','train')

#產生大地圖50x50
#顯示範圍9x9
map.x <- 51
map.y <- 51
scr.size <- 9

#
#產生大地圖
map.all <- matrix(data=' ',nrow=map.x,ncol=map.y)
map.all.name <- '地海世界'
#產生城鎮資料
##城鎮基本資料
towns.num <- sample(1:5)[1] #城鎮數量
towns.name <- c('幅雷亞','帕爾','威路','坎帕奧','雷諾瓦')
towns.type <- c('城鎮')
towns.symbol <- c('T')
towns.len.x <- 11 #城鎮基本尺寸
towns.len.y <- 11
towns.main.data <- data.frame()

towns.build.store.main.data <- array(' ',c(5,length(towns.build.store.symbol),towns.num))
towns.build.store.map <- array(' ',c(towns.len.x,towns.len.y,towns.num))
towns.build.store.type <- c('店舖')
towns.build.store.symbol <- c('W','S','P')
towns.build.store.name <- c('武器店','防具店','藥水店')
towns.build.material <- data.frame(
                entry='☐',
                wall='∎',
                tree='⁂',
                door.left='◖',
                door.right='◗'
)

##產生城鎮資料
for(i in 1:towns.num){

        #城鎮基本資料
        town.type <- towns.type[1]
        town.name <- towns.name[i]
        town.x=sample(1:map.x)[1]
        town.y=sample(1:map.y)[1]
        town.symbol <- towns.symbol[1]

        towns.main.data <- rbind(towns.main.data,c(town.type,town.name,town.y,town.x,town.symbol))
        map.all[town.y,town.x] <- town.symbol

        #產生城鎮地圖
        ##產生圍牆
        for(k in 1:towns.len.x){
                towns.build.store.map[1,k,i] <-towns.build.material$wall
                towns.build.store.map[towns.len.y,k,i] <-towns.build.material$wall
        }
        
        for(k in 1:towns.len.y){
                towns.build.store.map[k,1,i] <-towns.build.material$wall
                towns.build.store.map[k,towns.len.x,i] <-towns.build.material$wall
        }

        ##產生村鎮大門
        getpos <- player.pos.gen(towns.len.y,towns.len.x,mode='village')
        towns.build.store.map[towns.len.y,getpos$x,i] <-towns.build.material$entry
        towns.build.store.map[towns.len.y,getpos$x-1,i] <-towns.build.material$door.left
        towns.build.store.map[towns.len.y,getpos$x+1,i] <-towns.build.material$door.right

        ##產生城鎮相關設施和店舖
        for(j in 1:length(towns.build.store.symbol)){
        
                #城鎮基本資料
                store.type <- towns.build.store.type[1]
                store.name <- towns.build.store.name[j]
                store.x <- sample(2:towns.len.x-1)[1]
                store.y <- sample(2:towns.len.y-1)[1]
                store.symbol <- towns.build.store.symbol[j]

                towns.build.store.main.data <- rbind(towns.build.store.main.data,c(store.type,store.name,store.y,store.x,store.symbol))

                #store.x <- sample(2:towns.len.x-1)[1]
                #store.y <- sample(2:towns.len.y-1)[1]
                towns.build.store.map[sample.y,sample.x,i] <-store.symbol         
        } 


}
names(towns.main.data) <- c('type','name','y','x','symbol')

player.dataMGR(info=towns.main.data,path='towns.main.data.csv',mo='w')

#勇者初始位置 
print('[SYS]產生勇者設定...')

map.curr.x <- map.x
map.curr.y <- map.y
getpos <- player.pos.gen(map.curr.y,map.curr.x,mode='world')
#pos.x <- floor(map.x*0.5) 
#pos.y <- floor(map.y*0.5)
pos.x <- getpos$x 
pos.y <- getpos$y

player.symbol='@'

#
if (!file.exists(player.file)){
        print("第一次進入遊戲，初始化冒險世界及勇者資料...")
        Sys.sleep(1)
        player.dataMGR(info=player.data,path=player.file,mo='w')

}else{
        player.data <- player.dataMGR(path=player.file,mo='r')
        
}

hp =as.numeric(player.data$hp)
mp =as.numeric(player.data$mp)
sp =as.numeric(player.data$sp)
equ =as.numeric(player.data$equ)
att =as.numeric(player.data$att)
lv =as.numeric(player.data$lv)
coin =as.numeric(player.data$coin)
train =as.numeric(player.data$train)

towns.main.data.curr <- towns.main.data
map.curr <- map.all
map.curr.name <- map.all.name
map <- screen.generator(map.curr,pos.x,pos.y,scr.size)

#主程式
repeat{


        screen.operator()
        print(map)

        getans <- readline(prompt=paste0("[",map.curr.name," ",pos.y," ",pos.x, "] ","lv",lv," hp",hp," mp",mp," coin",coin," exp",train," (j/l/i/m/o) "))


        if(getans =='i'){
                #pos.y <- pos.y -1
                move.x <- 0
                move.y <- -1
                get.posCheck <- pos.check(pos.x,pos.y,move.x,move.y,scr.size,map.curr)
                pos.y <- get.posCheck[2]
                map <- screen.generator(map.curr,pos.x,pos.y,scr.size)
               
                #screen.operator(mode='clear')
                #print(map)
                print("你正往北走...")}
        if(getans =='m'){
                #pos.y <- pos.y +1
                move.x <- 0
                move.y <- 1
                get.posCheck <- pos.check(pos.x,pos.y,move.x,move.y,scr.size,map.curr)
                pos.y <- get.posCheck[2]
                map <- screen.generator(map.curr,pos.x,pos.y,scr.size)

                #screen.operator(mode='clear')
                #print(map)
                print("你正往南走...")}
        if(getans =='l'){
                #pos.x <- pos.x +1
                move.x <- 1
                move.y <- 0
                get.posCheck <- pos.check(pos.x,pos.y,move.x,move.y,scr.size,map.curr)
                pos.x <- get.posCheck[1]
                map <- screen.generator(map.curr,pos.x,pos.y,scr.size)

                #screen.operator(mode='clear')
                #print(map)
                print("你正往東走...")}
        if(getans =='j'){
                #pos.x <- pos.x -1
                move.x <- -1
                move.y <- 0
                get.posCheck <- pos.check(pos.x,pos.y,move.x,move.y,scr.size,map.curr)
                pos.x <- get.posCheck[1]
                map <- screen.generator(map.curr,pos.x,pos.y,scr.size)

                #screen.operator(mode='clear')
                #print(map)
                print("你正往西走...")}
        
        if(getans =='look'){
                
                getans <- readline(prompt=paste0('觀察方向(l/j/m/i) '))

                look.x <- pos.x
                look.y <- pos.y
                if(getans =='l'){look.x <- look.x+1}
                if(getans =='j'){look.x <- look.x-1}
                if(getans =='m'){look.y <- look.y+1}
                if(getans =='i'){look.y <- look.y-1}
                
                getBuild <- get.buildINFO(look.y,look.x)
                if(is.null(getBuild)){
                        getans <- readline(prompt='看來沒啥特別的...')
                }else if(getBuild){
                        getans <- readline(paste0('你看到了',gettowns.name,' (e)nter?'))
                        if(getans =='e'){

                                getans <- readline(prompt=paste0('準備進入 ',gettowns.name,' ...'))

                                towns.main.data.curr <- 
                                map.curr <-towns.build.store.map[,,getBuild$id] 
                                map.curr.name <- gettowns.name

                                map.curr.x <- ncol(map.curr) 
                                map.curr.y <- nrow(map.curr)
                                getpos <- player.pos.gen(map.curr.y,map.curr.x,mode='village')
                                
                                pos.x <- getpos$x
                                pos.y <- getpos$y
                                map <- screen.generator(map.curr,pos.x,pos.y,scr.size)
                        }
                }

        }


        if(getans =='cl'){
                print("下次見，勇者ˇ...")
                player.data<- c(hp,mp,sp,equ,att,lv,coin,train)
                player.dataMGR(player.data,player.file,mo='w')
                break
        }

        if(getans =='o'){print("儲存遊戲資料完成...")
                player.data<- c(hp,mp,sp,equ,att,lv,coin,train)
                player.dataMGR(player.data,player.file,mo='w')

        }

        chance <- runif(1)
        #print(paste0("debug:",chance))
        #if(chance >0.5){
        if(FALSE){
       
                get.monster <- monster.gen(lv)
                monster.lv <- get.monster[1]
                monster.hp <- get.monster[2]
                monster.train <- get.monster[3]
                monster.name <- monster.group[sample(1:5)[1]]
                times <- 0
                
                print(paste0("你碰到等級",monster.lv,"的",monster.name,"，進入戰鬥..."))
                Sys.sleep(1)

                repeat{
                        print("")
                        getans <- readline(prompt=paste0("lv",lv," hp",hp," mp",mp," coin",coin, " (a)ttack,(m)ove,(r)unaway <a>"))
                        if(getans == 'a' || getans ==''){
                                att.p <- round(runif(1),2)
                                monster.hp <- monster.hp-att.p
                                times  <- times +1

                                print(paste0("你攻擊怪物",monster.name,"，傷害",att.p,"點/",monster.hp," ..."))
                                Sys.sleep(1)
                                if(monster.hp <=0){
                                        
                                        print("怪物被擊斃...")
                                        coin.add <- round(runif(1)*10*times,0)
                                        coin <- coin +coin.add
                                        train <- train +monster.train
                                        Sys.sleep(1)
                                        print(paste0("你獲得 ",coin.add,"金幣/ ", monster.train,"經驗"))

                                        check.level <- floor(level.count(train))+1
                                        if(check.level >lv)
                                        {
                                                lv <- lv+1
                                                print(paste0("勇敢的騎士啊，恭喜你升級至 ",lv,"級"))
                                        }

                                        break
                                }
                                att.m <- round(runif(1),2)
                                hp <- hp-att.m
                                
                                print(paste0("你被怪物",monster.name,"攻擊，傷害",att.m,"點/",hp," ..."))
                                Sys.sleep(1)
                                if(hp <=0){
                                        
                                        print("你被怪物吃掉...")
                                        break
                                }


                        }
                

                }        

        }

}
