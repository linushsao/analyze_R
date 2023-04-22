#
#

#init
rm(list=ls())
#通用函式
print('[SYS]載入通用函式...')

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

        for(i in 1:nrow(obj.data.curr)){
                if(obj.data.curr$x[i] ==x
                   && obj.data.curr$y[i] ==y){
                        result.df <- data.frame(
                                        id=i,
                                        symbol=obj.data.curr$symbol[i],
                                        name=paste0(obj.data.curr$type[i],obj.data.curr$name[i])
                        )
                        #names(result.df) <- c('id','name')
                }        
        }

        if(is.null(result.df)){
                for(i in 1:ncol(towns.material)){
                        if(check.symbol==towns.material$symbol[i])
                                result.df <- data.frame(
                                                id=towns.material$id[i],
                                                symbol=towns.material$symbol[i],
                                                name=towns.material$name[i]
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
        #}else if(check.symbol %in% towns.def.symbol 
        #        || check.symbol %in% towns.def.store.symbol){
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

#===================主程式=================
#初始設定
print('[SYS]產生世界初始設定...')

world.map.file <- 'world.map.csv'
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
##顯示範圍9x9
map.x <- 51
map.y <- 51
scr.size <- 9

world.map <- matrix(data=' ',nrow=map.x,ncol=map.y)
world.map.name <- '地海世界'
world.obj.data <- data.frame()#紀錄大地圖上之各種物件(城鎮，植物，地形...)座標及代表符號
world.obj.type <- data.frame(
                        town='T',
                        cave='C'
                )
towns.num.limited <- 5 #大地圖城鎮上限數目
obj.rowname <- c('type','name','y','x','symbol')#通用物件資料欄位

##產生城鎮資料
###定義城鎮基本資料
towns.def.num <- sample(1:towns.num.limited)[1] #城鎮數量
towns.def.name <- c('幅雷亞','帕爾','威路','坎帕奧','雷諾瓦')
towns.def.type <- c('城鎮')
towns.def.symbol <- world.obj.type$town
towns.def.len.x <- 11 #城鎮基本尺寸
towns.def.len.y <- 11

#towns.def.store.type <- c('店舖')
#towns.def.store.symbol <- c('W','S','P')
#towns.def.store.name <- c('武器店','防具店','藥水店')

towns.def.obj <- data.frame(symbol=NA,name=NA,type=NA)
towns.def.obj <- df.insertROW(towns.def.obj,c('W','武器店','店舖'))
towns.def.obj <- df.insertROW(towns.def.obj,c('S','防具店','店舖'))
towns.def.obj <- df.insertROW(towns.def.obj,c('P','藥水店','店舖'))
#towns.def.obj <- rbind(towns.def.obj,c('W','武器店','店舖'))
#towns.def.obj <- rbind(towns.def.obj,c('S','防具店','店舖'))
#towns.def.obj <- rbind(towns.def.obj,c('P','藥水店','店舖'))

#產生空白城鎮地圖陣列
#1.標記物件符號如城鎮(T)及座標位置
towns.map <- array(' ',c(towns.def.len.x,towns.def.len.y,towns.def.num))
#2.標記單位城鎮地圖內座標之物件(牆，店舖..)相關屬性資料
#towns.obj.data <- array(' ',c(length(towns.def.store.symbol),length(obj.rowname),towns.def.num))
towns.obj.data <- data.frame()
#3.建材種類資料
#towns.material <- data.frame(
#                entry='☐',
#                wall='∎',
#                tree='⁂',
#                door.left='◖',
#                door.right='◗'
#)
towns.material <- data.frame(id=NA,symbol=NA,name=NA)
towns.material <- df.insertROW(towns.material,c('entry','☐','主入口'))
towns.material <- df.insertROW(towns.material,c('wall','∎','石牆'))
towns.material <- df.insertROW(towns.material,c('tree','⁂','白楊樹'))
towns.material <- df.insertROW(towns.material,c('door.left','◖','左大門'))
towns.material <- df.insertROW(towns.material,c('door.right','◗','右大門'))

##自動產生城鎮資料
for(t.id in 1:towns.def.num){

        #城鎮基本資料
        town.type <- towns.def.type[1]
        town.name <- towns.def.name[t.id]
        town.x=sample(1:map.x)[1]
        town.y=sample(1:map.y)[1]
        town.symbol <- towns.def.symbol[1]

        world.obj.data <- rbind(world.obj.data,c(town.type,town.name,town.y,town.x,town.symbol))
        world.map[town.y,town.x] <- town.symbol

        #產生城鎮地圖
        ##產生圍牆
        for(k in 1:towns.def.len.x){
                towns.map[1,k,t.id] <-df.lookup(db=towns.material,patten='wall',index=1,output=2)
                towns.map[towns.def.len.y,k,t.id] <-df.lookup(db=towns.material,patten='wall',index=1,output=2)
        }
        
        for(k in 1:towns.def.len.y){
                towns.map[k,1,t.id] <-df.lookup(db=towns.material,patten='wall',index=1,output=2)
                towns.map[k,towns.def.len.x,t.id] <-df.lookup(db=towns.material,patten='wall',index=1,output=2)
        }

        ##產生村鎮大門
        getpos <- player.pos.gen(towns.def.len.y,towns.def.len.x,mode='village')
        towns.map[towns.def.len.y,getpos$x,t.id] <-df.lookup(db=towns.material,patten='entry',index=1,output=2)
        towns.map[towns.def.len.y,getpos$x-1,t.id] <-df.lookup(db=towns.material,patten='door.left',index=1,output=2)
        towns.map[towns.def.len.y,getpos$x+1,t.id] <-df.lookup(db=towns.material,patten='door.right',index=1,output=2)

        ##產生單位城鎮內部相關設施和店舖
        #for(j in 1:length(towns.def.store.symbol)){
        for(j in 1:nrow(towns.def.obj)){

                #設施和店舖基本資料
                #obj.type <- towns.def.store.type[1]
                #obj.name <- towns.def.store.name[j]
                #obj.x <- sample(2:towns.def.len.x-1)[1]
                #obj.y <- sample(2:towns.def.len.y-1)[1]
                #obj.symbol <- towns.def.store.symbol[j]
                #obj.data <- c(t.id,obj.type,obj.name,obj.y,obj.x,obj.symbol)
                #towns.obj.data <- rbind(towns.obj.data,obj.data)

                obj.type <- towns.def.obj$type[j]
                obj.name <- towns.def.obj$name[j]
                obj.x <- sample(2:towns.def.len.x-1)[1]
                obj.y <- sample(2:towns.def.len.y-1)[1]
                obj.symbol <- towns.def.obj$symbol[j]
                obj.data <- c(t.id,obj.type,obj.name,obj.y,obj.x,obj.symbol)
                #towns.obj.data <- rbind(towns.obj.data,obj.data)
                towns.obj.data <- df.insertROW(towns.obj.data,obj.data)
                towns.map[obj.y,obj.x,t.id] <-obj.symbol         
        } 
}
names(world.obj.data) <- c('type','name','y','x','symbol')
names(towns.obj.data) <- c('t.id','type','name','y','x','symbol')

player.dataMGR(info=world.obj.data,path='world.obj.data.csv',mo='w')

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

obj.data.curr <- world.obj.data
map.curr <- world.map
map.curr.name <- world.map.name
map <- screen.generator(map.curr,pos.x,pos.y,scr.size)

#主程式
repeat{


        screen.operator()
        print(map)

        getans <- readline(prompt=paste0("[",map.curr.name," ",pos.y," ",pos.x, "] ","lv",lv," hp",hp," mp",mp," coin",coin," exp",train," (j/l/i/m/o) "))


        if(getans =='i'){
                move.x <- 0
                move.y <- -1
                get.posCheck <- pos.check(pos.x,pos.y,move.x,move.y,scr.size,map.curr)
                pos.y <- get.posCheck[2]
                map <- screen.generator(map.curr,pos.x,pos.y,scr.size)
               
                print("你正往北走...")}
        if(getans =='m'){
                move.x <- 0
                move.y <- 1
                get.posCheck <- pos.check(pos.x,pos.y,move.x,move.y,scr.size,map.curr)
                pos.y <- get.posCheck[2]
                map <- screen.generator(map.curr,pos.x,pos.y,scr.size)

                print("你正往南走...")}
        if(getans =='l'){
                move.x <- 1
                move.y <- 0
                get.posCheck <- pos.check(pos.x,pos.y,move.x,move.y,scr.size,map.curr)
                pos.x <- get.posCheck[1]
                map <- screen.generator(map.curr,pos.x,pos.y,scr.size)

                print("你正往東走...")}
        if(getans =='j'){
                move.x <- -1
                move.y <- 0
                get.posCheck <- pos.check(pos.x,pos.y,move.x,move.y,scr.size,map.curr)
                pos.x <- get.posCheck[1]
                map <- screen.generator(map.curr,pos.x,pos.y,scr.size)

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
                }else{
                        getans <- readline(paste0('你看到了',getBuild$name,' (e)nter?'))
                        if(getans =='e'){

                                getans <- readline(prompt=paste0('準備進入 ',getBuild$name,' ...'))

                                obj.data.curr <- towns.obj.data[towns.obj.data$t.id==getBuild$id,]
                                map.curr <-towns.map[,,getBuild$id] 
                                map.curr.name <- getBuild$name

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
