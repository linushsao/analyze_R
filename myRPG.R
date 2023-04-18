#
#

#init
rm(list=ls())
#通用函式
pos.check <- function(pos.x,pos.y,scr.size,map.x,map.y){

        if(pos.x >map.x){pos.x <- map.x}
        if(pos.x <1){pos.x <- 1}
        if(pos.y >map.y){pos.y <- map.y}
        if(pos.y <1){pos.y <- 1}

        return(c(pos.x,pos.y))
}

screen.generator <- function(map,pos.x,pos.y,scr.size){

        offset.size <- floor(scr.size*0.5)
        map.x <- ncol(map)
        map.y <- nrow(map)
        scr.playerX <- 0
        scr.playerY <- 0

        a <-readline(prompt=paste(pos.y,pos.x,scr.playerY,scr.playerX))


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
        a <-readline(prompt=paste(pos.y,pos.x,scr.playerY,scr.playerX))
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


#初始設定
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
#產生城鎮座標
town.x=sample(1:map.x)[1]
town.y=sample(1:map.y)[1]
town.symbol='T'
map.all[town.y,town.x] <- town.symbol
#勇者初始位置 
pos.x <- floor(map.x*0.5) 
pos.y <- floor(map.y*0.5)
player.symbol='@'

#
if (!file.exists(player.file)){
        print("第一次進入遊戲，初始化冒險世界及勇者資料...")
        Sys.sleep(1)
        player.dataMGR(player.data,player.file,mo='w')

}else{
        player.data <- as.data.frame(t(player.dataMGR(path=player.file,mo='r')))
        
}

map <- screen.generator(map.all,pos.x,pos.y,scr.size)
#player.data
#player.data[1,] <- c(0,0,0,0,0)

hp =as.numeric(player.data$hp)
mp =as.numeric(player.data$mp)
sp =as.numeric(player.data$sp)
equ =as.numeric(player.data$equ)
att =as.numeric(player.data$att)
lv =as.numeric(player.data$lv)
coin =as.numeric(player.data$coin)
train =as.numeric(player.data$train)



#主程式
repeat{

        print(map)

        getans <- readline(prompt=paste0("lv",lv," hp",hp," mp",mp," coin",coin," exp",train," (j/l/i/m/o) ",town.y,town.x," "))


        if(getans =='i'){
                pos.y <- pos.y -1
                get.posCheck <- pos.check(pos.x,pos.y,scr.size,map.x,map.y)
                pos.y <- get.posCheck[2]
                map <- screen.generator(map.all,pos.x,pos.y,scr.size)
                print(map)
                print("你正往北走...")}
        if(getans =='m'){
                pos.y <- pos.y +1
                get.posCheck <- pos.check(pos.x,pos.y,scr.size,map.x,map.y)
                pos.y <- get.posCheck[2]
                map <- screen.generator(map.all,pos.x,pos.y,scr.size)
                print(map)
                print("你正往南走...")}
        if(getans =='l'){
                pos.x <- pos.x +1
                get.posCheck <- pos.check(pos.x,pos.y,scr.size,map.x,map.y)
                pos.x <- get.posCheck[1]
                map <- screen.generator(map.all,pos.x,pos.y,scr.size)
                print(map)
                print("你正往東走...")}
        if(getans =='j'){
                pos.x <- pos.x -1
                get.posCheck <- pos.check(pos.x,pos.y,scr.size,map.x,map.y)
                pos.x <- get.posCheck[1]
                map <- screen.generator(map.all,pos.x,pos.y,scr.size)
                print(map)
                print("你正往西走...")}
        if(getans =='cl'){
                print("下次見，勇者ˇ...")
                player.dataMGR(player.data,player.file,mo='w')
                break
        }

        if(getans =='o'){print("儲存遊戲資料完成...")
                player.data<- c(hp,mp,lv,coin,train)
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
