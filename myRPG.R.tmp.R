#
#init
rm(list=ls())

#通用函式
print('[SYS]載入通用函式...')

lib.extra <- '~/src/analyze_R/lib'
source('myRPG_config.R')#建立系統變數
source('myRPG_api.R')#地圖通用函式
source('myRPG_worldGen.R') #地圖產生函式
source('myRPG_playerGen.R') #勇者產生函式
source('myRPG_monsterGen.R') #生物產生函式

source(paste(lib.extra,'misc.R',sep='/'))#通用函式
#===================主程式=================

#儲存地圖資料
player.dataMGR(info=towns.obj.data,path='towns.obj.data.csv',mo='w')

#維度編號(0:世界地圖 1:城鎮... 2:城鎮內建物設施)
env.map <- rep(0,10)
env.map.x <- rep(0,10)
env.map.y <- rep(0,10)

#首次更新地圖層級
obj.data.curr <- get.map.metadata(level=env.map[1],md='obj.data') 
map.curr <- get.map.metadata(level=env.map[1],md='map') 
map.curr.name <- get.map.metadata(level=env.map[1],md='title') 

map.curr.x <- ncol(map.curr) 
map.curr.y <- nrow(map.curr)
getpos <- player.pos.gen(map.curr.y,map.curr.x)
                                        
pos.x <- getpos$x
pos.y <- getpos$y

map <- screen.generator(map.curr,pos.x,pos.y)
screen.operator()

#主程式
key.pre <-NULL

repeat{

        display.map(map)

        getans <- readline(prompt=paste0("[",map.curr.name," ",pos.y," ",pos.x, "] ","lv",lv," hp",hp," mp",mp," coin",coin," exp",train," (j/l/i/m/o) "))

        #檢查輸入按鍵
        if(!is.null(key.pre) && getans ==''){
                getans <- key.pre
        }else if(getans !=''){
                key.pre <- getans
        }
        
        if(getans =='i'){
                move.x <- 0
                move.y <- -1
                get.posCheck <- pos.check(pos.x,pos.y,move.x,move.y,map.curr)
                pos.y <- get.posCheck[2]
                map <- screen.generator(map.curr,pos.x,pos.y)
               
                screen.operator()
                print("你正往北走...")}
        
        if(getans =='m'){
                move.x <- 0
                move.y <- 1
                get.posCheck <- pos.check(pos.x,pos.y,move.x,move.y,map.curr)
                pos.y <- get.posCheck[2]
                map <- screen.generator(map.curr,pos.x,pos.y)

                screen.operator()
                print("你正往南走...")}
       
        if(getans =='l'){
                move.x <- 1
                move.y <- 0
                get.posCheck <- pos.check(pos.x,pos.y,move.x,move.y,map.curr)
                pos.x <- get.posCheck[1]
                map <- screen.generator(map.curr,pos.x,pos.y)

                screen.operator()
                print("你正往東走...")}
   
        if(getans =='j'){
                move.x <- -1
                move.y <- 0
                get.posCheck <- pos.check(pos.x,pos.y,move.x,move.y,map.curr)
                pos.x <- get.posCheck[1]
                map <- screen.generator(map.curr,pos.x,pos.y)

                screen.operator()
                print("你正往西走...")}
        
        if(getans =='look' || getans =='lk'){
                
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
                        info.entry <- ifelse(getBuild$action==0,
                                                '',
                                                ' (e)nter ')
                        getans <- readline(paste0('你看到了',getBuild$name,info.entry))
                        if((getans =='e' || getans =='k') 
                           && getBuild$sub.group !=0){

                                getans <- readline(prompt=paste0('準備進入 ',getBuild$name,' ...'))
                                
                                status.updn <- as.numeric(getBuild$action)
                                #更新地圖層級
                                ##往上一子層級地圖前，儲存目前地圖層級及座標
                                a <-readline("T1");
                                if(status.updn>0)
                                {
                                        env.map <- push.pull(vec=env.map,
                                                             num=mode.map,
                                                             md=status.updn)
                                        env.map.x <- push.pull(vec=env.map.x,
                                                             num=pos.x,
                                                             md=status.updn)
                                        env.map.y <- push.pull(vec=env.map.y,
                                                             num=pos.y,
                                                             md=status.updn)
                                }
                                
                                a <-readline("T1");

                                #更新地圖層級
                                mode.map <- mode.map +status.updn
                                mode.map <- ifelse(mode.map<0,0,mode.map)
                                a <-readline("T1");
                              
                                #更新當前使用之地圖資料陣列
                                obj.data.curr <- get.map.metadata(level=mode.map,md='obj.data',info=getBuild) 
                                map.curr <- get.map.metadata(level=mode.map,md='map',info=getBuild) 
                                map.curr.name <- get.map.metadata(level=mode.map,md='title',info=getBuild) 
                                a <-readline("T1");

                                #取得勇者正確之位置座標
                                if(status.updn >0)
                                {
                                        #進入上一子層級地圖前，取得勇者進入時之預設起始座標
                                        map.curr.x <- ncol(map.curr) 
                                        map.curr.y <- nrow(map.curr)
                                        getpos <- player.pos.gen(map.curr.y,map.curr.x,mode='village')
                                        
                                        pos.x <- getpos$x
                                        pos.y <- getpos$y

                                }else if(status.updn <0)
                                        {
                                                #回到下一母層級地圖前，取得勇者前次座標
                                                #取得前次座標
                                                mode.map <- env.map[1]
                                                pos.x <- env.map.x[1]
                                                pos.y <- env.map.y[1]
                                                #更新env.map資料
                                                env.map <- push.pull(vec=env.map,
                                                     num=mode.map,
                                                     md=pn(status.updn))
                                                env.map.x <- push.pull(vec=env.map.x,
                                                     num=pos.x,
                                                     md=pn(status.updn))
                                                env.map.y <- push.pull(vec=env.map.y,
                                                     num=pos.y,
                                                     md=pn(status.updn))

                                        }

                                map <- screen.generator(map.curr,pos.x,pos.y)
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
