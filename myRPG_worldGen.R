#大地圖初始設定
print('[SYS]產生世界設定')

#產生大地圖50x50
##顯示範圍9x9
#map.x <- 51
#map.y <- 51
#scr.size <- 9
###定義城鎮基本資料
world.map.file <- 'world.map.csv'
world.map <- matrix(data=symbol.map.empty,nrow=map.x,ncol=map.y)
#world.map.name <- '地海世界'
#towns.num.limited <- 5 #大地圖城鎮上限數目
#towns.def.len.x <- 11 #城鎮基本尺寸
#towns.def.len.y <- 11

#紀錄世界地圖尺度上之各種物件(城鎮，植物，地形...)座標及代表符號
world.obj.data <- data.frame(symbol=NA,name=NA,type=NA,y=NA,x=NA,sub.group=NA)

meta.obj.data <- data.frame(symbol=NA,name=NA,type=NA,group=NA,sub.group=NA)
meta.obj.data <- df.insertROW(meta.obj.data,c('T','城鎮','town','w',level.upper))
meta.obj.data <- df.insertROW(meta.obj.data,c('C','洞穴','cave','w',level.upper))

##產生城鎮資料
towns.def.obj <- data.frame()
towns.def.num <- sample(1:towns.num.limited)[1] #城鎮數量
towns.def.name <- c('芙雷亞鎮','帕爾鎮','威納鎮','坎帕奧鎮','雷諾瓦鎮')
towns.def.type <- df.lookup(db=meta.obj.data,patten='town',index=3,output=2)
towns.def.symbol <- df.lookup(db=meta.obj.data,patten='town',index=3,output=1)

meta.obj.data <- df.insertROW(meta.obj.data,c('W','武器店','store','t',level.upper))
meta.obj.data <- df.insertROW(meta.obj.data,c('S','防具店','store','t',level.upper))
meta.obj.data <- df.insertROW(meta.obj.data,c('p','藥劑店','store','t',level.upper))
meta.obj.data <- df.insertROW(meta.obj.data,c('I','旅店','store','t',level.upper))
meta.obj.data <- df.insertROW(meta.obj.data,c('T','神廟','temple','t',level.upper))
meta.obj.data <- df.insertROW(meta.obj.data,c('C','墳場','temple','t',level.upper))

towns.def.obj <- meta.obj.data[meta.obj.data$group=='t',]
towns.def.obj <- randomize.df(towns.def.obj)
#產生城鎮尺度之空白地圖陣列
#1.標記單位城鎮內之物件符號(如店鋪)及座標位置
towns.map <- array(symbol.map.empty,c(towns.def.len.x,towns.def.len.y,towns.def.num))
#2.標記單位城鎮地圖內座標之物件(牆，店舖..)相關屬性資料
towns.map.obj <- data.frame(t.id=NA,type=NA,name=NA,y=NA,x=NA,symbol=NA,sub.group=NA)
#3.城鎮組成建材種類資料
meta.material.type <- data.frame(symbol=NA,name=NA,type=NA,group=NA,sub.group=NA)
meta.material.type <- df.insertROW(meta.material.type,c('☐','主入口','entry'     ,'t',level.lower))
meta.material.type <- df.insertROW(meta.material.type,c('∎','石牆'  ,'wall'      ,'t',0))
meta.material.type <- df.insertROW(meta.material.type,c('⁂','白楊樹','tree'      ,'t',0))
meta.material.type <- df.insertROW(meta.material.type,c('◖','左大門','door.left' ,'t',0))
meta.material.type <- df.insertROW(meta.material.type,c('◗','右大門','door.right','t',0))

towns.material <- meta.material.type[meta.material.type$group=='t',] 

##自動產生城鎮資料
for(t.id in 1:towns.def.num){

        #城鎮基本資料
        town.symbol <- towns.def.symbol[1]
        town.name <- towns.def.name[t.id]
        town.type <- towns.def.type[1]
        town.x=sample(1:map.x)[1]
        town.y=sample(1:map.y)[1]
        town.subgroup <- df.lookup(db=meta.obj.data,patten='town',index=3,output=5)

        world.obj.data <- df.insertROW(world.obj.data,c(town.symbol,
                                                        town.name,
                                                        town.type,
                                                        town.y,town.x,
                                                        town.subgroup))
        world.map[town.y,town.x] <- town.symbol

        #產生城鎮地圖
        ##產生圍牆
        for(k in 1:towns.def.len.x){
                towns.map[1,k,t.id] <-df.lookup(db=towns.material,patten='wall',index=3,output=1)
                towns.map[towns.def.len.y,k,t.id] <-df.lookup(db=towns.material,patten='wall',
                                                              index=3,output=1)
        }
        
        for(k in 1:towns.def.len.y){
                towns.map[k,1,t.id] <-df.lookup(db=towns.material,patten='wall',index=3,output=1)
                towns.map[k,towns.def.len.x,t.id] <-df.lookup(db=towns.material,patten='wall',
                                                              index=3,output=1)
        }

        ##產生村鎮大門
        getpos <- player.pos.gen(towns.def.len.y,towns.def.len.x,mode='village')
        towns.map[towns.def.len.y,getpos$x,t.id] <-df.lookup(db=towns.material,patten='entry',
                                                             index=3,output=1)
        towns.map[towns.def.len.y,getpos$x-1,t.id] <-df.lookup(db=towns.material,patten='door.left',
                                                               index=3,output=1)
        towns.map[towns.def.len.y,getpos$x+1,t.id] <-df.lookup(db=towns.material,patten='door.right',
                                                               index=3,output=1)

        ##產生單位城鎮內部相關設施和店舖
        faci.num <- sample(1:nrow(towns.def.obj))[1]
        #for(j in 1:nrow(towns.def.obj)){
        for(j in 1:faci.num){

                #設施和店舖基本資料
                obj.type <- towns.def.obj$type[j]
                obj.name <- towns.def.obj$name[j]
                obj.x <- sample(2:(towns.def.len.x-1))[1]
                obj.y <- sample(2:(towns.def.len.y-1))[1]
                obj.symbol <- towns.def.obj$symbol[j]
                obj.sub.group <- df.lookup(db=towns.material,patten='entry',index=3,output=1)
                obj.data <- c(t.id,obj.type,obj.name,obj.y,obj.x,obj.symbol,obj.sub.group)
                towns.map.obj <- df.insertROW(towns.map.obj,obj.data)
                towns.map[obj.y,obj.x,t.id] <-obj.symbol         
        } 
}

