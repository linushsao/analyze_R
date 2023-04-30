#通用函式
print('[SYS]載入通用函式...')

#rm(list=ls())

#lib.extra <- '~/src/analyze_R/lib'
#source('myRPG_config.R')#建立系統變數
#source('myRPG_api.R')#地圖通用函式
#source('myRPG_worldGen.R') #地圖產生函式
#source('myRPG_playerGen.R') #勇者產生函式
#source('myRPG_monsterGen.R') #生物產生函式

#source(paste(lib.extra,'misc.R',sep='/'))#通用函式

#大地圖初始設定
print('[SYS]產生世界設定')

#基本變數
towns.def.num <- sample(1:towns.num.limited)[1] #城鎮數量
#通用變數
###1.設定儲存單位母物件(group，如世界地圖)內附屬子物件(sub.group，如世界地圖下的城鎮..洞穴)之第一層屬性欄位
meta.obj.data <- sample.obj.data 

#產生各層級資料框
###定義世界尺度之地圖資料儲存檔案
worlds.map.file <- 'world.map.csv'
#####1.產生地圖矩陣空間：儲存單位世界地圖內之物件符號(城鎮 洞穴...)
worlds.map <- matrix(data=symbol.map.empty,nrow=map.x,ncol=map.y)
#####2.設定儲存單位世界地圖內附屬物件(城鎮 洞穴..)之第一層屬性欄位(映射world.map上的座標)
#####3.設定儲存所有物件之第二層屬性欄位(細節)
worlds.def.name <- c('地海')

###定義城鎮尺度之地圖資料儲存檔案
towns.map.file <- 'towns.map.csv'
#####1.產生地圖陣列空間：儲存單位城鎮內之物件符號(店舖)
towns.map <- array(symbol.map.empty,c(towns.def.len.y,towns.def.len.x,towns.def.num))
#####2.設定儲存單位城鎮地圖內附屬物件(店舖..)之第一層屬性欄位(映射towns.map上的座標)
#####3.設定儲存單位城鎮地圖內附屬物件(店舖..)之第二層屬性欄位(各種材料)
#####4.定義城鎮基本資料
towns.def.name <- c('芙雷亞','帕爾','威納','坎帕奧','雷諾瓦')

###定義店舖尺度之地圖資料儲存檔案
stores.map.file <- 'stores.map.csv'
#####1.產生地圖陣列空間：儲存單位店舖內之物件符號(櫃台 桌椅)
stores.map <- array(symbol.map.empty,c(stores.def.len.y,stores.def.len.x,stores.num.limited))
#####2.設定儲存單位店舖地圖內附屬物件(櫃台 桌椅)之第一層屬性欄位(映射stores.map上的座標)
stores.bytowns.obj.data <- sample.obj.data
#####3.設定儲存單位店舖地圖內附屬物件(櫃台 桌椅)之第二層屬性欄位(各種材料)
#####4.定義店舖基本資料
stores.def.name <- c('雜貨店','武器店','防具店','藥劑店','旅店','神廟','墳場')
stores.def.symbol <- c(symbol.town.market, symbol.town.weapon, symbol.town.shield, symbol.town.potion,
                       symbol.town.inn, symbol.town.temple, symbol.town.cemetery)
def.stores.type <- c(rep(def.store,4),def.inn,def.temple,def.cemetery)

#產生資料細節
###1.產生世界地圖尺度上之附屬物件資料
#####1-1.定義第一層尺度物件分類及個別名稱
#######sub.group=0表母物件編號，>0表其下同階層子物件
for(i in 1:length(worlds.def.name)){
        meta.obj.data <- df.insertROW(meta.obj.data,
                              c('','',worlds.def.name[i],0,0,0,def.world,i))
}
#####1-2.定義第二層尺度附屬物件分類名稱
for(i in 1:length(towns.def.name)){
        meta.obj.data <- df.insertROW(meta.obj.data,
                              c(symbol.world.town,'',towns.def.name[i],0,0,0,def.world+def.town,0))
       
}
#####1-3.定義第三層尺度附屬物件分類名稱
for(i in 1:length(stores.def.name)){
        meta.obj.data <- df.insertROW(meta.obj.data,
                              c(stores.def.symbol[i],'',stores.def.name[i],0,0,0,def.world+def.town+def.stores.type[i],0))
       
}

###3.產生店舖尺度上之附屬物件資料
###
#####2-1.由附屬物件資料庫萃取出城鎮所屬物件資料
###3.產生單位物件組成資料
#####3-1.產生城鎮地圖組成建材資料
tmp.data <- data.frame(a=NA,b=NA,c=NA,d=NA)
tmp.data <- df.insertROW(tmp.data,c('☐','entry','主入口',level.lower))
tmp.data <- df.insertROW(tmp.data,c('∎','wall','石牆',0))
tmp.data <- df.insertROW(tmp.data,c('⁂','tree','白楊樹',0))
tmp.data <- df.insertROW(tmp.data,c('◖','door.left','左大門',0))
tmp.data <- df.insertROW(tmp.data,c('◗','door.right','右大門',0))
tmp.data
for(i in 1:nrow(tmp.data)){

        meta.obj.data <- df.insertROW(meta.obj.data,c(df.row2vector(tmp.data[i,]),
                                                      0,0,
                                                      def.world+def.town+def.material,
                                                      i))
}
#####3-2.產生店舖地圖組成建材資料
tmp.data <- data.frame(a=NA,b=NA,c=NA,d=NA)
tmp.data <- df.insertROW(tmp.data,c('☐','store.entry','店舖入口',level.lower))
tmp.data <- df.insertROW(tmp.data,c('∎','wall.brick','磚牆',0))
for(i in 1:nrow(tmp.data)){

        meta.obj.data <- df.insertROW(meta.obj.data,c(df.row2vector(tmp.data[i,]),
                                                      0,0,
                                                      def.world+def.town+def.material,
                                                      i))
}

###4.產生城鎮工業製品種類資料
tmp.data <- data.frame(a=NA,b=NA,c=NA,d=NA,e=NA,f=NA,g=NA)
tmp.data <- df.insertROW(tmp.data,c('','parchment.rolls','羊皮卷',0,0,0,def.world+def.town+def.store+def.s.market))
tmp.data <- df.insertROW(tmp.data,c('','feather.pen','羽毛筆',0,0,0,def.world+def.town+def.store+def.s.market))
tmp.data <- df.insertROW(tmp.data,c('','ink.black','黑墨水',0,0,0,def.world+def.town+def.store+def.s.market))
tmp.data <- df.insertROW(tmp.data,c('','dagger','匕首',0,0,0,def.world+def.town+def.store+def.s.weapon))
tmp.data <- df.insertROW(tmp.data,c('','penknife','小刀',0,0,0,def.world+def.town+def.store+def.s.weapon))
tmp.data <- df.insertROW(tmp.data,c('','shield.wood','木盾',0,0,0,def.world+def.town+def.store+def.s.shield))
tmp.data <- df.insertROW(tmp.data,c('','potion.red','紅水',0,0,0,def.world+def.town+def.store+def.s.potion))
tmp.data <- df.insertROW(tmp.data,c('','beer.black','濃麥酒',0,0,0,def.world+def.town+def.inn+def.i.inn))
tmp.data <- df.insertROW(tmp.data,c('','apple','蘋果',0,0,0,def.world+def.town+def.inn+def.i.inn))
tmp.data <- df.insertROW(tmp.data,c('','curufix','十字架',0,0,0,def.world+def.town+def.temple+def.t.temple))
tmp.data <- df.insertROW(tmp.data,c('','bone','人骨',0,0,0,def.world+def.town+def.cemetery+def.c.cemetery))

for(i in 1:nrow(tmp.data)){

        meta.obj.data <- df.insertROW(meta.obj.data,c(df.row2vector(tmp.data[i,]),
                                                      i))
}
#meta.obj.data

#物件資料分類庫萃
#####2-1.由物件資料庫萃取出基本資料分類
#產生世界/城鎮地圖詳細資料
worlds.obj.data <- data.frame()
###1.取出城鎮所屬物件資料
towns.obj.data <- meta.obj.data[meta.obj.data$group==def.world+def.town,]
towns.obj.data <- randomize.df(towns.obj.data,2)
row.names(towns.obj.data) <- NULL
towns.obj.data
###2.取出店舖所屬物件資料
stores.obj.data.store <- meta.obj.data[meta.obj.data$group==def.world+def.town+def.store,] 
stores.obj.data.inn <- meta.obj.data[meta.obj.data$group==def.world+def.town+def.inn,] 
stores.obj.data.temple <- meta.obj.data[meta.obj.data$group==def.world+def.town+def.temple,] 
stores.obj.data.cemetery <- meta.obj.data[meta.obj.data$group==def.world+def.town+def.cemetery,] 
stores.obj.data <- rbind(stores.obj.data.store,
                        stores.obj.data.inn,
                        stores.obj.data.temple,
                        stores.obj.data.cemetery)

row.names(stores.obj.data) <- NULL
stores.obj.data
#取出其他材料
material.obj.data <- meta.obj.data[meta.obj.data$group==def.world+def.town+def.material,] 
row.names(material.obj.data) <- NULL
material.obj.data

towns.def.num

#產生各層級地圖及附屬物件相關資料
for(t.id in 1:towns.def.num){
        #城鎮基本資料
        towns.obj.data$symbol[t.id] <-symbol.world.town 
        towns.obj.data$action[t.id] <- level.upper
        towns.obj.data$y[t.id]=sample(1:map.y)[1]
        towns.obj.data$x[t.id]=sample(1:map.x)[1]
        towns.obj.data$sub.group[t.id] <- t.id
        #於世界地圖陣列內相對應位置寫入城鎮符號
        worlds.map[as.numeric(towns.obj.data$y[t.id]),as.numeric(towns.obj.data$x[t.id]) ] <-symbol.world.town
        group.combine <-as.numeric(towns.obj.data$group[t.id]) +as.numeric(towns.obj.data$sub.group[t.id])
        #產生城鎮地圖
        ###產生外圍圍牆
        towns.map <- map.generator(m.df=towns.map, obj.df=meta.obj.data, md='wall.around',
                                        symbol=df.lookup(db=material.obj.data,
                                                        patten='wall',index=2,output=1),
                                        map.lenx=towns.def.len.x,
                                        map.leny=towns.def.len.y,
                                        action=0,
                                        group=group.combine,
                                        belong.to=t.id)
        
        getpos <- player.pos.gen(towns.def.len.y,towns.def.len.x,mode='village')
        ###產生城鎮大門
        towns.map <- map.generator(m.df=towns.map, obj.df=meta.obj.data, md='single.sample',
                                symbol=df.lookup(db=material.obj.data,
                                                patten='entry',index=2,output=1),
                                map.lenx=getpos$x,
                                map.leny=towns.def.len.y,
                                action=level.lower,
                                group=group.combine,
                                belong.to=t.id)
        towns.map <- map.generator(m.df=towns.map, obj.df=meta.obj.data, md='single.sample',
                                symbol=df.lookup(db=material.obj.data,
                                                patten='door.left',index=2,output=1),
                                map.lenx=getpos$x-1,
                                map.leny=towns.def.len.y,
                                group=group.combine,
                                belong.to=t.id)
        towns.map <- map.generator(m.df=towns.map, obj.df=meta.obj.data, md='single.sample',
                                symbol=df.lookup(db=material.obj.data,
                                                patten='door.right',index=2,output=1),
                                map.lenx=getpos$x+1,
                                map.leny=towns.def.len.y,
                                group=group.combine,
                                belong.to=t.id)
        
        ##產生單位城鎮內部相關設施和店舖
        faci.num <- sample(1:nrow(stores.obj.data))[1]

        for(j in 1:faci.num){
                ##設施和店舖基本資料
                getpos <- player.pos.gen(stores.def.len.y,stores.def.len.x,mode='village')

                tmp.data <-stores.obj.data[j,] 
                tmp.data$x <- sample(2:(towns.def.len.x-1))[1]
                tmp.data$y <- sample(2:(towns.def.len.y-1))[1]
                tmp.data$action <- level.upper
                tmp.data$group <-group.combine
                tmp.data$sub.group <- j
                stores.bytowns.obj.data <- df.insertROW(stores.bytowns.obj.data,tmp.data)
                towns.map[tmp.data$y,tmp.data$x,t.id] <- tmp.data$symbol
                ##產生店舖平面圖
                stores.map <- map.generator(m.df=stores.map,obj.df=stores.bytowns.obj.data,
                                                md='wall.around',
                                                symbol=df.lookup(db=material.obj.data,
                                                        patten='wall.brick',index=2,output=1),
                                        map.lenx=stores.def.len.x,map.leny=stores.def.len.y
                                        ,group=group.combine,belong.to=t.id)
                ###產生店舖大門
                stores.map <- map.generator(m.df=stores.map,obj.df=stores.bytowns.obj.data,
                                            md='single.sample',
                                symbol=df.lookup(db=material.obj.data,
                                                patten='store.entry',index=2,output=1),
                                map.lenx=getpos$x,map.leny=stores.def.len.y
                                ,group=group.combine ,belong.to=t.id)

                ##

        } 
}

towns.obj.data
towns.obj.data <-towns.obj.data[towns.obj.data$action !=0,] 
