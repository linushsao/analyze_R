#系統變數設定
print('[SYS]載入系統變數')

#
library(zoo)
#系統變數
#world.map.name <- '地海世界'
sample.obj.data <- data.frame(symbol=NA,id=NA,name=NA,action=NA,y=NA,x=NA,group=NA,sub.group=NA)

#產生大地圖50x50
map.x <- 51
map.y <- 51

towns.num.limited <- 5 #大地圖城鎮上限數目
towns.def.len.x <- 11 #城鎮基本尺寸
towns.def.len.y <- 11
stores.num.limited <- 10 #城鎮地圖內店舖之上限數目
stores.def.len.x <- 5  #店舖基本尺寸
stores.def.len.y <- 7 

#顯示範圍
scr.size.data <- c(13 #1>mode.map=0 之顯示範圍
                   ,9 #2>mode.map=1 之顯示範圍
                )

##地圖層級
###+1 進入上一子層級地圖
###-1 回到下一母層級地圖
mode.map <- 0   #0表世界地圖層級
status.updn <- 0  #地圖層級切換方向
level.upper <- 1  #進入上一子層級地圖
level.lower <- -1 #回到下一母層級地圖

#符號集合
###世界地形符號
symbol.map.empty='.'    #空白土地

###城鎮地圖符號
symbol.root.world   ='W'#世界
symbol.world.town   ='TN'#城鎮

symbol.town.market  ='M'#雜貨店
symbol.town.weapon  ='W'#武器店
symbol.town.shield  ='S'#防具店
symbol.town.potion  ='P'#藥劑店
symbol.town.inn     ='I'#旅店
symbol.town.temple  ='T'#神廟
symbol.town.cemetery='C'#墳場

###玩家符號
player.symbol <- '@'

#層級編號定義
lev.root =0 #種類名稱

#群組編號定義
def.world   =1000#世界

def.town    =100 #城鎮
def.cave    =200 #洞穴

def.store   =10  #商店
def.inn     =20  #旅店
def.temple  =30  #神廟
def.cemetery=40  #墳場
def.material=90  #材料

def.s.weapon  =1#武器店
def.s.shield  =2#防具店
def.s.market  =3#雜貨店
def.s.potion  =4#藥劑店
def.i.inn     =5#旅店
def.t.temple  =6#神廟
def.c.cemetery=7#墳場

def.name.world='wo'
def.name.town='tow'
def.name.store='st'
def.name.material='ma'
#中文名稱



