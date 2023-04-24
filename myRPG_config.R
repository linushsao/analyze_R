#
print('[SYS]載入系統變數')

#系統變數
world.map.name <- '地海世界'
#產生大地圖50x50
map.x <- 51
map.y <- 51

towns.num.limited <- 5 #大地圖城鎮上限數目
towns.def.len.x <- 11 #城鎮基本尺寸
towns.def.len.y <- 11
#顯示範圍9x9
scr.size <- 9
##地圖層級
###+1 進入上一子層級地圖
###-1 回到下一母層級地圖
mode.map <- 0   #0表世界地圖層級
level.upper <- 1  #進入上一子層級地圖
level.lower <- -1 #回到下一母層級地圖

#地圖符號
symbol.map.empty='.'

#玩家符號
player.symbol <- '@'
