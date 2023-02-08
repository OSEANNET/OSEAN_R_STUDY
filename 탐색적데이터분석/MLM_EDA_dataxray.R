#
#
#
# 1기 : 0817
# 2기 : 1821
#
rm(list=ls())

library(tidyverse)
library(readxl)
library(data.table)
library(dataxray)

tdt1 <- read_excel("data/monitoring2008_2017_Item_Corrected.xlsx") %>% as.data.table()
tdt2 <- read_excel("data/Moni1821_Item.xlsx") %>% as.data.table()
msinfo.new <- read_csv("data/MonitoringSiteInfomation_40sites_20220811.csv") %>% as.data.table()

tdt1$Site %>% unique()
msinfo.new$SiteName1st %>% unique()
names(tdt1)[3] <- "SiteName1st"
tdt1$SiteName1st %>% unique()
tdt1 <- tdt1[,-"OseanCode"]
tdt1 <- tdt1[msinfo.new[,c(1,2,14:15,19:20)],on = .(SiteName1st)]

msinfo.new[,SiteName2nd] %>% unique()

#names(tdt2)[4] <- "Site"
tdt2 <- tdt2[msinfo.new[,c(1,2,14:15,17,19:20)],on = .(Site)]
#tdt2 <- inner_join(tdt2,msinfo.new[,c(1,2,14:15,17,19:20)],by="Site")
# decimal date 만들기
Period <- paste0(1:6,"차")
decidate <- round(c(1,3,5,7,9,11)*1/12 ,digits = 3)
pcpd <- data.table(Period,decidate)
tdt1 <- tdt1[pcpd, on = .(Period)][,decidate := Year + decidate]
tdt2 <- tdt2[pcpd, on = .(Period)][,decidate := Year + decidate]

# 연도별 항목별 40개 정점 통계치 구하기
tdt1.freq <- tdt1[, .(Sumfreq=.N),by=.(Site)]
View(tdt1.freq)
 # tdt.freq <- tdt1 %>% group_by(Site,Year) %>% 
 #   summarise(Tfreq=n()) %>% group_by(Site,Year) %>% 
 #   summarise(Sumfreq=sum(Tfreq)/100)#
 # 
ggplot(tdt1.freq,aes(x=Site,y=Sumfreq,group = Site))+
  #geom_bar(stat = "identity",position = "stack")+
  geom_point(color="steelblue") + 
  geom_segment( aes(x=Site, xend=Site, y=0, yend=Sumfreq))+
  #scale_x_continuous(breaks = 2008:2017)+ 
  #scale_y_continuous(breaks = seq(0,6,1))+
  labs(title = "모니터링0817 정점별 총 차수",
       x = "  ",
       y = " Frequency")+
  #facet_wrap(~Site2nd,nrow = 5)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

tdt2.freq <- tdt2[, .(Sumfreq=.N),by=.(Site)]
# tdt.freq <- tdt2 %>% group_by(Site,Year,Item) %>% 
#   summarise(Tfreq=n()) %>% group_by(Site,Year) %>% 
#   summarise(Sumfreq=sum(Tfreq))#

ggplot(tdt2.freq,aes(x=Site,y=Sumfreq))+
  geom_bar(stat = "identity",position = "dodge")+
  #scale_x_continuous(breaks = 2018:2021)+ 
  #scale_y_continuous(breaks = seq(0,600,100))+
  labs(title = "모니터링1821 연도별 정점별 총 차수",
       x = "  ",
       y = " Frequency")+
  #facet_wrap(~Site,nrow = 5)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))
#
#  dataxray  활용 (자료의 양이 많을 경우 시간이 다소 걸림)
#  Viewer에서 확인 가능

tdt1 %>% make_xray() %>% view_xray()
