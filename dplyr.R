# install.packages(c('dplyr','hflights'))

library(dplyr)
library(hflights) # 샘플데이터
library(ggplot2)

dim(hflights) # 데이터 수 확인
hflights_df <- tbl_df(hflights) # 모든 데이터를 화면에 출력하지 않고 내용 파악 가능
hflights_df

filter(hflights_df, Month == 1 & DayofMonth == 1) # 1월 1일의 데이터만 필터
# hflights_df %>% filter(Month == 1 & DayofMonth == 1)

filter(hflights_df, Month == 1 | Month ==2) # 1,2월 데이터 출력 
# hflights_df %>% filter(Month %in% c(1,2)) 

arrange(hflights_df,ArrDelay,Month,Year) # 정렬
#hflights_df %>% arrange(ArrDelay,Month,Year)

arrange(hflights_df,desc(Month)) # 내림차순 정렬
# hflights_df %>% arrange(desc(Month))

select(hflights_df,Year,Month,DayOfWeek)
hflights_df[,c("Year","Month","DayOfWeek")]

hflights_df <- hflights_df %>% mutate(gain = ArrDelay - DepDelay,gain_per_hour = gain/(AirTime/60))

hflights_df %>% summarise(delay = mean(DepDelay,na.rm = T))

planes <- group_by(hflights_df,TailNum)
delay <- summarise(planes,count=n(),dist = mean(Distance,na.rm = T),
                   delay = mean(ArrDelay,na.rm = T))
delay <- delay %>% filter(count>20,dist<2000)

ggplot(delay,aes(dist,delay))+
  geom_point(aes(size=count),alpha = 0.5)+
  geom_smooth()+
  scale_size_area()

a1 <- hflights %>% group_by(Year,Month,DayofMonth)
a2 <- a1[,c(1:3,12,13)]
a3 <- a2 %>% summarise(arr = mean(ArrDelay,na.rm = T),dep = mean(DepDelay,na.rm = T))
a4 <- a3 %>% filter(arr>30 | dep >30)

hflights_df %>% group_by(Year,Month,DayofMonth) %>% 
  summarise(arr = mean(ArrDelay,na.rm = T),dep = mean(DepDelay,na.rm = T)) %>% 
  filter(arr>30 | dep >30)
