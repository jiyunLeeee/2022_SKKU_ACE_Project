library(tidyverse)
library(data.table)
library(magrittr)

data1<-fread("회귀예측 (1).csv")
data2<-fread('호우 피해2.csv')



join<-left_join(data1,data2,by=c('시도','시군구2'))

join$면적<-(join$반지름)^2

data3<-join %>% group_by(시도, 시군구2) %>% summarise(시군구면적=sum(면적))
join<-left_join(join,data3,by=c('시도','시군구2'))

join$`강수 피해액` %<>% as.numeric

join$`면적 비중`<-join$면적/join$시군구면적

join$`면적당 강수피해액`<-(join$`강수 피해액`)*join$`면적 비중`
join %<>% select(c(시도,시군구,읍면동,`면적당 강수피해액`)) 

