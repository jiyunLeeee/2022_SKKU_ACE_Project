library(tidyverse)
library(data.table)
library(magrittr)

data1<-fread("쓰레기 처리 시설.csv")
data2<-fread("시군구위도경도.csv")

view(data1);view(data2)

data<-inner_join(data1,data2,by=c('시도','시군구'))
view(data)
write.csv(data,file='폐기물 처리시설&행정구역 주소.csv')
