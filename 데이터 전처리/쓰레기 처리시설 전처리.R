library(tidyverse)
library(data.table)
library(magrittr)

data1<-fread("C:/Users/shn20/Desktop/작업물/통계분석학회/여름스터디/전처리/쓰레기 처리 시설.csv")
data2<-fread("C:/Users/shn20/Desktop/작업물/통계분석학회/여름스터디/자료 다운로드/시군구위도경도.csv")

view(data1);view(data2)

data<-inner_join(data1,data2,by=c('시도','시군구'))
view(data)
write.csv(data,file='폐기물 처리시설&행정구역 주소.csv')
