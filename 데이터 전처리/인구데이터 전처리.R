library(tidyverse)
library(data.table)
library(magrittr)

people<-fread("C:/Users/shn20/Desktop/작업물/통계분석학회/여름스터디/자료 다운로드/시군구_성_연령_5세_별_주민등록연앙인구_20220708135553.csv")
people %<>% filter(성별=='계') %>% select(-c(성별,계))

seoul<-which(people$`행정구역(시군구)별`=='서울특별시')
incheon<-which(people$`행정구역(시군구)별`=='인천광역시')
gg<-which(people$`행정구역(시군구)별`=='경기도')
c(seoul,incheon,gg)

people %<>% mutate(addr1=`행정구역(시군구)별`,addr2=`행정구역(시군구)별`)

for (i in c(1:nrow(people))){
  if (i<27){people$addr1[i]='서울'}else{
    if(i<38){people$addr1[i]='인천'}else{
      people$addr1[i]='경기'}}}

people %<>% select(-`행정구역(시군구)별`) %>% relocate(c(addr1,addr2)) %>% 
  rename('시도'=addr1,'시군구'=addr2)

x<-c(1,27,38)
people<-people[-x,]
view(people)

write.csv(people,file="인구데이터.csv")
