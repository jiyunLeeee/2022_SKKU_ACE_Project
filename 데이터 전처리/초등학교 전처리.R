library(tidyverse)
library(data.table)
library(magrittr)



school<-fread("C:/Users/shn20/Desktop/작업물/통계분석학회/여름스터디/자료 다운로드/초등학교수_시도_시_군_구__20220706211504.csv")



seoul<-which(school$V1=='서울특별시')
incheon<-which(school$V1=='인천광역시')
gg<-which(school$V1=='경기도')
c(seoul,incheon,gg)



school %<>% mutate(addr1=V1,addr2=V1)



for (i in c(1:nrow(school))){
  if (i<28){school$addr1[i]='서울'}else{
    if(i<39){school$addr1[i]='인천'}else{
      school$addr1[i]='경기'}}}



#school<-school[-c(seoul,incheon,gg),]
#school<-school[-1,]



school %<>% select(-V1) %>% relocate(c(addr1,addr2)) %>% 
  rename('시도'=addr1,'시군구'=addr2,'2019초교수'=V2,'2020초교수'=V3,'2021초교수'=V4)



x<-c(1,2,28,39)
school<-school[-x,]
view(school)



write.csv(school,file="초등학교.csv")


