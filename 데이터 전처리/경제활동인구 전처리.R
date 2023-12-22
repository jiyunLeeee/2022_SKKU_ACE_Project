
library(tidyverse)
library(data.table)
library(magrittr)

# 1.경제 활동 인구 데이터 전처리 

population<-fread("시군구_경제활동인구_총괄_20220704185535.csv")

colnames(population)<-population[1,] %>% as.vector %>% as.character()

population %<>% select(-c(`경제활동인구 (천명)`,
                         `취업자 (천명)`,
                         `실업자 (천명)`,
                         `비경제활동인구 (천명)`,
                         `15세이상인구 (천명)`,
                         `실업자(C.V.)`,
                         `실업률(C.V.)`))

population<-population[-1,]

x<-str_split_fixed(population$행정구역별,' ',2)
addr1<-c()
addr2<-c()
for (i in c(1:nrow(population))){
  if (i<=74){addr2<-c(addr2,x[i,2])}else{
    addr2<-c(addr2,x[i,1])
  }
}
population %<>% mutate('행정구역2'=addr2)

sudo<-c('종로구','중구','용산구','성동구','광진구','동대문구',
        '중랑구','성북구','강북구','도봉구','노원구','은평구',
        '서대문구','마포구','양천구','강서구','구로구','금천구',
        '영등포구','동작구','관악구','서초구','강남구','송파구',
        '강동구','동구','미추홀구','연수구','남동구','부평구',
        '계양구','서구','강화군','옹진군','수원시','고양시','용인시',
        '성남시','안양시','부천시','안산시','화성시','남양주시',
        '평택시','의정부시','광명시','동두천시','과천시','구리시',
        '오산시','시흥시','군포시','의왕시','하남시','파주시',
        '이천시','안성시','김포시','광주시','양주시','포천시','여주시',
        '연천군','가평군','양평군')

population %<>% filter(행정구역2 %in% sudo)

x<-str_split_fixed(population$행정구역별,' ',2)
addr1<-c()
for (i in c(1:nrow(population))){
  if (i<=49){addr1<-c(addr1,x[i,1])}else{
    addr1<-c(addr1,'경기')
  }
}

population %<>% mutate('행정구역1'=addr1)

population %<>% filter(행정구역1 %in% c("서울","경기","인천"))

population %<>% select(-행정구역별) %>% relocate(c(행정구역1,행정구역2)) %>% 
  rename('시도'=행정구역1,'시군구'=행정구역2)


write.csv(population,file="경제활동인구.csv")



