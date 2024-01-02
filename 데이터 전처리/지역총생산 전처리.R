library(tidyverse)
library(data.table)
library(magrittr)

seoul<-fread("서울특별시_경제활동별_지역내총부가가치_및_요소소득_20220704184831.csv")


# 1. 서울 


original<-seoul[2,] %>% as.vector %>% as.character()
col<-c("행정구역","경제활동별","산출액","중간소비","지역 내 총부가가치","고정자본소모","순기타 생산세",
       "지역내 요소소득","산출액2","지역 내 총부가가치(연쇄)")
colnames(seoul)<-col
seoul<-seoul[-c(1,2),]
seoul<-seoul %>% mutate_at(vars(3:10),as.numeric)



seoul<-seoul %>% filter(행정구역!="서울시") %>%
  mutate(addr1='서울',addr2=행정구역)

fs<-seoul %>% select(행정구역,경제활동별,`지역 내 총부가가치`,addr1,addr2) %>% 
  spread(key=경제활동별,value=`지역 내 총부가가치`)


fs %<>% relocate(c(소계),.after=제조업)


# 2. 인천 


incheon<-fread("인천광역시_경제활동별_지역내총부가가치_및_요소소득_20220704184904.csv")


incheon<-incheon[-c(1,2),]
incheon<-incheon %>% mutate_at(vars(3:10),as.numeric)

incheon<-incheon %>% select(V1,V2,V5) %>% 
  filter(V1!="총계") %>% 
  mutate(addr1='인천',addr2=V1)


fi<-incheon %>% spread(key=V2,value=V5)


# 3. 경기 


gg<-fread("경기도_경제활동별_지역내총부가가치_및_요소소득_20220704185049.csv")

gg<-gg[-c(1,2),]
gg<-gg %>% mutate_at(vars(3:10),as.numeric) %>% 
  select(V1,V2,V5) %>% 
  filter(V1!="경기도") %>% 
  mutate(addr1="경기",addr2=V1)

fg<-gg%>% spread(key=V2,value=V5)

#col<-rbind(colnames(fs),colnames(fi),colnames(fg)) %>% as.data.frame
#write.csv(col,file='변수 통일 필요.csv')

col<-fread("변수 통일 필요.csv")
col %<>% select(-V1)

uni<-col[2,] %>% as.character %>% as.vector
colnames(fs)<-uni
colnames(fi)<-uni
colnames(fg)<-uni

data<-rbind(fs,fi,fg)
data %<>% select(-행정구역) %>% rename('시도'=addr1,'시군구'=addr2,'총부가가치'=소계)
view(data) 

write.csv(data,file="지역내총부가가치.csv")


