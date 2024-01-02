library(tidyverse)
library(data.table)
library(magrittr)
library(dbscan)
library("factoextra")

data<-fread("최종2차.csv") 
geo<-data %>% select(c(시도,시군구,읍면동))
data %<>% select(-c(시도,시군구,읍면동)) 

kNNdistplot(data,k=4)
abline(h=0.045,lty=2)

set.seed(1004)
library(fpc)

db <- dbscan(data, eps = 0.045, MinPts = 4)
fviz_cluster(db, data, stand = FALSE, frame = FALSE, geom = "point")

data$cluster=db$cluster

nor_minmax = function(x){
  result = (x - min(x)) / (max(x) - min(x))
  return(result)}

data2<-fread("최종파일.csv")
geo2<-data2 %>% select(c(시도,시군구,읍면동))
data2 %<>% select(-c(시도,시군구,읍면동,`면적당 강수피해액`))%>%
  nor_minmax%>%
  as.data.frame

kNNdistplot(data2,k=4)
abline(h=0.012,lty=2)

db2 <- dbscan(data2, eps = 0.012, MinPts = 4)
fviz_cluster(db2, data2, stand = T, frame = FALSE, geom = "point")+theme_bw()+
  theme(plot.title=element_text(hjust=0.5))+
  ggtitle("DBSCAN PLOT")

data2$cluster=db2$cluster

forbar<-db2$cluster %>% table %>% as.data.frame
forbar %<>% rename('cluster'='.')

color<-c('black','red','green','blue')
ggplot(forbar,aes(cluster,Freq,fill=cluster))+
  geom_bar(stat = 'identity',alpha=0.4)+
  theme_bw()+
  theme(plot.title=element_text(hjust=0.5),legend.position='none')+
  scale_fill_manual(values=color)+
  ylab('')+xlab('DBSCAN 클러스터')

geo2[which(data2$cluster %in% c(0,2,3)),] %>% view

library(dbscan)
hdb.model<-hdbscan(data,minPts=4)
plot(hdb.model,show_flat=T)


library(isotree)

set.seed(2829)
model<-isolation.forest(data)
anomaly_score<-predict(model,data)
iso_result<-anomaly_score
outlier_val<-quantile(iso_result,0.75)+1.5*IQR(iso_result)
iso_result<-ifelse(iso_result>=0.55,1,0)
geo[which(iso_result==1),] %>% view
final<-cbind(geo,data,iso_result) %>% as.data.frame
final %<>% select(-cluster) 
final$iso_2<-final$iso_result

for (i in c(1:ncol(final))){
  if(final$iso_result[i]==0){
    final$iso_2[i]<-final$iso_result[i]}else{
      if(final$시군구[i] %in% c('포천시','가평군','연천군')==T){
        final$iso_2[i]<-2}else{final$iso_2[i]<-1}
      }
    }

index<-which(final$시군구 %in% c('포천시','가평군','연천군')==TRUE)
final$iso_2[index]<-2
final$iso_2 %<>% as.factor
final %<>% rename('클러스터링'='iso_2') %>% 
  mutate_if(is.numeric,nor_minmax) %>% as.data.frame

var1<-ggplot(final,aes(클러스터링,`부동산 단위 가격`,color=클러스터링))+
  geom_boxplot()+
  theme_classic()

var2<-ggplot(final,aes(클러스터링,강수피해,color=클러스터링))+
  geom_boxplot()+
  theme_classic()

var3<-ggplot(final,aes(클러스터링,인구,color=클러스터링))+
  geom_boxplot()+
  theme_classic()

var4<-ggplot(final,aes(클러스터링,`전력소모량(인당)`,color=클러스터링))+
  geom_boxplot()+
  theme_classic()

var5<-ggplot(final,aes(클러스터링,산업발전,color=클러스터링))+
  geom_boxplot()+
  theme_classic()

var1<-ggplot(final,aes(`부동산 단위 가격`,color=클러스터링))+
  geom_line(stat='density')+
  theme_classic()+
  facet_wrap(vars(클러스터링),ncol=3,scales='free')

var2<-ggplot(final,aes(강수피해,color=클러스터링))+
  geom_line(stat='density')+
  theme_classic()+
  facet_wrap(vars(클러스터링),ncol=3,scales='free')

var3<-ggplot(final,aes(인구,color=클러스터링))+
  geom_line(stat='density')+
  theme_classic()+
  facet_wrap(vars(클러스터링),ncol=3,scales='free')

var4<-ggplot(final,aes(`전력소모량(인당)`,color=클러스터링))+
  geom_line(stat='density')+
  theme_classic()+
  facet_wrap(vars(클러스터링),ncol=3,scales='free')

var5<-ggplot(final,aes(산업발전,color=클러스터링))+
  geom_line(stat='density')+
  theme_classic()+
  facet_wrap(vars(클러스터링),ncol=3,scales='free')

var1
