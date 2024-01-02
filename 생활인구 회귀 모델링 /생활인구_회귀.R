library(tidyverse)
library(data.table)
library(magrittr)

library(gvlma)
library(lmtest)
library(car)
options(scipen=99)

data1<-fread('회귀x변수.csv')
data2<-fread('행정동 생활인구.csv')

x<-data1[which(data1$시도=='서울'),]
practice<-left_join(x,data2,
                    by=c('시도','시군구','읍면동','행정동 코드'))

# 1. 아무처리도 안 하고 회귀 

xy<-practice %>% select(-c(
  '시도','시군구','읍면동','행정동 코드','위도','경도'))

fit<-lm(data=xy,총생활인구수~.)
summary(fit)

vif(fit)

library(corrplot)
library(ggcorrplot)

xy %<>% na.omit
xy_cor = cor(xy)
ggcorrplot(xy_cor,type="lower", lab=T,lab_size=3)+theme_classic()+labs(title="상관관계")+
  theme(axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5))

null<-lm(data=xy,총생활인구수~1)
full<-lm(data=xy,총생활인구수~.)
step_result<-step(null,scope=list(lower=null,upper=full),direction='both')

stepfit<-lm(formula = 총생활인구수 ~ 편의점 + `2022년06월_총인구수` + 
    `토지 매매가` + 관광명소 + 카페, data = xy)
summary(stepfit)

plot(stepfit)
gvlma(stepfit)

shapiro.test(stepfit$residuals)
ncvTest(stepfit)


# 2. minmax scaling
nor_minmax = function(x){
  result = (x - min(x)) / (max(x) - min(x))
  return(result)}

y<-xy$총생활인구수 
xy %<>% select(-총생활인구수) %>% apply(nor_minmax,2)
xy<-cbind(xy,y) %>% rename('총생활인구수'='y')
