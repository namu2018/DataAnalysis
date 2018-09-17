#50page데이터 정제하기 4

getwd()
setwd("C:/Users/ktm/Documents/choi_dontouch/data")
df <- read.table("phone.csv", sep=",")
#############어떤조건에 만족하는 행의 번호를 얻겠다
########which(조건문) 조건문이 만족하는 부분을 true가 되는 행의 번호 추출 
which(df[,1]==2016)

##일일 데이터 사용량이 500 이상인 샘플  
df[df[,9]>=500,]

#4-1-2 데이터 변환
#구간을 나누어 주는 이유: overfitting을 없애주기 위해서
##분석목적에 따라 데이터 변환이 효율적인 때가 있다
##구간화(binning)
names(df)
names(df)[c(4,5,6,9)]<-c('age','height','weight','useDATA')
###구간화
DegreeofAge <- table(cut(df$age, breaks=(1:11)*5))
cut(df$age, breaks=(1:11)*5)

DegreeofAge1 <- table(cut(df$age, breaks=(0:6)*10))
cut(df$age, breaks=(0:6)*10)

max(df$height)
min(df$height)
Degreeofheight <- table(cut(df$height, breaks=(1:6)*3))
cut(df$height, breaks=(14:19)*11)
cut(df$weight, breaks=(10:20)*5)
range(df$weight)
####데이터 변환
###ifelse(조건문, 참일떄 실행값, 거짓일떄 실행값)
df$gen <- ifelse(df$age <30,"young",
                 ifelse(df$age<60,"middle",
                        "old"))
###총계(aggregation)
##두개 이상의 관측치를 하나의 샘플로 합산.
##군집화 
###k평균 알고리즘
km <- kmeans(df[,9],5)
km
km$size
km$iter
a<- df[,9]
b<-km$cluster
df_cluster <- data.frame(value=a, cluster_num=b)
df_cluster
###1번 그룹의 속한 친구들에게 중심값을 치환
km$cluster
km$cluster[km$cluster==1]<- km$centers[1]
for (i in 1:5){
  km$cluster[km$cluster==i] <- km$centers[i]
  }
km$cluster
####height 3개로 나눔  
km_h <- kmeans(df[,5],3)
km_h
for (i in 1:3){
  km_h$cluster[km_h$cluster==i]<- km_h$centers[i]
}
km_h$cluster
summary(km_h)

km_h$centers
km_h
