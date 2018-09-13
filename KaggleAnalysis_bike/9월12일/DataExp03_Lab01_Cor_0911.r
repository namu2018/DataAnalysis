## 01. 데이터 불러오기
getwd()
pf <- read.csv('pseudo_facebook01.tsv', sep='\t')

## 02. 데이터 탐색
str(pf)
dim(pf)
boxplot(pf$age)
plot(pf$age, pf$friend_count)

## 02. 70세 이하의 사람들 상관계수 구하기
## t, df, p-value
## 95% 신뢰 구간에 따라 
library(dplyr)
pf_70 <- pf %>% filter(age<70) 
cor.test(pf_70$age, pf_70$friend_count)

pf_70 <- pf %>% filter(age<=70) 
cor.test(pf_70$age, pf_70$friend_count)

# 70살이 포함되는 것과 포함되지 않는 것에 대한 차이 존재 

### 03. 공분산 구하기 예제 
cov(1:5, 2:6)
cov(1:5, c(3, 3, 3, 3, 3))
cov(1:5, 5:1)

### 03. 공분산 구하기 예제  
### cov(X, Y) = E[(X - E(X))(Y - E(Y))]
### [1,2,3,4,5], [2,3,4,5,6] 공분산 구하기 
a <- c(1,2,3,4,5)
a_mean <- c(3,3,3,3,3)
a_dif <- a - a_mean
a_dif

b <- c(3,4,11,6,12)
b_mean <- rep(mean(b), times=5)
b_dif <- b - b_mean

cov_ab <- sum(a_dif * b_dif)/ (length(a)-1)
cov_ab
cov(a,b)


### 04. 피어선 상관계수
### 공분산(a, b)/a의표준편차*b의표준편차 
### 피어슨의 상관계수는 cor()로 계산
### X와 Y가 함께 변하는 정도/X와 Y가 각각 변하는 정도 
cov_ab/sqrt( var(a)*var(b) )
cor(a, b)

### 04. pearson 상관계수, spearman 상관계수 
with(subset(pf, age<70), cor.test(age, friend_count, 
                                  method="pearson")) # pearson

with(subset(pf, age<=70), cor.test(age, friend_count,
                                   method="spearman")) # spearman


### 05. spearman 상관계수 이해
a <- c(1,2,3,4,5)
a <- rank(a); a
a_mean <- rep(mean(a), times=5)
a_dif <- a - a_mean
a_dif

b <- c(3,4,11,6,12)
b <- rank(b); b
b_mean <- rep(mean(b), times=5)
b_dif <- b - b_mean

cov_ab <- sum(a_dif * b_dif)/ (length(a)-1)
cov_ab
cov(a,b)

### 04. 피어선 상관계수 vs 스피어만 상관계수
### 스피어만 상관계수
### 데이터가 서열 척도의 경우, 즉 자료의 값 대신 순위를 이용한다. 
### 두 변수 간의 연관 관계가 있는지 없는지를 밝혀주며 자료에 이상점이 있거나 표본크기가 작을 때 유용하다
cov_ab/sqrt( var(a)*var(b) )
cor(a, b)

a <- c(1,2,3,4,5)
b <- c(3,4,11,6,12)

# ?cor
cor(a, b, method = "spearman")
cor(a, b, method = "pearson")
cor(a, b, method = "kendall")

### 05. 상관계수 구하기 
library(ggplot2)
names(pf)
sel = c("www_likes_received", "likes_received")
table(pf$www_likes_received)
table(pf$likes_received)

ggplot(aes(x=www_likes_received, y=likes_received),
       data=pf) + geom_point() + 
       xlim(0, quantile(pf$www_likes_received, 0.95))

## 이상치 제외해 보기 
## quantile(pf$likes_received, 0.95)에 해당되는 값 561개 
quantile(pf$likes_received, 0.95)
ggplot(aes(x=www_likes_received, y=likes_received),
       data=pf) + geom_point() + 
      xlim(0, quantile(pf$www_likes_received, 0.95)) +
      ylim(0, quantile(pf$likes_received, 0.95))

## 회귀직선을 그어보자.
ggplot(aes(x=www_likes_received, y=likes_received),
       data=pf) + geom_point() + 
  xlim(0, quantile(pf$www_likes_received, 0.95)) +
  ylim(0, quantile(pf$likes_received, 0.95)) +
  geom_smooth(method="lm", color='red')

## 상관관계 확인
## 매우 높은 상관관계를 갖는다.
## 상관계수는 실제 상관관계를 정량화시킨다.
cor(pf$www_likes_received, pf$likes_received)
with(pf, cor.test(www_likes_received, likes_received))

## age, 
str(pf)
