setwd("E:/BigData3/dataset/mouda/2ndCompetitionData")
test <- read.csv("test.csv")
tr <- read.csv("train.csv", na.strings=c("",NA))

library(dplyr)
tr_a <- tr
test_a <- test

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# df_NA : 결측 처리할 데이터 셋
# sel : 몇번째 컬럼인가?
# wantedVal : 'mean', 'median', 'mode'
NAProcess <- function(df_NA, sel, wantedVal='mean') {
  for  (i in sel) {
    type = class(df_NA[[i]])
    
    if (wantedVal=='mean') {
      ## 평균값을 구하고, 그 값으로 위치 찾고, 값 치환 
      Val = mean( df_NA[[i]], na.rm=T)
      loc = which(is.na(df_NA[[i]]))
    } else if (wantedVal=='median') {
      Val = median( df_NA[[i]], na.rm=T)
      loc = which(is.na(df_NA[[i]]))
    } else if (wantedVal=='mode') {
      Val = getmode(df_NA[[i]])
      loc = which(is.na(df_NA[[i]]))   # 위치 확인
    }
    
    # 자료형에 따른 값 치환
    if (type=='numeric') {
      df_NA[loc,i ] = as.numeric(Val)
    } else if (type=='integer') {
      df_NA[loc,i ] = as.integer(Val)
    } else if (type=='factor') {
      df_NA[loc,i ] = Val
    }
  }
  return (df_NA)
}

names(tr_a)
library(Amelia)
missmap(tr_a)
str(tr_a)

tr_a$employee1 <- as.integer(tr_a$employee1)
tr_a$employee2 <- as.integer(tr_a$employee2)

# 6:bedcount, 56:employee1,  57:employee2
tr_a <- NAProcess(tr_a, sel=c(6,56,57), 'median')
missmap(tr_a)

colN = c("busan", "choongbuk", "choongnam", "daegu", "daejeon",
         "gangwon", "gwangju", "gyeongbuk", "gyeonggi", "gyeongnam",
         "incheon", "jeonbuk", "jeonnam", "sejong", "seoul",
         "ulsan", "jeju")

tr_a$sido = factor(tr_a$sido, levels = colN)
test$sido = factor(test$sido, levels = colN)

colN = c("", "clinic", "dental_clinic", "general_hospital", "hospital" ,
         "nursing_hospital", "traditional_clinic", "traditional_hospital")

tr_a$instkind = factor(tr_a$instkind, levels = colN)
test$instkind = factor(test$instkind, levels = colN)

levels(tr_a$sido)
levels(test$sido)
levels(tr_a$instkind)
levels(test$instkind)

#  randomForest 모델 만들기 
library(dplyr)
tr_sub <- select(tr_a, -ownerChange  )
missmap(tr_sub)

tr_sub_noNA <- tr_sub[!is.na(tr_sub$salary1), ]
missmap(tr_sub_noNA)

tr_sub_noNA$OC <- ifelse(tr_sub_noNA$OC=='open',1, 0)

tr_sub_noNA$OC <- as.factor(tr_sub_noNA$OC)


table(tr_sub_noNA$OC)
is(tr_sub_noNA$OC)
str(tr_sub)
library(randomForest)
?randomForest
set.seed(30)
rf_model <- randomForest(OC~., data=tr_sub_noNA)
rf_model

