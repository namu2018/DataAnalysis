getwd()
setwd("C:/Users/ktm/Documents/choi_dontouch/data/hospital")
tr <- read.csv("train.csv")
test <- read.csv("test.csv")
library(dplyr)
library(corrplot)



### 01. 데이터 불러오기
setwd("E:/BigData3/dataset/mouda/2ndCompetitionData")
test <- read.csv("test.csv")
tr <- read.csv("train.csv")

library(dplyr)     # 데이터 처리 
library(corrplot)  # 시각화 Heatmap
library(Amelia)

### 02. EDA
str(tr); str(test);

### 03. randomForest 모델.
### 모델을 만들고, test를 이용하여 예측 
### (1) 훈련용(tr), 테스트(test) 데이터를 전처리
### (2) 모델을 만든다.
### (3) 모델에 테스트 데이터를 넣어서 예측

### (1) 학습용 데이터를 전처리
### 변수를 선택해 준다.
missmap(tr)

## tr_sub <- employee2, employee1, bedCount, ownerChange 열을 뺀다.
tr_sub <- select(tr, -c( bedCount, ownerChange) )

tr_sub_noNA <- tr_sub[  !is.na(tr_sub$salary1),    ]
missmap(tr_sub_noNA)

library(randomForest)
# rf_model <- randomForest(factor(OC) ~ . , data=tr_sub_noNA)
rf_model <- randomForest(factor(OC) ~ revenue1 + sga1 + salary1 +
                           interest1+profit1,data=tr_sub_noNA)
rf_model

### 변수의 중요도
importance <- importance(rf_model)
importance

sel = c('revenue1', 'sga1', 'salary1', 'interest1', 'profit1')
test_sub <- select(test, -c(employee2, employee1, bedCount, ownerChange) )
str(test_sub[ ,sel ])

test_sub1 <- test_sub[ , sel]  # 데이터 중에 sel의 변수만 선택
missmap(test_sub1)

'''
for (idx  in  범위)  {
실행문1
실행문2
}
'''
'''
if (조건문) {
실행문1
}
'''

lenVar = length(test_sub1)
for (i  in  1:lenVar) {
  type = class(test_sub1[[i]])
  if (type=="numeric") {
    ### (1) 평균값을 구하기 (2) NA의 위치 확인 (3) 값을 치환(평균값) 
    ## (1) 평균값
    meanVal = mean( test_sub1[[i]] , na.rm=T )  
    
    ## (2) NA의 위치 확인
    loc = which(is.na(test_sub1[[i]]))  # NA의 위치 확인
    
    ## (3) 값을 치환 
    test_sub1[loc , i] = meanVal
  }
}
missmap(test_sub1)
prediction <- predict(rf_model, newdata=test_sub1)
sub <- read.csv("submission_sample.csv")
prediction <- ifelse(prediction=='open', 1, 0)
sub$OC <- prediction
table(sub$OC)
write.csv(sub, file="sub_0919.csv", row.names=FALSE)

str(hos)
str(hos_test)
