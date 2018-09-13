install.packages("corrplot")
library(corrplot)

##데이터 불러오기
dftr <- read.csv("train.csv")
dftest <- read.csv("test.csv")

##탐색
str(dftr)


##데이터 전처리
trA <- dftr[,-c(10,11)]
testA <- dftest
trA$season <- as.factor(trA$season)
trA$holiday <- as.factor(trA$holiday)
trA$workingday <- as.factor(trA$workingday)
trA$weather <- as.factor(trA$weather)
trA$datetime <- as.POSIXct(trA$datetime, format="%Y-%m-%d %H:%M:%S")

testA$season <- as.factor(testA$season)
testA$holiday <- as.factor(testA$holiday)
testA$workingday <- as.factor(testA$workingday)
testA$weather <- as.factor(testA$weather)
testA$datetime <- as.POSIXct(testA$datetime, format="%Y-%m-%d %H:%M:%S")

####파생변수 생성
trA$day = strftime(trA$datetime, '%d')
class(trA$day)
trA$hour = substring(trA$datetime,12,13)
trA$hour = as.factor(substring(trA$datetime,12,13))
trA$weekday <-strftime(trA$datetime, '%u') 
str(trA)

####test데이터

testA$day = strftime(testA$datetime, '%d')
testA$hour = substring(testA$datetime,12,13)
testA$hour = as.factor(substring(testA$datetime,12,13))
testA$weekday <-as.factor(weekdays(as.Date(testA$datetime))) 

trA <- trA[,-c(1)]
testA<- testA[,-c(1)]
names(trA)
#######Correlation(상관) plot between fileds
sel <- c("temp","atemp","humidity","windspeed", "count" )
trA_subset <- trA[,sel]

##상관계수 만들기
tr_cor=cor(trA_subset)
install.packages("corrplot")
library(corrplot)
corrplot(tr_cor)
corrplot(cor(tr_cor), method="color", addCoef.col="black")
