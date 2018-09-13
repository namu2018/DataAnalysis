str(trA)
Lm <- lm(count~., data=trA)
step(Lm, direction = "both")
Lm <- lm(count ~ season + weather + temp + atemp + humidity + 
           windspeed + hour + weekday, data=trA)
preval <- predict(Lm, newdata=testA)

reddf <- resdf
preval[preval<=0]<-0
resdf$count <- preval



#####랜덤포레스트

install.packages("randomForest")
library(randomForest)

## 1. 랜덤 포레스트를 사용한 모델링
result = randomForest(count~season+temp+humidity+windspeed+day+weekday+busy, data=trA_h, importance=T)
summary(result)
result
preval_R <- predict(result, th)
preval_R
preval_R[preval<=0] <- 0
reddf$count <- preval_R
write.csv(reddf, file="result_R.csv", row.names=FALSE)
getwd()

trA_h <- trA
th<- testA
lm_h <- lm(count~hour, data=trA)
summary(lm_h)
trA_h$hour = as.integer(trA_h$hour)
th$hour <- as.integer(th$hour)
lm_h
lm_h$coefficients
names(lm_h)
plot(lm_h$coefficients)
preval[preval<=0]<-0
names(trA)
trA_h$busy[trA_h$busy>=7]  <- 0
str(trA)
trA_h$busy[trA_h$busy==8]
str(trA_h)
trA_h[trA_h$busy]<- trA_h$hour
trA_h<-trA_h[,-c(13)]
trA_h$busy <- trA_h$hour
th$busy<- th$hour
trA_h[trA_h$busy >=22 , "busy"]<-5
trA_h$busy<8
th[th$busy < 8 , "busy"]<-1
th[th$busy >=8 & th$busy <11, "busy"]<-2
th[th$busy >=11 & th$busy <17, "busy"]<-3
th[th$busy >=17 & th$busy <22, "busy"]<-4
th[th$busy >=22 , "busy"]<-5


str(trA_h)
th$busy <- as.factor(th$busy)
th<- th[,-c(11)]
trA_h
str(trA_h)