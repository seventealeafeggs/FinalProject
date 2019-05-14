library(sf)
library(data.table)
library(tidyverse)
library(data.table)
library(stringr)
library(readr)
library(foreign)
library(readxl)
library(haven)
library(ROCR)
library(C50)
library(randomForest)
library(rpart)
library(rpart.plot)
library(rattle)
library(caret)
library(forecast)
library(tidyverse)
#### 加速
library('compiler')
enableJIT(3)
Sys.setlocale(category="LC_ALL",locale="zh_TW.UTF-8")

dtpT <- dtp[,c(7:10)]
dtpT <- na.omit(dtpT)

load("STAT_TP_INCOME_V2.RData")
stp <- STAT_TP_INCOME
stp %>% str()
 
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 化妝品業家數 ~ ., data = stp[,c(8:13,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)

stpt<-stp
stpt$result <- result
result - stp$化妝品業家數

summary(stpt$tmp)
hist(stpt$tmp,breaks = 100)

plot(stpt$化妝品業家數,stpt$result)

# 化妝品業家數

#ml1
pmatrix <- scale(dtp[,c(8:10,11,19)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 化妝品業家數 ~ ., data = stp[,c(8:10,11,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$化妝m1 <- result - stp$化妝品業家數
#ml2
pmatrix <- scale(dtp[,c(8:10,11,12,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 化妝品業家數 ~ ., data = stp[,c(8:10,11,12,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$化妝m2 <- result - stp$化妝品業家數
###ml1 P_CNT M_F_RAT P_H_CNT    P_DEN A65_A0A14_RAT MARRIAGED_RATE COLLEGE_RATE

#ml3
pmatrix <- scale(dtp[,c(8:10,11,12,13,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 化妝品業家數 ~ ., data = stp[,c(8:10,11,12,13,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$化妝m3 <- result - stp$化妝品業家數
#ml4
pmatrix <- scale(dtp[,c(8:10,11,13,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 化妝品業家數 ~ ., data = stp[,c(8:10,11,13,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$化妝m4 <- result - stp$化妝品業家數
#ml5
pmatrix <- scale(dtp[,c(8:10,14,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 化妝品業家數 ~ ., data = stp[,c(8:10,14,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$化妝m5 <- result - stp$化妝品業家數
#ml6
pmatrix <- scale(dtp[,c(8:10,14,12,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 化妝品業家數 ~ ., data = stp[,c(8:10,14,12,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$化妝m6 <- result - stp$化妝品業家數
#ml7
pmatrix <- scale(dtp[,c(8:10,14,12,13,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 化妝品業家數 ~ ., data = stp[,c(8:10,14,12,13,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$化妝m7 <- result - stp$化妝品業家數
#ml8
pmatrix <- scale(dtp[,c(8:10,14,13,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 化妝品業家數 ~ ., data = stp[,c(8:10,14,13,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$化妝m8 <- result - stp$化妝品業家數
###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A20_A39_COUNT_RATE 
#m9
pmatrix <- scale(dtp[,c(8:10,15,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 化妝品業家數 ~ ., data = stp[,c(8:10,15,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$化妝m9 <- result - stp$化妝品業家數

#m10
pmatrix <- scale(dtp[,c(8:10,15,12,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 化妝品業家數 ~ ., data = stp[,c(8:10,14,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$化妝m10 <- result - stp$化妝品業家數
###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A20_A39_COUNT_RATET MARRIAGED_RATE COLLEGE_RATE
#11
pmatrix <- scale(dtp[,c(8:10,15,12,13,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 化妝品業家數 ~ ., data = stp[,c(8:10,15,12,13,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$化妝m11 <- result - stp$化妝品業家數

###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A20_A39_COUNT_RATE MARRIAGED_RATE
#m12
pmatrix <- scale(dtp[,c(8:10,15,13,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 化妝品業家數 ~ ., data = stp[,c(8:10,15,13,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$化妝m12 <- result - stp$化妝品業家數

###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A40_A59_COUNT_RATE 
#m13
pmatrix <- scale(dtp[,c(8:10,16,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 化妝品業家數 ~ ., data = stp[,c(8:10,16,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$化妝m13 <- result - stp$化妝品業家數
###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A40_A59_COUNT_RATE COLLEGE_RATE
#m14
pmatrix <- scale(dtp[,c(8:10,16,12,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 化妝品業家數 ~ ., data = stp[,c(8:10,16,12,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$化妝m14 <- result - stp$化妝品業家數

###ml1 P_CNT M_F_RAT P_H_CNT  P_DEN  A40_A59_COUNT_RATE MARRIAGED_RATE COLLEGE_RATE
#m15
pmatrix <- scale(dtp[,c(8:10,16,12,13,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 化妝品業家數 ~ ., data = stp[,c(8:10,16,12,13,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$化妝m15 <- result - stp$化妝品業家數

###ml1 P_CNT M_F_RAT P_H_CNT  P_DEN  A40_A59_COUNT_RATE MARRIAGED_RATE
#m16
pmatrix <- scale(dtp[,c(8:10,16,13,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 化妝品業家數 ~ ., data = stp[,c(8:10,16,13,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$化妝m16 <- result - stp$化妝品業家數

###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A60_A79_COUNT_RATE 
#m17
pmatrix <- scale(dtp[,c(8:10,17,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 化妝品業家數 ~ ., data = stp[,c(8:10,17,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$化妝m17 <- result - stp$化妝品業家數

###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A60_A79_COUNT_RATE COLLEGE_RATE
#m18
pmatrix <- scale(dtp[,c(8:10,17,12,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 化妝品業家數 ~ ., data = stp[,c(8:10,17,12,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$化妝m18 <- result - stp$化妝品業家數


###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A60_A79_COUNT_RATE MARRIAGED_RATE COLLEGE_RATE
#m19
pmatrix <- scale(dtp[,c(8:10,17,12,13,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 化妝品業家數 ~ ., data = stp[,c(8:10,17,12,13,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$化妝m19 <- result - stp$化妝品業家數

###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A60_A79_COUNT_RATE MARRIAGED_RATE
#m20
pmatrix <- scale(dtp[,c(8:10,17,13,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 化妝品業家數 ~ ., data = stp[,c(8:10,17,13,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$化妝m20 <- result - stp$化妝品業家數

###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A80up_COUNT_RATE 
#m21
pmatrix <- scale(dtp[,c(8:10,18,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 化妝品業家數 ~ ., data = stp[,c(8:10,18,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$化妝m21 <- result - stp$化妝品業家數

###ml1 P_CNT M_F_RAT P_H_CNT  P_DEN  A80up_COUNT_RATE COLLEGE_RATE
#m22
pmatrix <- scale(dtp[,c(8:10,18,12,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 化妝品業家數 ~ ., data = stp[,c(8:10,18,12,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$化妝m22 <- result - stp$化妝品業家數

###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A80up_COUNT_RATE MARRIAGED_RATE COLLEGE_RATE
#m23
pmatrix <- scale(dtp[,c(8:10,18,12,13,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 化妝品業家數 ~ ., data = stp[,c(8:10,18,12,13,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$化妝m23 <- result - stp$化妝品業家數

###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A80up_COUNT_RATE MARRIAGED_RATE
#m24
pmatrix <- scale(dtp[,c(8:10,18,13,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 化妝品業家數 ~ ., data = stp[,c(8:10,18,13,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$化妝m24 <- result - stp$化妝品業家數

# 日常
#ml1
pmatrix <- scale(dtp[,c(8:10,11,19)])

#跑隨機樹森林模型
randomforestM <- randomForest(formula = 日常用品業家數 ~ ., data = stp[,c(8:10,11,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$日常用品業家數m1 <- result - stp$日常用品業家數
#ml2
pmatrix <- scale(dtp[,c(8:10,11,12,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 日常用品業家數 ~ ., data = stp[,c(8:10,11,12,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$日常用品業家數m2 <- result - stp$日常用品業家數
###ml1 P_CNT M_F_RAT P_H_CNT    P_DEN A65_A0A14_RAT MARRIAGED_RATE COLLEGE_RATE

#ml3
pmatrix <- scale(dtp[,c(8:10,11,12,13,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 日常用品業家數 ~ ., data = stp[,c(8:10,11,12,13,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$日常用品業家數m3 <- result - stp$日常用品業家數
#ml4
pmatrix <- scale(dtp[,c(8:10,11,13,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 日常用品業家數 ~ ., data = stp[,c(8:10,11,13,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$日常用品業家數m4 <- result - stp$日常用品業家數
#ml5
pmatrix <- scale(dtp[,c(8:10,14,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 日常用品業家數 ~ ., data = stp[,c(8:10,14,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$日常用品業家數m5 <- result - stp$日常用品業家數
#ml6
pmatrix <- scale(dtp[,c(8:10,14,12,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 日常用品業家數 ~ ., data = stp[,c(8:10,14,12,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$日常用品業家數m6 <- result - stp$日常用品業家數
#ml7
pmatrix <- scale(dtp[,c(8:10,14,12,13,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 日常用品業家數 ~ ., data = stp[,c(8:10,14,12,13,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$日常用品業家數m7 <- result - stp$日常用品業家數
#ml8
pmatrix <- scale(dtp[,c(8:10,14,13,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 日常用品業家數 ~ ., data = stp[,c(8:10,14,13,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$日常用品業家數m8 <- result - stp$日常用品業家數
###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A20_A39_COUNT_RATE 
#m9
pmatrix <- scale(dtp[,c(8:10,15,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 日常用品業家數 ~ ., data = stp[,c(8:10,15,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$日常用品業家數m9 <- result - stp$日常用品業家數

#m10
pmatrix <- scale(dtp[,c(8:10,15,12,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 日常用品業家數 ~ ., data = stp[,c(8:10,14,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$日常用品業家數m10 <- result - stp$日常用品業家數
###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A20_A39_COUNT_RATET MARRIAGED_RATE COLLEGE_RATE
#11
pmatrix <- scale(dtp[,c(8:10,15,12,13,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 日常用品業家數 ~ ., data = stp[,c(8:10,15,12,13,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$日常用品業家數m11 <- result - stp$日常用品業家數

###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A20_A39_COUNT_RATE MARRIAGED_RATE
#m12
pmatrix <- scale(dtp[,c(8:10,15,13,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 日常用品業家數 ~ ., data = stp[,c(8:10,15,13,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$日常用品業家數m12 <- result - stp$日常用品業家數

###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A40_A59_COUNT_RATE 
#m13
pmatrix <- scale(dtp[,c(8:10,16,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 日常用品業家數 ~ ., data = stp[,c(8:10,16,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$日常用品業家數m13 <- result - stp$日常用品業家數
###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A40_A59_COUNT_RATE COLLEGE_RATE
#m14
pmatrix <- scale(dtp[,c(8:10,16,12,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 日常用品業家數 ~ ., data = stp[,c(8:10,16,12,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$日常用品業家數m14 <- result - stp$日常用品業家數

###ml1 P_CNT M_F_RAT P_H_CNT  P_DEN  A40_A59_COUNT_RATE MARRIAGED_RATE COLLEGE_RATE
#m15
pmatrix <- scale(dtp[,c(8:10,16,12,13,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 日常用品業家數 ~ ., data = stp[,c(8:10,16,12,13,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$日常用品業家數m15 <- result - stp$日常用品業家數

###ml1 P_CNT M_F_RAT P_H_CNT  P_DEN  A40_A59_COUNT_RATE MARRIAGED_RATE
#m16
pmatrix <- scale(dtp[,c(8:10,16,13,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 日常用品業家數 ~ ., data = stp[,c(8:10,16,13,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$日常用品業家數m16 <- result - stp$日常用品業家數

###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A60_A79_COUNT_RATE 
#m17
pmatrix <- scale(dtp[,c(8:10,17,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 日常用品業家數 ~ ., data = stp[,c(8:10,17,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$日常用品業家數m17 <- result - stp$日常用品業家數

###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A60_A79_COUNT_RATE COLLEGE_RATE
#m18
pmatrix <- scale(dtp[,c(8:10,17,12,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 日常用品業家數 ~ ., data = stp[,c(8:10,17,12,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$日常用品業家數m18 <- result - stp$日常用品業家數


###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A60_A79_COUNT_RATE MARRIAGED_RATE COLLEGE_RATE
#m19
pmatrix <- scale(dtp[,c(8:10,17,12,13,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 日常用品業家數 ~ ., data = stp[,c(8:10,17,12,13,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$日常用品業家數m19 <- result - stp$日常用品業家數

###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A60_A79_COUNT_RATE MARRIAGED_RATE
#m20
pmatrix <- scale(dtp[,c(8:10,17,13,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 化妝品業家數 ~ ., data = stp[,c(8:10,17,13,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$化妝m20 <- result - stp$化妝品業家數

###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A80up_COUNT_RATE 
#m21
pmatrix <- scale(dtp[,c(8:10,18,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 化妝品業家數 ~ ., data = stp[,c(8:10,18,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$化妝m21 <- result - stp$化妝品業家數

###ml1 P_CNT M_F_RAT P_H_CNT  P_DEN  A80up_COUNT_RATE COLLEGE_RATE
#m22
pmatrix <- scale(dtp[,c(8:10,18,12,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 化妝品業家數 ~ ., data = stp[,c(8:10,18,12,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$化妝m22 <- result - stp$化妝品業家數

###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A80up_COUNT_RATE MARRIAGED_RATE COLLEGE_RATE
#m23
pmatrix <- scale(dtp[,c(8:10,18,12,13,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 化妝品業家數 ~ ., data = stp[,c(8:10,18,12,13,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$化妝m23 <- result - stp$化妝品業家數

###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A80up_COUNT_RATE MARRIAGED_RATE
#m24
pmatrix <- scale(dtp[,c(8:10,18,13,19,20)])
#跑隨機樹森林模型
randomforestM <- randomForest(formula = 化妝品業家數 ~ ., data = stp[,c(8:10,18,13,19,20)], importane = T, proximity = T,do.trace = 100, ntree=200)
#預測
result <- predict(randomforestM, newdata = stp)
stp$化妝m24 <- result - stp$化妝品業家數



