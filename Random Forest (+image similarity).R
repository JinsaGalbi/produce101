## RFE(Recursive Feature Elimination)
setwd('c:/Users/USER/Desktop')
library(dplyr)
library(magrittr)
library(caret)
library(randomForest)
dat = read.csv("pd22.csv")
colnames(dat)[1] <- "age"


## make train, test set 
set.seed(12345)
df<- dat
index <- createDataPartition(df$final_rank, p=0.7, list = F)
train <- df[index,]
test <- df[-index,]
test_x = test[,-15]
test_y = test[,15]

Folds = createFolds(train$final_rank,k=5,returnTrain = TRUE)
Ftest1 = setdiff(1:nrow(train),Folds$Fold1)
Ftest2 = setdiff(1:nrow(train),Folds$Fold2)
Ftest3 = setdiff(1:nrow(train),Folds$Fold3)
Ftest4 = setdiff(1:nrow(train),Folds$Fold4)
Ftest5 = setdiff(1:nrow(train),Folds$Fold5)

FOLDS_TEST = list()
FOLDS_TEST$Fold1 = Ftest1
FOLDS_TEST$Fold2 = Ftest2
FOLDS_TEST$Fold3 = Ftest3
FOLDS_TEST$Fold4 = Ftest4
FOLDS_TEST$Fold5 = Ftest5
str(FOLDS_TEST)
str(Folds)


## modeling
set.seed(12345)
#make seeds
seeds<- vector(mode='list',length=6)
for(i in 1:6) seeds[[i]]<- sample.int(n=1000,14)
control <- trainControl(method = "repeatedcv",index = Folds,
                        savePredictions = 'final',seeds = seeds)
tunegrid <- expand.grid(mtry = c(1:14))
rf_cv <- train(final_rank~., data = train, method = "rf",
               metric = "Accuracy",ntree=500,
               trControl=control,tuneGrid=tunegrid,importance=TRUE)

final_model <- rf_cv$finalModel
rf_predict <- predict(final_model, test_x, type = 'class')
confusionMatrix(rf_predict,test_y)
data.frame(rf_predict,test_y)
summary(final_model)

## importance plot
importance(final_model)
varImpPlot(final_model,sort=TRUE)


## predict
pd4 <- read.csv("season4.csv")
final_predict <- predict(final_model, pd4, type = 'class')
final_predict
table(final_predict)
final_predict[final_predict=="A"]



## Logistic
library(nnet)
library(car)
library(lmtest)
library(pscl)
logistic <- multinom(final_rank~., data= train)
lrtest(logistic)
vif(logistic)
summary(logistic)
pR2(logistic)

control <- trainControl(method = "repeatedcv",index = Folds,
                        savePredictions = 'final',seeds = seeds)
tunegrid <- expand.grid()
logit_cv <- train(final_rank~., data = train, method = "vglmAdjCat",
                  metric = "Accuracy",ntree=500,
                  trControl=control,tuneGrid=tunegrid)