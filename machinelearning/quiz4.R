#Question 1

library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
mod_rf <- train(y ~ ., data = vowel.train, method = "rf")
mod_gbm <- train(y ~ ., data = vowel.train, method = "gbm")
pred_rf <- predict(mod_rf, vowel.test)
pred_gbm <- predict(mod_gbm, vowel.test)
confusionMatrix(pred_rf, vowel.test$y)$overall[1]
confusionMatrix(pred_gbm, vowel.test$y)$overall[1]

predDF <- data.frame(pred_rf, pred_gbm, y = vowel.test$y)
sum(pred_rf[predDF$pred_rf == predDF$pred_gbm] == 
        predDF$y[predDF$pred_rf == predDF$pred_gbm]) / 
    sum(predDF$pred_rf == predDF$pred_gbm)

# Question 2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis, predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[inTrain, ]
testing = adData[-inTrain, ]

set.seed(62433)
mod_rf <- train(diagnosis ~ ., data = training, method = "rf")
mod_gbm <- train(diagnosis ~ ., data = training, method = "gbm")
mod_lda <- train(diagnosis ~ ., data = training, method = "lda")
pred_rf <- predict(mod_rf, testing)
pred_gbm <- predict(mod_gbm, testing)
pred_lda <- predict(mod_lda, testing)
predDF <- data.frame(pred_rf, pred_gbm, pred_lda, diagnosis = testing$diagnosis)
combModFit <- train(diagnosis ~ ., method = "rf", data = predDF)
combPred <- predict(combModFit, predDF)

confusionMatrix(pred_rf, testing$diagnosis)$overall[1]
confusionMatrix(pred_gbm, testing$diagnosis)$overall[1]
confusionMatrix(pred_lda, testing$diagnosis)$overall[1]
confusionMatrix(combPred, testing$diagnosis)$overall[1]

# question 3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[inTrain, ]
testing = concrete[-inTrain, ]

set.seed(233)
mod_lasso <- train(CompressiveStrength ~ ., data = training, method = "lasso")
library(elasticnet)
plot.enet(mod_lasso$finalModel, xvar = "penalty", use.color = TRUE)

# question 4
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv", destfile = "./gaData.csv")
library(lubridate)  # For year() function below
dat = read.csv("./gaData.csv")
training = dat[year(dat$date) < 2012, ]
testing = dat[(year(dat$date)) > 2011, ]
tstrain = ts(training$visitsTumblr)

install.packages("forecast")
library(forecast)
mod_ts <- bats(tstrain)
fcast <- forecast(mod_ts, level = 95, h = dim(testing)[1])
sum(fcast$lower < testing$visitsTumblr & testing$visitsTumblr < fcast$upper) / 
    dim(testing)[1]

# question 5
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[inTrain, ]
testing = concrete[-inTrain, ]

set.seed(325)
library(e1071)
mod_svm <- svm(CompressiveStrength ~ ., data = training)
pred_svm <- predict(mod_svm, testing)
accuracy(pred_svm, testing$CompressiveStrength)
