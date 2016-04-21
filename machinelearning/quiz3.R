library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
#install.packages("rpart")
library(rpart)
#install.packages("rpart.plot")

# 1. Subset the data to a training set and testing set based on the Case variable in the data set. 
inTrain <- createDataPartition(y = segmentationOriginal$Case, p = 0.6, list = FALSE) # 60% training
training <- segmentationOriginal[inTrain, ]
testing <- segmentationOriginal[-inTrain, ]
# 2. Set the seed to 125 and fit a CART model with the rpart method using all predictor variables and default caret settings. (The outcome class is contained in a factor variable called Class with levels "PS" for poorly segmented and "WS" for well segmented.)
set.seed(125)
modFit <- train(Class ~ ., method = "rpart", data = training)
modFit$finalModel

#install.packages("rattle")
suppressMessages(library(rattle))
library(rpart.plot)
fancyRpartPlot(modFit$finalModel)

# question 3
install.packages("pgmm")
library(pgmm)
data(olive)
olive = olive[,-1]

modolive <- train(Area ~ ., method = "rpart", data = olive)
newdata = as.data.frame(t(colMeans(olive)))
predict(modolive, newdata = newdata)

# question 4
install.packages("ElemStatLearn")
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

set.seed(13234)
modelSA <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, 
                 data = trainSA, method = "glm", family = "binomial")
missClass(testSA$chd, predict(modelSA, newdata = testSA))
missClass(trainSA$chd, predict(modelSA, newdata = trainSA))

#question 5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
install.packages("randomForest")
library(randomForest)

modvowel <- randomForest(y ~ ., data = vowel.train)
order(varImp(modvowel), decreasing = T)
