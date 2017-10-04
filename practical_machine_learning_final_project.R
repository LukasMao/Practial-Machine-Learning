## Practical Machine Learning Project

## import data
pml_training <- read.csv("C:/Users/lukas/Downloads/pml-training.csv",header = T)
pml_testing <- read.csv("C:/Users/lukas/Downloads/pml-testing.csv",header = T)
#head(pml_training)

pml_training <- read.csv("C:/Users/lukas/Downloads/pml-training.csv",header = T,na.strings=c("NA",""))
pml_testing <- read.csv("C:/Users/lukas/Downloads/pml-testing.csv",header = T,na.strings=c("NA",""))
#head(pml_training)

pml_training <- pml_training[,colSums(is.na(pml_training)) == 0]
pml_testing <- pml_testing[,colSums(is.na(pml_testing)) == 0]
#head(pml_training)

## In this project, your goal will be to use data from accelerometers 
## on the belt, forearm, arm, and dumbell of 6 participants. Thus we delete the 
## unrelated variables.
pml_training <- pml_training[,-c(1:7)]
pml_testing <- pml_testing[,-c(1:7)]

## to do cross-validation
library(caret)

set.seed(1001)
inTrain <- createDataPartition(y=pml_training$classe, p=3/4, list=FALSE)
pml_training_train <- pml_training[inTrain,] 
pml_training_test <- pml_training[-inTrain,]

histogram(pml_training_train$classe,pml_training_train,xlab = "Classes",
                ylab="Percentage",main="Frequency of each type in Training data")
histogram(pml_training_test$classe,pml_training_test,xlab = "Classes",
                ylab="Percentage",main="Frequency of each type in Testing data")

## rpart: Regressive Partitioning and Regression trees
library(rpart)
#install.packages("RGtk2") if necessary
#library(RGtk2)
#library(rattle)
modFit <- rpart(classe ~ .,data=pml_training_train,method="class")
#fancyRpartPlot(modFit)
train_pred <- predict(modFit,pml_training_test,type = "class")
confusionMatrix(train_pred,pml_training_test$classe)

## Now we try random forest.
library(randomForest)
modFit <- randomForest(classe ~., data=pml_training_train, method="class")
train_pred <- predict(modFit,newdata = pml_training_test,type = "class")
confusionMatrix(train_pred,pml_training_test$classe)

## Finally, we predict the 20 predicting tests.
test_pred <- predict(modFit,newdata = pml_testing)
test_pred