training <- read.csv("pml-training.csv")
testing <- read.csv("pml-testing.csv")

library(dplyr)

#Preproccesament
#NA columm
trainNA <- apply(training, 2, function(x) {
  sum(is.na(x))
})
training <- training[,trainNA == 0]

#blank columm
trainBlank <- apply(training, 2, function(x) {
  sum(x%in%"")
})
training <- training[,trainBlank == 0]

#check class
training_class <- sapply(training, class)

#factor class columm
colnames(training[,training_class=="factor"])

#X
#user_name
#cvtd_timestamp
#new_window

training <- select(training, -c(X,user_name, cvtd_timestamp, new_window))

head(training)
str(training)

#test samples
testNA <- apply(testing, 2, function(x) {
  sum(is.na(x))
})
testing <- testing[,testNA==0]

testing <- select(testing, -c(X,user_name, cvtd_timestamp, new_window))


#Machine Learning
library(caret)

#mesure testing error SMSE

#LDA
FitLDA <- train(classe ~., method = "lda", data = training,
                trControl = trainControl(method = "cv"))
FitLDA
predLDA <- predict(FitLDA, training)
table(predLDA, training$classe)

predLDAtest <- predict(FitLDA, testing)
predLDAtest

#Random forest
library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

fitControl <- trainControl(method = "cv",
                           number = 5,
                           allowParallel = TRUE)

FitLRF <- train(classe ~., method = "rf", data = training,
                trControl = fitControl)

predRF <- predict(FitLRF, training)
confusionMatrix(predRF, training$classe)

predRFtest <- predict(FitLRF, testing)
predRFtest
## [1] B A B A A E D B A A B C B A E E A B B B
