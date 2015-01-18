library(caret)
library(corrplot)
library(kernlab)
library(knitr)
library(randomForest)

if (!file.exists("data")) {
    dir.create("data")
    # file URL and destination file
    fileUrl1 <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
    destfile1 <- "./data/pml-training.csv"
    fileUrl2 <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
    destfile2 <- "./data/pml-testing.csv"
    # download the file and note the time
    download.file(fileUrl1, destfile = destfile1)
    download.file(fileUrl2, destfile = destfile2)
    dateDownloaded <- date()
}
data_training <- read.csv("./data/pml-training.csv")
data_testing <- read.csv("./data/pml-testing.csv")

#By briefly looking at the data, the variables related to min, max, var, kurtosis,
#skewness,avg, stddev, amplitude are not belonged to raw data. We can remove them
#The first 7 columns are related to test-time, subject names and windows used,
#Thus, they can be removed as well.

#Test set
index <- grep('max|min|var|kurtosis|skewness|avg|stddev|amplitude',
     names(data_testing))
testing <- as.data.frame(data_testing[,-c(1:7,index)])
#Training set
index <- grep('max|min|var|kurtosis|skewness|avg|stddev|amplitude',
              names(data_training))
training <- data_training[,-c(1:7,index)]
# plot a correlation matrix
correlMatrix <- cor(training[, -length(training)])
corrplot(correlMatrix, order = "FPC", method = "circle", type = "lower", tl.cex = 0.8,  tl.col = rgb(0, 0, 0))
#Subset the training again for cross validation
inTrain <- createDataPartition(y = training$classe, p = 0.7, list = FALSE)
sub_train <- training[inTrain, ]
crossval <- training[-inTrain, ]
#This is a classification problem, regression model is not applied here
set.seed(12345)
rf_model <- train(sub_train$classe ~ ., data = sub_train,method = "rf",
                  trControl = trainControl(number = 4))
gbm_model <- train(classe ~ ., data = sub_train, method = "gbm",verbose = FALSE)
lda_model <- train(classe ~ ., data = sub_train, method = "lda")
rf_predict <- predict(rf_model, crossval)
gbm_predict <- predict(gbm_model, crossval)
lda_predict <- predict(lda_model, crossval)
c1 <- confusionMatrix(crossval$classe, rf_predict )
c2 <- confusionMatrix(crossval$classe, gbm_predict)
c3 <-confusionMatrix(crossval$classe, lda_predict)
DF_combined <- data.frame(rf_predict, gbm_predict, lda_predict,
                          classe = crossval$classe)
com_model <- train(classe ~ ., data = DF_combined ,method = "rf",
                  trControl = trainControl(number = 4))
com_predict <- predict(lda_model, crossval)
c4 <-confusionMatrix(crossval$classe, com_predict)
acc <- c(c1$overall[[1]],c2$overall[[1]],c3$overall[[1]],c4$overall[[1]])
name <- c("rf","gbm","lda","combined")
names(acc) <- name
print(acc)
com_predict
predictTest <- predict(rf_model, testing)
names(predictTest) <- testing$problem_id
predictTest
