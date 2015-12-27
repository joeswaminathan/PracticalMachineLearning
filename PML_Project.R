load <- function() {
    fileurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
    download.file(fileurl, destfile = "pml-training.csv")
    fileurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
    download.file(fileurl, destfile = "pml-testing.csv")
}

#load()

library(ElemStatLearn)
library(caret)
library(rpart)
library(randomForest)
set.seed(12345)

trainset <- read.csv("pml-training.csv", header=TRUE, sep=",", na.strings=c("NA",""))
testset <- read.csv("pml-testing.csv", header=TRUE, sep=",", na.strings=c("NA",""))

# Remove the ID column
trainData <- subset(trainset, TRUE, select = -X) 
testData <- subset(testset, TRUE, select = -X)  

# Partition the data into training and validation
subTrainPart = createDataPartition(y=trainData$classe, p=0.6, list=FALSE)
training = trainData[subTrainPart,]
validation = trainData[-subTrainPart,]

# Select columns that has NAs less than 40%
retain <- colSums(!is.na(training[,-ncol(training)]))/nrow(training) >= 0.6
training <- training[, retain] 
testData <- testData[, retain]

# Random Forest Model
model <- randomForest(classe~.,data=training) 
plot(model, log ="y", lwd = 2, main = "Random forest accuracy")
#print(model)
#importance(model)

# Confusion Matrix
confusionMatrix(predict(model,newdata=subset(validation, TRUE, select = -classe)),validation$classe)

# Coerce testing dataset to same class and strucuture of training dataset 
testData <- rbind(training[100, -ncol(training)] , testData[, -ncol(testData)]) 
row.names(testData) <- c(100, 1:20)

# Predict
predictions <- predict(model,newdata=testData[-1,])
print(predictions)

# Write predictions to file
write_files <- function(x) {
    n <- length(x)
    for (i in 1:n) {
        filename <- paste0("problem_id", i, ".txt")
        write.table(x[i], file=filename, quote=FALSE, row.names=FALSE,col.names=FALSE)
    }
}

write_files(predictions)