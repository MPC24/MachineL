#download and import training and test data
tracsv <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(tracsv, destfile=paste(getwd(), "pml-training.csv", sep="/"))
train <- read.csv(paste(getwd(), "pml-training.csv", sep="/"), header=TRUE)
tescsv <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(tescsv, destfile=paste(getwd(), "pml-testing.csv", sep="/"))
test <- read.csv(paste(getwd(), "pml-testing.csv", sep="/"), header=TRUE)

library(caret)
set.seed(33141)
#Create partition and remove NA columns
inTrain <- createDataPartition(y=train$classe, p=0.60, list=FALSE)
training <- train[inTrain,]
testing <- train[-inTrain,]
tr <- training[,sapply(training, function(x) sum(is.na(x))) == 0]



#Feature selection and preprocessing
ntr <- tr[,sapply(tr,class)=="numeric"]

#Exclude un-predictive variables
ntrCor <- cor(ntr)
highCor <- findCorrelation(ntrCor, cutoff = 0.70)
ntr <- ntr[, -highCor]
nzv <- nearZeroVar(ntr)
ntr <- ntr[,-nzv]
ntrfinal <- ntr[, sapply(ntr, sd) > 1]

#Append outcome and dummy variables to final and dummy variables to test
dumtrain <- dummyVars(~ user_name, data=training)
userdum <- predict(dumtrain, training)
ntrfinal <- cbind(ntrfinal, userdum)
ntrfinal$classe <- training$classe
usertedum <- dummyVars(~user_name, data=testing)
ustesting <- predict(usertedum, testing)
testing <- cbind(testing, ustesting)
usertest <- dummyVars(~ user_name, data=test)
userte <- predict(usertest, newdata=test)
test <- cbind(test, userte)


#Model Selection
feModrf <- train(ntrfinal$classe ~ ., data=ntrfinal, method="rf", trControl=tc)
feModrf
fintesting <- predict(feModrf, testing)
confusionMatrix(testing$classe, fintesting)



