library(caret)
library(randomForest)
#load the data
data_train <- read.table(file='pml-training.csv', header=T, sep=',', na.strings = c("NA","#DIV/0!"))

#Find and leave out variables with too many N/A values and variables for case identification
keep <- c()
for (i in 1:160){
  if (length(which(is.na(data_train[,i])))<100){keep <- c(keep,i)}
}

data_clean <- data_train[,keep[8:length(keep)]]

#Split the data into training and testing set
set.seed(123)
index_train <- createDataPartition(y=data_clean$classe,p=0.6,list=F)
train <- data_clean[index_train,]
test <- data_clean[-index_train,]

#Build the model
modelFit <- randomForest(classe~.,data=train)
modelFit$confusion
#Run the model on the testing data
pred <- test$classe==predict(modelFit,test[,-53])
table(pred)
as.numeric(table(pred)[1]/(table(pred)[1]+table(pred)[2]))

#Run the model on the 20 test cases
data_test <- read.table(file='pml-testing.csv', header=T, sep=',', na.strings = c("NA","#DIV/0!"))
datat_clean <- data_test[,keep[8:length(keep)]]
pred_test <- predict(modelFit,datat_clean[,-53])
