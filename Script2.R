# Load in packages
library('caret')

# read the data
data <- read.csv("./data/mushrooms.csv")
summary(data)
anyNA(data) # FALSE
data$veil.type<-NULL # Because there is only 1 level in veil.type

# split data into train and test subsets
set.seed(87)
data$rgroup <- runif(dim(data)[[1]])
Train <- subset(data,rgroup<=0.7)
Test <- subset(data,rgroup>0.7)

# build model
ctrl<-trainControl(method="cv", number=5)
SVM <- train(Formula, data=Train, method = "svmLinear", trControl = ctrl)
plot(varImp(SVM), main="SVM Variable Importance")
pred <- predict(SVM, Test)

# confusion matrix
cm<-data.frame(Actual=Test$class,Predict=pred)
confusionMatrix(table(cm$Actual,cm$Predict))

