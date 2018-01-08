# Load in packages
library('e1071')
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

# Create a stratified sample for repeated cv
cv.5.folds<-createMultiFolds(Train,k=5)

# create a control object for repeated cv in caret
ctrl.1<-trainControl(method="repeatedcv",repeats=5,index=cv.5.folds, savePred=T)
SVM <- train(Formula, data=Train, method = "svmLinear", trControl = ctrl.1)

y_predicted<-predict(SVM,Test)
linearCM <- table('Actual' = Test$class, 'Predict' = y_predicted)
linearCM
plot(varImp(SVM),main="SVM - Variable Importance Plot")
Lacc <- sum(diag(linearCM)) / sum(linearCM)
Lacc

df1<-data.frame(Orig=Test$class,Pred=y_predicted)
df1
confusionMatrix(table(df1$Orig,df1$Pred))

# null model
table(data$class)
nullAcc <- table(data$class)[['e']]/length(data$class)
nullAcc
# 
# # model
mushVars <- setdiff(colnames(data), list('class', 'rgroup'))
Formula <- as.formula(paste('class', paste(mushVars,collapse=' + '),sep=' ~ '))
# linearSVM <- svm(mushFormula, data=Train, kernel='linear', cross=5)
# radialSVM <- svm(mushFormula, data=Train, kernel='radial', cross=5)
# linearResults <- predict(linearSVM, Test)
# radialResults <- predict(radialSVM, Test)
# linearCM <- table('Actual' = Test$class, 'Predict' = linearResults)
# radialCM <- table('Actual' = Test$class, 'Predict' = radialResults)
# 
# # summary of models
# summary(linearSVM)
# summary(radialSVM)
# 
# # performance of models
# linearCM
# Lacc <- sum(diag(linearCM)) / sum(linearCM)
# Lprec <- linearCM[1,1] / sum(linearCM[,1])
# Lsens <- linearCM[1,1] / sum(linearCM[1,])
# Lspec <- linearCM[2,2] / sum(linearCM[2,])
# radialCM
# Racc <- sum(diag(radialCM)) / sum(radialCM)
# Rprec <- radialCM[1,1] / sum(radialCM[,1])
# Rsens <- radialCM[1,1] / sum(radialCM[1,])
# Rspec <- radialCM[2,2] / sum(radialCM[2,])
# Lpfm <- c(Lacc, Lprec, Lsens, Lspec)
# Rpfm <- c(Racc, Rprec, Rsens, Rspec)
# rowname <- c("Accuracy", "precision", "Sensitivity", "Specificity")
# performanceTable <- data.frame("Linear SVM"=Lpfm, "Radial SVM"=round(Rpfm, digits=3), row.names = rowname)
# print(performanceTable)
# plot(linearSVM, Train)
