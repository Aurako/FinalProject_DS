# Load in packages
library('e1071')

# read the data
data <- read.csv("./data/mushrooms.csv")
summary(data)
anyNA(data) # FALSE
data$veil.type<-NULL # Because there is only 1 level in veil.type

# split data into train and test subsets
set.seed(87)
data$rgroup <- runif(dim(data)[[1]])
Train <- subset(data,rgroup<=0.8)
Test <- subset(data,rgroup>0.8)

# model
mushVars <- setdiff(colnames(data), list('class', 'rgroup'))
mushFormula <- as.formula(paste('class', paste(mushVars,collapse=' + '),sep=' ~ '))
svmM <- svm(mushFormula,data=Train)
results <- predict(svmM, Test)
cm <- table(x = Test$class, y = results)
cm
SVMaccuracy <- sum(diag(cm)) / sum(cm)
SVMaccuracy
