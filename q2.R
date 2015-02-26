library(e1071)
library(klaR)
library(rpart)
library(ROCR)

source('calROC.R')
source('calPred.R')
source('convertLabels.R')
source('plotROC.R')
source('joinFolds.R')
source('folds.R')

prefix = './q2_results/' # the directory storing results
type = 'N' # 'N' is set to TRUE and 'D' is set to FALSE

# load data 
fileName = 'Kato_P53_mutants_200.txt'
data = read.table(fileName, header = TRUE)
testidx <- which(1 : length(iris[, 1]) %% 5 == 0)           
dataTrain <- data[-testidx, ]
dataTest <- data[testidx, ]

#-------------------- Naive Bayes --------------------

# calculate prediction
nbModel <- NaiveBayes(CLASS ~ ., data = dataTrain);      
nbPred <- predict(nbModel, dataTest[, -2]) 
table(nbPred$class, dataTest$CLASS)

#-------------------- Decision Tree --------------------

# calculate prediction
dtModel <- rpart(CLASS ~ ., data = dataTrain)
dtPred <- predict(dtModel, newdata = dataTest, type = 'class')
table(dtPred, dataTest$CLASS)

#-------------------- SVM --------------------

# calculate prediction
svmModel <- svm(CLASS ~ ., data = dataTrain, probability = TRUE)
svmPred <- predict(svmModel, dataTest[, -2], probability = TRUE)
table(svmPred, dataTest$CLASS)

# plot ROC
labels = convertLabels(dataTest$CLASS, type)
scores = convertLabels(svmPred, type)
svmROC = calROC(scores, labels)
plotROC(svmROC[[1]], svmROC[[2]], 'SVM_ROC', prefix)

# 10 cross validation

foldsNum = 10

dataFolds = folds(data, foldsNum)
pred = vector(mode="list", length=3)
names(pred) = c('nb', 'dt', 'svm')
for (i in 1 : 3){
    pred[[i]] = vector(mode = 'list', len = foldsNum)
}
labels = vector(mode = 'list', len = foldsNum)

for (i in 1 : foldsNum){
    # get training data and testing data from the folds
    dataTrain = joinFolds(dataFolds[-i])
    dataTest = dataFolds[[i]]
    # Naive Bayes
    nbModel <- NaiveBayes(CLASS ~ ., data = dataTrain);      
    tmp <- predict(nbModel, dataTest[, -2]) 
    pred$nb[[i]] = tmp$posterior
    # Decision Tree
    dtModel <- rpart(CLASS ~ ., data = dataTrain)
    pred$dt[[i]] <- predict(dtModel, newdata = dataTest, type = 'class')
    # SVM
    svmModel <- svm(CLASS ~ ., data = dataTrain, probability = TRUE)
    pred$svm[[i]] <- predict(svmModel, dataTest[, -2], probability = TRUE)
    # true labels
    labels[[i]] = dataTest[, 2]
}
warnings()
# convert pred and labels to enalbe ROC calculation
for (i in 1 : 3){
    pred[[i]] = convertLabels(pred[[i]], type)
}
labels = convertLabels(labels, type)

perf = vector(mode='list', len=3)
names(perf) = c('nb', 'dt', 'svm')
namesList = names(perf)
for (i in 1 : 3){
    ROCtmp = calROC(pred[[i]], labels)
    perf[[i]] = ROCtmp[[1]]

    tiff(paste(prefix, '10-cross_', namesList[i], '.tiff', sep = ''))
    plot(perf[[i]], col="grey82", lty=3)
    plot(perf[[i]], lwd=3, avg="vertical", spread.estimate="boxplot", add=T)
}
