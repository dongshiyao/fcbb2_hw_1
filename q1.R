# get the spam database into R

library(kernlab)
library(e1071)
library(ROCR)
data(spam)

source('calROC.R')
source('calPred.R')
source('convertLabels.R')
source('plotROC.R')
source('joinFolds.R')

source('folds.R')

resultsDir = './q1_results/'

# split the spam data into training and testing spam sets
testid = which((seq(1, length(spam[, 1]))) %% 5 == 1)
spamTest = spam[testid ,]
spamTrain = spam[-testid ,]

# SVM

predLinear = calPred(spamTrain, spamTest, ker = "linear")
predRadial = calPred(spamTrain, spamTest, ker = "radial")
# predPoly = calPred(spamTrain, spamTest, ker = "polynomial")
# predSig = calPred(spamTrain, spamTest, ker = "sigmoid")

# -------------------- plot ROC --------------------

type = 'spam'
labels <- convertLabels(spamTest$type, type)

# plot ROC of linear kernel svm result
predLinear <- convertLabels(predLinear, type)
ROCLinear = calROC(predLinear, labels)
plotROC(ROCLinear[[1]], ROCLinear[[2]], 'linear_kernel')

# plot ROC of radial kernele svm result
predRadial <- convertLabels(predRadial, type)
ROCRadial = calROC(predRadial, labels)
plotROC(ROCRadial[[1]], ROCRadial[[2]], 'radial_kernel')

# -------------------- 10 fold cross-validation --------------------

foldsNum = 10
spamFolds = folds(spam, foldsNum)
pred = vector(mode = 'list', len = foldsNum)
labels = vector(mode = 'list', len = foldsNum)

for (i in 1 : foldsNum){
    dataTrain = joinFolds(spamFolds[-i])
    dataTest = spamFolds[[i]]
    pred[[i]] = calPred(dataTrain, dataTest, ker = "radial")
    labels[[i]] = dataTest$type
}
pred = convertLabels(pred, type)
labels = convertLabels(labels, type)

ROCtmp = calROC(pred, labels)
perf = ROCtmp[[1]]
tiff(paste(resultsDir, '10-cross.tiff', sep = ''))
plot(perf, col = "grey82", lty = 3)
plot(perf, lwd = 3, avg = 'vertical', spread.estimate = "boxplot", add = T)
