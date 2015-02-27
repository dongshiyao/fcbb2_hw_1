# get the spam database into R

library(kernlab)
library(e1071)
library(ROCR)
data(spam)

source('funcs.R')

args <- commandArgs(trailingOnly = TRUE)
parser = parse_1(args)

# split the spam data into training and testing spam sets
testid = which((seq(1, length(spam[, 1]))) %% 5 == 1)
spamTest = spam[testid ,]
spamTrain = spam[-testid ,]

# SVM

predLinear = calSVMPred(spamTrain, spamTest, ker = "linear")
predRadial = calSVMPred(spamTrain, spamTest, ker = "radial")

# -------------------- plot ROC --------------------

type = 'spam'
labels <- convertLabels(spamTest$type, type)

pdf(parser$outFile1)
# plot ROC of linear kernel svm result
predLinear <- convertLabels(predLinear, type)
ROCLinear = calROC(predLinear, labels)
plotROC(ROCLinear[[1]], ROCLinear[[2]], 'kernel: linear')

# plot ROC of radial kernele svm result
predRadial <- convertLabels(predRadial, type)
ROCRadial = calROC(predRadial, labels)
plotROC(ROCRadial[[1]], ROCRadial[[2]], 'kernel: radial')

# -------------------- 10 fold cross-validation --------------------

print('10 folds cross-validation')

foldsNum = 10
spamFolds = folds(spam, foldsNum)
pred = vector(mode = 'list', len = foldsNum)
labels = vector(mode = 'list', len = foldsNum)

for (i in 1 : foldsNum){
    print(paste('fold', i, 'is running...'))
    dataTrain = joinFolds(spamFolds[-i])
    dataTest = spamFolds[[i]]
    pred[[i]] = calSVMPred(dataTrain, dataTest, ker = "radial")
    labels[[i]] = dataTest$type
}
pred = convertLabels(pred, type)
labels = convertLabels(labels, type)

pdf(parser$outFile2)
for (i in 1 : foldsNum){
    ROCtmp = calROC(pred[[i]], labels[[i]])
    plotROC(ROCtmp[[1]], ROCtmp[[2]], paste('kernel: radial, fold:', i))
}
