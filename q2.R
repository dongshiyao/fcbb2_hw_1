library(e1071)
library(klaR)
library(rpart)
library(ROCR)

source('funcs.R')

args <- commandArgs(trailingOnly = TRUE)
parser = parse_2(args)

class = 'N' # 'N' is set to TRUE and 'D' is set to FALSE
type = parser$type

# load data 

fileName = parser$inputFile
data = read.table(fileName, header = TRUE)
data = data[-1]

# leave-one-out (LOO) cross-validation

pred = vector(mode="list", length=nrow(data))
print('Computing...')

for (i in 1 : nrow(data)){
    dataTrain = data[-i, ]
    dataTest = data[i, ]
    if (type == 'nbayes'){ # Naive Bayes
        nbModel <- NaiveBayes(CLASS ~ ., data = dataTrain);      
        tmp <- predict(nbModel, dataTest[, -1]) 
        tmp = tmp$posterior
    }else if (type == 'dtree'){ # Decision Tree
        dtModel <- rpart(CLASS ~ ., data = dataTrain)
        tmp <- predict(dtModel, newdata = dataTest[, -1], type = 'prob')
    }else if (type == 'svm'){ # SVM
        svmModel <- svm(CLASS ~ ., data = dataTrain, probability = TRUE)
        tmp <- predict(svmModel, dataTest[, -1], probability = TRUE)
        tmp = attr(tmp, 'probabilities')
    }else {
        stop(paste('Unrecognized type:', type))
    }
    pred[[i]] = tmp
}

# convert pred and labels to enalbe ROC calculation
pred = convertLabels(pred, class)
pred = as.double(pred)
labels = convertLabels(data$CLASS, class)

pdf(parser$outputFile)
ROCtmp = calROC(pred, labels)
plotName = paste('ROC_', type, sep = '')
plotROC(ROCtmp[[1]], ROCtmp[[2]], plotName)

print(sprintf('AUC = %.4f', ROCtmp[[2]]))
