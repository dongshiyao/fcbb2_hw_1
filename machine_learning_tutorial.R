# iris data 
data(iris);
head(iris);
summary(iris);

# some plotting
boxplot(Sepal.Length ~ Species, data = iris);
pairs(iris[1 : 4], main = "Iris Data", pch = 21,            
    bg = c("red", "green3", "blue")[unclass(iris$Species)])
    # plotting every possible pair 

# Naive Bayes

testidx <- which(1 : length(iris[, 1]) %% 5 == 0)           
    # every 5 elements to form testing data
iristrain <- iris[-testidx, ]
iristest <- iris[testidx, ]

library(klaR)
nbmodel <- NaiveBayes(Species ~ ., data = iristrain);      
    # Species ~. means get the model with dependent variable 
    # Species and control variables except Species 

prediction <- predict(nbmodel, iristest[, -5]) # use variables except the
    # 5th column (Species) to predict testing cases
table(prediction$class, iristest[, 5])
    # compare the results between predition and the true lables. The verti-
    # cal line is prediction and the horizontal line is true lables

# Decision Tree
library(rpart)
treemodel <- rpart(Species ~ ., data = iristrain)
plot(treemodel)
text(treemodel, use.n = T)

prediction <- predict(treemodel, newdata = iristest, type = 'class')
table(prediction, iristest[, 5])

# Support Vector Machine
library(e1071)
model <- svm(Species ~ ., data = iristrain)
prediction <- predict(model, iristest)
table(iristest$Species, prediction)

plot(model, iris, Petal.Width ~ Petal.Length,  # the dots are plotted in
    # Petal.Width ~ Petal.Length plane
     slice = list(Sepal.Width = 3, Sepal.Length = 4)) # I believe the slice
    # here is one 2d slice of the SVM thresholds boundry in 4D space
    # but what's the meaning of X and O?

# SVM TUNE
tune <- tune.svm(Species ~ ., data = iristrain, 
    gamma = 10^(-5:0), cost=10^(0:5))
summary(tune)
# I think this may result overfitting
model <- svm(Species ~ ., data = iristrain, probability = T, 
    gamma = 0.001, cost = 10000)
prediction <- predict(model, iristest)
table(iristest$Species, prediction)

# ROC curve for Naive Bayes
library(klaR)
nbmodel <- NaiveBayes(Species ~ ., data = iristrain)
prediction <- predict(nbmodel, iristest[, -5])
table(prediction$class, iristest[, 5])
library(ROCR)
nbmodel <- NaiveBayes(Species ~ ., data = iristrain)
nbprediction <- predict(nbmodel, iristest[, -5], type = 'raw')
score <- nbprediction$posterior[, c("virginica")]
actual_class <- iristest$Species == 'virginica'
pred <- prediction(score, actual_class) # transform predictions and labels
    # into a standarized format
nbpref <- performance(pred, "tpr", "fpr") # true positive rate and false
    # positve rate
nbauc <- performance(pred, "auc") # area under the ROC
nbauc <- unlist(slot(nbauc, "y.values")) # get the value from performance 
    # object
plot(nbpref, colorize = TRUE)
legend(0.6, 0.3, c(c(paste('AUC is', nbauc)), "\n"),
border = "white", cex = 1.0, box.col = "white")

# 10 cross validation
data(ROCR.xval)
pred <- prediction(ROCR.xval$predictions, ROCR.xval$labels)
perf <- performance(pred, "tpr", "fpr")

plot(perf, col = "grey82", lty = 3)
plot(perf, lwd = 3, avg = "vertical", spread.estimate = "boxplot", add = T)
