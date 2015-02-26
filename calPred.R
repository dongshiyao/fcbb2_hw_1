calPred <- function(dataTrain, dataTest, ker = "linear"){
    model <- svm(type ~ ., data = dataTrain, kernel = ker,
        probability = TRUE)
    pred <- predict(model, dataTest, probability = TRUE)
    print(paste(ker, "kernel result:"))
    t = table(dataTest$type, pred)
    print(t)
    return(pred)
}
