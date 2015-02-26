crossValidation <- function(data, foldsNum, typeCol) {

    dataFolds = folds(data, foldsNum)
    pred = vector(mode = 'list', len = foldsNum)
    labels = vector(mode = 'list', len = foldsNum)
    for (i in 1 : foldsNum){
        dataTrain = joinFolds(spamFolds[-i])
        dataTest = spamFolds[[i]]
        pred[[i]] = calPred(dataTrain, dataTest, ker = "radial")
        lables[[i]] = dataTest[, typeCol]
    }
    pred = convertLables(pred, 'spam')
    lables = convertLables(lables, 'spam')
    ROCtmp = calROC(pred, lables)
    perf = ROCtmp[[1]]
    tiff('10-cross.tiff')
    plot(perf, col = "grey82", lty = 3)
    plot(perf, lwd = 3, avg = "threshold", spread.estimate = "boxplot", add = T)
}     

folds <- function(data, num){
    
    len = nrow(data)
    res = len %% num;
    data = data[1 : (len - res), ]
    f = vector(mode = "list", length = num)
    for (i in 1 : num){
        indeces = seq(i, nrow(data), num)
        f[[i]] = data[indeces, ]
    }
    return(f)

}
