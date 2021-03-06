calSVMPred <- function(dataTrain, dataTest, ker = "linear"){
    # calculate the ROC for SVM
    model <- svm(type ~ ., data = dataTrain, kernel = ker,
        probability = TRUE)
    pred <- predict(model, dataTest, probability = TRUE)
    return(pred)
}

calNBPred <- function(dataTrain, dataTest){
    # calculate the ROC for Naive Bayes
    model <- NaiveBayes(type~ ., data = dataTrain);      
    pred <- predict(model, dataTest[, -2]) 
    return(pred)
}

calDTPred <- function(dataTrain, dataTest){
    # calculate the ROC for Decision Tree
    model <- rpart(type~ ., data = dataTrain)
    pred <- predict(model, newdata = dataTest, type = 'class')
    return(pred)
}

calROC <- function(score, labels) {

    # calculate the ROC curve and AUC from predication possibilites and 
    # true labels

    ROCPred <- prediction(score, labels) 

    # into a standarized format
    curve <- performance(ROCPred, "tpr", "fpr") # true positive 
        # rate and false positve rate
    AUC <- performance(ROCPred, "auc") # area under the ROC
    AUC <- unlist(slot(AUC, "y.values")); # get the numerial value of AUC
    ROC = c(curve, AUC)

    return(ROC)

}

convertLabels <- function(pred, ref) {

    # convert pred to enable ROC calculation

    if (class(pred) == 'list'){
        for (i in 1 : length(pred)){
            pred[[i]] = convertSingle(pred[[i]], ref)
        }
    }else{
        pred = convertSingle(pred, ref)
    }
    return(pred)
}

getScore <- function(pred, ref) {

    # get possibilities score from pred according to the type specified     
    # with ref

    if (class(pred) == 'matrix'){
        score = pred
    }else{
        score = attr(pred, 'probabilities')
    }
    if (!is.null(score)){   
        score = score[, ref]
    }
    return(score)
}

convertSingle <- function(pred, ref){

    # convert a single pred into useable possibilities score

    score = getScore(pred, ref)
    if (is.null(score)) {
        pred = (pred == ref) * 1
    }else{
        pred = score
    }
    return(pred)
} 

folds <- function(data, num){
    
    # split data into num folds
    
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

joinFolds <- function(foldsList){
    
    # join multiple folds into one data frame

    len = length(foldsList)
    tmp = foldsList[[1]]
    for (i in 2 : len){
        tmp = rbind(tmp, foldsList[[i]])
    }
    return(tmp)
}
        
parse_1 <- function(args) {

    # parsing input for question 1

    parser = vector(mode = 'list')
    parser$outFile1 = 'ROCholdout.pdf'
    parser$outFile2 = 'ROC10foldCV.pdf'
    
    if (length(args) > 0){
        for (i in 1 : length(args)) {
            item = args[i]
            if (item == '-o'){
                parser$outFile1= args[i + 1]
            }else if (item == '-c'){
                parser$outFile2= args[i + 1]
            }
        }
    }

    return(parser)
}

parse_2 <- function(args) {

    # parsing input for question 2

    parser = vector(mode = 'list')
    parser$type = 'nbayes'
    parser$inputFile = 'Kato_P53_mutants_200.txt'
    parser$outputFile = 'ROC.pdf'

    if (length(args) > 0){
        for (i in 1 : length(args)) {
            item = args[i]
            if (item == '-c'){
                parser$type = args[i + 1]
            }else if (item == '-t') {
                parser$inputFile = args[i + 1]
            }else if (item == '-o'){
                parser$outputFile = args[i + 1]
            }
        }
    }

    return(parser)
}

plotROC = function(curve, AUC, plotName) {

    # plot ROC curve with AUC in legend

    plot(curve, colorize = TRUE)
    AUC = sprintf("%0.4f", AUC)
    legend(0.6, 0.3, paste('AUC is', AUC), 
        border = 'white', cex = 1.0, box.col = 'white')
    title(plotName)
}
