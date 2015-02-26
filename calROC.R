calROC <- function(score, labels) {

    ROCPred <- prediction(score, labels) 

    # into a standarized format
    curve <- performance(ROCPred, "tpr", "fpr") # true positive 
        # rate and false positve rate
    AUC <- performance(ROCPred, "auc") # area under the ROC
    AUC <- unlist(slot(AUC, "y.values")); # get the value from performance 
    ROC = c(curve, AUC)
    return(ROC)

}
