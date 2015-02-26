convertLabels <- function(pred, ref) {
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
    score = getScore(pred, ref)
    if (is.null(score)) {
        pred = (pred == ref) * 1
    }else{
        pred = score
    }
    return(pred)
} 


