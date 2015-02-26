joinFolds <- function(foldsList){
    len = length(foldsList)
    tmp = foldsList[[1]]
    for (i in 2 : len){
        tmp = rbind(tmp, foldsList[[i]])
    }
    return(tmp)
}
        

