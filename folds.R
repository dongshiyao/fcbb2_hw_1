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
