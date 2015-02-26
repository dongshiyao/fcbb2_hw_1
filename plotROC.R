plotROC = function(curve, AUC, fileName, dir = './') {
    suffix = '.tiff'
    fileName2 = paste(dir, fileName, suffix, sep = '')
    tiff(fileName2)
    plot(curve, colorize = TRUE)
    title(fileName)
    AUC = sprintf("%0.4f", AUC)
    legend(0.6, 0.3, paste('AUC is', AUC), 
        border = 'white', cex = 1.0, box.col = 'white')
}
