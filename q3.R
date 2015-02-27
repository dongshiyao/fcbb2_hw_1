num = 1000

# normal distribution
rvs = rnorm(num, mean = 0, sd = 1)
pvalues = 1 - pnorm(rvs, mean = 0, sd = 1) 
normHist = hist(pvalues, 
    main="histogram of P-values for the standard normal distribution", 
    plot=FALSE)
normHist$counts = normHist$counts / num
pdf('normal.pdf')
plot(normHist, 
    main="pdf plot of the P-values histogram for the standard normal distribution",
    ylab="density")

# beta distribution
rvs = rbeta(num, 2, 5) 
pvalues = 1 - pbeta(rvs, 2, 5)
bHist = hist(pvalues, 
    main="histogram of P-values for the beta distribution",
    plot=FALSE)
bHist$counts = bHist$counts / num
pdf('beta.pdf')
plot(bHist, 
    main="pdf plot of the P-values histogram for the beta distribution", 
    ylab="density")
