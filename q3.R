# generate 1000 random variables
num = 1000
rvs = rnorm(num, mean = 0, sd = 1)
pvalues = 1 - pnorm(rvs, mean = 0, sd = 1) 
normHist = hist(pvalues);
normHist$counts = normHist$counts / num
plot(normHist)

rvs = rbeta(num, 2, 5) 
pvalues = 1 - pbeta(rvs, 2, 5)
bHist = hist(pvalues);
bHist$counts = bHist$counts / num
plot(bHist)
