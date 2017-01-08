library(ggplot2)

##Part 1 - Simulation Exercise for Exponential Distribution
set.seed(1264)

###Set paramaters
n <- 40
lambda <- 0.2
numsimulations <-1000

###Calculate Theoretical Properties
theoreticalmean <- round(1/lambda,2)
theoreticalsd <- round(1/lambda,2)
theoreticalvar <- round(1/lambda^2,2)

###Simulate 1000 trials, and calculate the properties of the distribution
simdata <- matrix(rexp(n*numsimulations,lambda), numsimulations) 
simmeansdata <- round(apply(simdata,1,mean),2)
simmean <-round(mean(simmeansdata),2)
simsd <- round(sd(simmeansdata),2)*sqrt(n)
simvar <- round(simsd^2,2)

###Table of Values
tablevalues <- matrix(c(theoreticalmean, simmean,theoreticalvar, simvar, 
                        theoreticalsd, simsd), ncol = 2, byrow = TRUE )
colnames(tablevalues) <- c("Theoretical", "Simulated")
rownames(tablevalues) <- c("Mean", "Variance", "Standard Deviation")
tablevalues <- as.table(tablevalues)
tablevalues

###Turn simulated means data into a data frame so it can be plotted
simmeansdatadf <- data.frame(simmeansdata)

ggplot(simmeansdatadf, aes(x = simmeansdata)) + 
    geom_histogram(binwidth = 0.4, color = 'black', fill = 'dark green', aes(y = ..density..)) +
    stat_function(fun = dnorm, color = 'red', size = 2, args = list(mean = simmean, sd = .75)) +
    xlab('Sample mean') + geom_vline(xintercept = theoreticalmean, color = 'orange', size = 2) + 
    ylab('Density') + ggtitle("Simulated Exponential Distribution Comparision")