#### bootstrap function by different methods
## Non-parametric percentile method
## purpose: generate a bootstrap CI of the mean of a univariate sample of data by percentile method
## input: the data, the confidence level(¦Á) with a default value 0.05
## output: the CI of the mean
## The upper limit a1 : (1-¦Á/2)(b+1)%th
## The lower limit a2:  ¦Á/2(b+1)%th
percentileMethod <- function(dataset, alpha = 0.05) {
  checkArguments(dataset,alpha)
  B <- 10000 # repeat 10,000 times
  n <- length(dataset)
  boot.meanNew <- bootStrap(dataset, B, n)
  # take the upper limit a1 and the lower limit a2
  a1 <- (1-alpha/2) * B /10000  
  a2 <- alpha/2 * B /10000 
  boot.mean.CI <- getCI(boot.meanNew, a1, a2)
}

## Non-parametric BCa method
## purpose: generate a bootstrap CI of the mean of a univariate sample of data by BCa
## input: the data, the confidence level(¦Á) with a default value 0.05, the distribution of the data (only normal or poisson permitted)
## output: the CI of the mean
## The upper limit a1 : stated in the lecture notes
## The lower limit a2:  stated in the lecture notes
BCaMethod <- function(dataset, alpha = 0.05) {
  checkArguments(dataset,alpha)
  B <- 10000 # repeat 10,000 times
  n <- length(dataset)
  sampleMean <- mean(dataset) # the mean of the data set
  
  boot.meanNew <- bootStrap(dataset, B, n)
  boot.meanNew.mean <- mean(boot.meanNew)
  # take the upper limit a1 and the lower limit a2
  # z0: the bias correction factor (estimate of median bias of s(x))
  # alpha.hat: the acceleration parameter (the estimate rate of change of seF(x) with respect to t(F))
  # z.alpha: the 100¦Á percentile of the standard normal
  # source('BCaHelperFunctions_v2.r') # under Linux
  source('D:/MT4113/BCaHelperFunctions_v2.r') # under my laptop Windows
  alpha.hat <- get.ahat(dataset, SingleEst)
  # if the distribution is normal:
  # here I use functions written in BCaHelperFunction_v2.r in Moodle
  z.alpha <- qnorm(alpha)
  z.upper.alpha <- qnorm(1-alpha)
  z0.hat <- get.zhat0(sampleMean, boot.meanNew)
  a1 <- pnorm(z0.hat + (z0.hat+z.upper.alpha)/(1-alpha.hat*(z0.hat+z.upper.alpha)))
  a1 <- a1*B/10000
  a2 <- pnorm(z0.hat + (z0.hat+z.alpha)/(1-alpha.hat*(z0.hat+z.alpha)))
  a2 <- a2*B/10000  
  boot.mean.CI <- getCI(boot.meanNew, a1, a2)
}


########################################################################
#### other useful functions:

## checkArguments
## intputs:
# arg1: the dataset, it should have at least two values, cannot be Inf
# arg2: the alpha, it should be between 0 and 1
checkArguments <- function(arg1, arg2) {
  if(is.numeric(arg1)==FALSE||length(arg1) < 2 || abs(sum(arg1)) == Inf){
    stop("Invalid arguments")
  }
  
  if(is.numeric(arg2)==FALSE||arg2 < 0 || arg2 > 1){
    stop("Invalid arguments")
  }
}

## bootStrap function
## inputs:
# dataset: the input data
# B: 10,000
# n: the length of dataset
bootStrap <- function(dataset, B, n) {
  # generate the bootstrap data set
  boot.set <- replicate(B, dataset[sample.int(n, replace = TRUE)])
  # generate the mean set by bootstrap
  boot.mean <- apply(boot.set, 1, mean)
  # order the mean set from low to high
  boot.mean.newset <- sort(boot.mean)
}

## generate a single estimator
## inputs:
# data: the input data
SingleEst <- function(data) {
  mean(data[sample.int(length(data), replace = TRUE)])
}

## get.zhat0 of Poisson distribution
## inputs:
# est: the mean of the input dataset
# boot.est: the mean set by bootstrap
get.zhat0.poisson<-function(est,boot.est){
  #Purpose: Return the bias correction factor, zhat0, in BCa bootstrap CI method
  #Inputs:
  # est - estimated quantity of interest from data
  # boot.est - vector of bootstrap estimates of quantity of interest
  
  prop.less<-sum(boot.est<est)/length(boot.est)
  zhat<-qpois(prop.less, mean(boot.est))
  return(zhat)
}

## get the bootstrap CI
## inputs:
# data: the mean set by bootstrap from low to high
# upperLimit: the upper confidence limit
# LowerLimit: the lower confidence limit
getCI <- function(data, upperLimit, LowerLimit) {
  FirstNo <- unname(quantile(data,LowerLimit)) # the lower confidence limit
  LastNo <- unname(quantile(data,upperLimit)) # the upper confidence limit
  # get the locations of estimators between FirstNo and LastNo
  numberLocation <- which(data > FirstNo & data < LastNo )
  # creat an CI and fill the value
  boot.CI <- numeric(length(numberLocation))
  for(i in 1:length(numberLocation)) {
    boot.CI[i] <- data[numberLocation[i]]
  }
  boot.CI <- sort(c(boot.CI, FirstNo, LastNo))
}
