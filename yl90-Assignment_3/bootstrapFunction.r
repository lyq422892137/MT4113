# I confirm that the attached is my own work, except where clearly indicated in the text.

#### bootstrap function by different methods
#################################################################
#       required methods
#################################################################

## Non-parametric percentile method
## purpose: generate a bootstrap CI of the mean of a univariate sample of data by percentile method
## input: the data, the confidence level(¦Á) with a default value 0.05, B repeat times
## output: the CI of the mean
## The upper limit a1 : (1-¦Á/2)(b+1)%th
## The lower limit a2:  ¦Á/2(b+1)%th
NonPara.percentileMethod <- function(dataset, alpha = 0.05, B = 1000) {
  NonPara.checkArguments(dataset, alpha, B)
  n <- length(dataset)
  boot.meanNew <- bootStrap(dataset, B, n)
  # take the upper limit a1 and the lower limit a2
  a1 <- (1-alpha/2)  
  a2 <- alpha/2 
  boot.mean.CI <- getCI(boot.meanNew, a1, a2)
}

## Non-parametric BCa method
## purpose: generate a bootstrap CI of the mean of a univariate sample of data by BCa
## input: the data, the confidence level(¦Á) with a default value 0.05, B repeat times
## output: the CI of the mean
## The upper limit a1 : stated in the lecture notes
## The lower limit a2:  stated in the lecture notes
NonPara.BCaMethod <- function(dataset, alpha = 0.05, B = 1000) {
  NonPara.checkArguments(dataset,alpha, B)
  n <- length(dataset)
  sampleMean <- mean(dataset) # the mean of the data set
  alpha <- alpha/2
  boot.meanNew <- bootStrap(dataset, B, n)
  # take the upper limit a1 and the lower limit a2
  # z0: the bias correction factor (estimate of median bias of s(x))
  # alpha.hat: the acceleration parameter (the estimate rate of change of seF(x) with respect to t(F))
  # z.alpha: the 100¦Á percentile of the standard normal
  source('BCaHelperFunctions_v2.r') # under Linux, functions supplied by Dr Len
  # source('D:/MT4113/BCaHelperFunctions_v2.r') # under my laptop Windows
  alpha.hat <- get.ahat(dataset, NonPara.SingleEst)
  # if the distribution is normal:
  # here I use functions written in BCaHelperFunction_v2.r in Moodle
  z.alpha <- qnorm(alpha)
  z.upper.alpha <- qnorm(1-alpha)
  z0.hat <- get.zhat0(sampleMean, boot.meanNew)
  a1 <- pnorm(z0.hat + (z0.hat+z.upper.alpha)/(1-alpha.hat*(z0.hat+z.upper.alpha)))
  a2 <- pnorm(z0.hat + (z0.hat+z.alpha)/(1-alpha.hat*(z0.hat+z.alpha)))
  boot.mean.CI <- getCI(boot.meanNew, a1, a2)
}

#################################################################
#       parametric methods
#################################################################

## parametric bootstrap with percentile method
## the user should know the distribution of the data
Para.percentileMethod <- function(dataset, alpha = 0.05, B = 1000, distribution = "normal") {
  disType <- Para.checkArguments(dataset, alpha, B, distribution) # the type of distribution
  lambda <- mean(dataset)
  if(disType == 1) { # poisson
    boot.new.data <- replicate(B, rpois(length(dataset),lambda))
  } else { # normal
    sd <- sd(dataset)
    boot.new.data <- replicate(B,rnorm(length(dataset), lambda, sd))
  }
  # calculate the mean set of the bootstrap data
  boot.mean <- apply(boot.new.data, 2, mean)
  boot.mean <- sort(boot.mean)
  # take the upper limit a1 and the lower limit a2
  a1 <- (1-alpha/2)  
  a2 <- alpha/2 
  boot.mean.CI <- getCI(boot.mean, a1, a2)
}


########################################################################
#### other functions used in functions above:
########################################################################

## checkArguments for Non-parametric bootstrap functions
## intputs:
# arg1: the dataset, it should have at least two values, cannot be Inf
# arg2: the alpha, it should be between 0 and 1
# arg3: B, should be an integer
NonPara.checkArguments <- function(arg1, arg2, arg3) {
  if(is.numeric(arg1)==FALSE||length(arg1) < 3 || abs(sum(arg1)) == Inf){
    stop("Invalid arguments")
  }
  
  if(is.numeric(arg2)==FALSE||arg2 < 0 || arg2 > 1 || length(arg2) != 1){
    stop("Invalid arguments")
  }
  
  if(is.numeric(arg3)==FALSE || length(arg3) != 1 || abs(arg3) == Inf || arg3%%1 != 0 || arg3 <= 0){
    stop("Invalid arguments")
  }
  
}

## checkArguments for Parametric bootstrap functions
## intputs:
# arg1: the dataset, it should have at least two values, cannot be Inf
# arg2: the alpha, it should be between 0 and 1
# arg3: B, should be an integer
# arg4: the distribution of the data
# valid inputs only include:
# 1. Poisson, POISSON, poisson
# 2. Normal, NORMAL, normal
## return value: flag: if the distribution is poisson, flag = 1, or flag = 0
Para.checkArguments <- function(arg1, arg2, arg3, arg4) {
  NonPara.checkArguments(arg1,arg2,arg3)
  
  if(arg4 == "Poisson" || arg4 == "poisson" || arg4 == "POISSON") {
    flag <- 1
  } else if (arg4 == "Normal" || arg4 == "normal" || arg4 == "NORMAL") {
    flag <- 0
  } else {
    stop("Invalid arguments")
  }
  return(flag)
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
  boot.mean <- apply(boot.set, 2, mean)
  # order the mean set from low to high
  boot.mean.newset <- sort(boot.mean)
}

## generate a single estimator for non-parametric bootstrap
## inputs:
# data: the input data
NonPara.SingleEst <- function(data) {
  mean(data[sample.int(length(data), replace = TRUE)])
}


## get.zhat0 of Poisson distribution
## refers get.zhat0() in BCaHelperFunctions_v2.r
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
# here it is not necessary to obtain estimated means 
# the reason I do it because I want to calculate the mean of the mean set
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
