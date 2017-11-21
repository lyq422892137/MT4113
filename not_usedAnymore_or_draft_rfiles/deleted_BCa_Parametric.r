## parametric bootstrap with BCa
## the user should know the distribution of the data
Para.BCaMethod <- function(dataset, alpha = 0.05, B = 10000, distribution = "normal") {
  source(bootstrapFunction)
  disType <- Para.checkArguments(dataset, alpha, B, distribution) # the type of distribution
  lambda <- mean(dataset)
  alpha <- alpha/2
  
  # source('BCaHelperFunctions_v2.r') # under Linux
  source('D:/MT4113/BCaHelperFunctions_v2.r') # under my laptop Windows
  
  if(disType == 1) { # poisson
    boot.new.data <- replicate(B, rpois(length(dataset),lambda))
    # calculate the mean set of the bootstrap data
    boot.mean <- apply(boot.new.data, 1, mean)
    boot.mean <- sort(boot.mean)
    print(boot.mean)
    boot.mean.lambda <- mean(boot.mean)
    
    alpha.hat <- get.ahat(dataset, Para.SingleEst.Poisson)
    z.alpha <- qpois(alpha, boot.mean.lambda)
    z.upper.alpha <- qpois(1-alpha, boot.mean.lambda)
    z0.hat <- get.zhat0.poisson(lambda, boot.mean)
    
    a1 <- ppois(z0.hat + (z0.hat+z.upper.alpha)/(1-alpha.hat*(z0.hat+z.upper.alpha)), boot.mean.lambda)
    a2 <- ppois(z0.hat + (z0.hat+z.alpha)/(1-alpha.hat*(z0.hat+z.alpha)), boot.mean.lambda)
  } else { # normal
    sd <- sd(dataset)
    boot.new.data <- replicate(B,rnorm(length(dataset), lambda, sd))
    # calculate the mean set of the bootstrap data
    boot.mean <- apply(boot.new.data, 1, mean)
    boot.mean <- sort(boot.mean)
    
    alpha.hat <- get.ahat(dataset, Para.SingleEst.Normal)
    z.alpha <- qnorm(alpha)
    z.upper.alpha <- qnorm(1-alpha)
    z0.hat <- get.zhat0(lambda, boot.mean)
    a1 <- pnorm(z0.hat + (z0.hat+z.upper.alpha)/(1-alpha.hat*(z0.hat+z.upper.alpha)))
    a2 <- pnorm(z0.hat + (z0.hat+z.alpha)/(1-alpha.hat*(z0.hat+z.alpha)))
  }
  
  # take the upper limit a1 and the lower limit a2
  boot.mean.CI <- getCI(boot.mean, a1, a2)
}


## generate a single estimator for parametric bootstrap for poisson
## inputs:
# data: the input data
Para.SingleEst.Poisson <- function(data) {
  mean(rpois(length(dataset),mean(dataset)))
}

## generate a single estimator for parametric bootstrap for poisson
## inputs:
# data: the input data
Para.SingleEst.Normal <- function(data) {
  mean(rnorm(length(dataset),mean(dataset)))
}