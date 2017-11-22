#### simulation
## source the r file containing bootstrap functions
# source('bootstrapFunction.r')
source('D:/MT4113/bootstrapFunction.r') # under Windows
################################################################
###############################################################
#### simulation 1:
#### Different distributions (Normal or Poisson) 
#### with the same sample size 
#### by the same method 
#### with the same alpha

## NonPara.percentileMethod
# Normal datasets
# 1000 observations
# alpha = 0.05
simulation1 <- function(round = 10) {
  count.overall <- 0
  count.right <- 0
  count.left <- 0
  difference <- numeric(round)
  for(i in 1:round) {
    ## first, set a seed
    set.seed(123145)
    data.normal<-rnorm(1000,4,5)
    data.mean <- mean(data.normal)
    result1 <- NonPara.percentileMethod(data.normal)
    if(data.mean>=result1[1] & data.mean<=result1[length(result1)]){
      count.overall <- count.overall + 1
    } else if (data.mean>=result1[1]) {
      count.right <- count.right + 1
    } else {
      count.left <- count.left + 1
    }
    difference[i] <- result1[length(result1)] - result1[1]
  }
  print(paste("The percentage that the boostrap CI contains the ture mean is: ", count.overall/round))
  print(paste("The percentage the ture mean is smaller than the CI: ", count.left/round))
  print(paste("The percentage the ture mean is bigger than the CI: ", count.right/round))
  print(paste("The mean of the length of the CI: ", mean(difference)))
  print(paste("The mean of the dataset: ", data.mean, " The mean of the boostrap: ", mean(result1)))
  print(paste("The difference of the means: ", data.mean - mean(result1)))
  
}

#### simulation 2:
#### Different distributions (Normal or Poisson) 
#### with the same sample size 
#### by the same method 
#### with the same alpha
## NonPara.percentileMethod
# Poisson datasets
# 1000 observations
# alpha = 0.05
simulation2 <- function(round = 10) {
  count.overall <- 0
  count.right <- 0
  count.left <- 0
  difference <- numeric(round)
  for(i in 1:round) {
    ## first, set a seed
    set.seed(123145)
    data.poisson<-rpois(1000,4)
    data.mean <- mean(data.poisson)
    result1 <- NonPara.percentileMethod(data.poisson)
    if(data.mean>=result1[1]&data.mean<=result1[length(result1)]){
      count.overall <- count.overall + 1
    } else if (data.mean>=result1[1]) {
      count.right <- count.right + 1
    } else {
      count.left <- count.left + 1
    }
    difference[i] <- result1[length(result1)] - result1[1]
  }
  print(paste("The percentage that the boostrap CI contains the ture mean is: ", count.overall/round))
  print(paste("The percentage the ture mean is smaller than the CI: ", count.left/round))
  print(paste("The percentage the ture mean is bigger than the CI: ", count.right/round))
  print(paste("The mean of the length of the CI: ", mean(difference)))
  print(paste("The mean of the dataset: ", data.mean, " The mean of the boostrap: ", mean(result1)))
  print(paste("The difference of the means: ", data.mean - mean(result1)))
  
}


#### simulation 3:
#### Different distributions (Normal or Poisson) 
#### with the same sample size 
#### by the same method 
#### with the same alpha
## NonPara.percentileMethod
# Normal datasets
# 10 observations
# alpha = 0.05
simulation3 <- function(round = 10) {
  count.overall <- 0
  count.right <- 0
  count.left <- 0
  difference <- numeric(round)
  for(i in 1:round) {
    ## first, set a seed
    set.seed(123145)
    data.normal<-rnorm(10,4,5)
    data.mean <- mean(data.normal)
    result1 <- NonPara.percentileMethod(data.normal)
    if(data.mean>=result1[1]&data.mean<=result1[length(result1)]){
      count.overall <- count.overall + 1
    } else if (data.mean>=result1[1]) {
      count.right <- count.right + 1
    } else {
      count.left <- count.left + 1
    }
    difference[i] <- result1[length(result1)] - result1[1]
  }
  print(paste("The percentage that the boostrap CI contains the ture mean is: ", count.overall/round))
  print(paste("The percentage the ture mean is smaller than the CI: ", count.left/round))
  print(paste("The percentage the ture mean is bigger than the CI: ", count.right/round))
  print(paste("The mean of the length of the CI: ", mean(difference)))
  print(paste("The mean of the dataset: ", data.mean, " The mean of the boostrap: ", mean(result1)))
  print(paste("The difference of the means: ", data.mean - mean(result1)))
  
}

#### simulation 4:
#### Different distributions (Normal or Poisson) 
#### with the same sample size 
#### by the same method 
#### with the same alpha
## NonPara.percentileMethod
#  Poisson datasets
# 10 observations
# alpha = 0.05
simulation4 <- function(round = 10) {
  count.overall <- 0
  count.right <- 0
  count.left <- 0
  difference <- numeric(round)
  for(i in 1:round) {
    ## first, set a seed
    set.seed(123145)
    data.poisson<-rpois(10,4)
    data.mean <- mean(data.poisson)
    result1 <- NonPara.percentileMethod(data.poisson)
    if(data.mean>=result1[1]&data.mean<=result1[length(result1)]){
      count.overall <- count.overall + 1
    } else if (data.mean>=result1[1]) {
      count.right <- count.right + 1
    } else {
      count.left <- count.left + 1
    }
    difference[i] <- result1[length(result1)] - result1[1]
  }
  print(paste("The percentage that the boostrap CI contains the ture mean is: ", count.overall/round))
  print(paste("The percentage the ture mean is smaller than the CI: ", count.left/round))
  print(paste("The percentage the ture mean is bigger than the CI: ", count.right/round))
  print(paste("The mean of the length of the CI: ", mean(difference)))
  print(paste("The mean of the dataset: ", data.mean, " The mean of the boostrap: ", mean(result1)))
  print(paste("The difference of the means: ", data.mean - mean(result1)))
  
}

#### simulation 5:
#### Different distributions (Normal or Poisson) 
#### with the same sample size 
#### by the same method 
#### with the same alpha
## NonPara.BCaMethod
#  Normal datasets
# 1000 observations
# alpha = 0.05
simulation5 <- function(round = 10) {
  count.overall <- 0
  count.right <- 0
  count.left <- 0
  difference <- numeric(round)
  for(i in 1:round) {
    ## first, set a seed
    set.seed(123145)
    data.normal<-rnorm(1000,4)
    data.mean <- mean(data.normal)
    result1 <- NonPara.BCaMethod(data.normal)
    if(data.mean>=result1[1]&data.mean<=result1[length(result1)]){
      count.overall <- count.overall + 1
    } else if (data.mean>=result1[1]) {
      count.right <- count.right + 1
    } else {
      count.left <- count.left + 1
    }
    difference[i] <- result1[length(result1)] - result1[1]
  }
  print(paste("The percentage that the boostrap CI contains the ture mean is: ", count.overall/round))
  print(paste("The percentage the ture mean is smaller than the CI: ", count.left/round))
  print(paste("The percentage the ture mean is bigger than the CI: ", count.right/round))
  print(paste("The mean of the length of the CI: ", mean(difference)))
  print(paste("The mean of the dataset: ", data.mean, " The mean of the boostrap: ", mean(result1)))
  print(paste("The difference of the means: ", data.mean - mean(result1)))
  
}

#### simulation 6:
#### Different distributions (Normal or Poisson) 
#### with the same sample size 
#### by the same method 
#### with the same alpha
## NonPara.BCaMethod
#  poisson datasets
# 1000 observations
# alpha = 0.05
simulation6 <- function(round = 10) {
  count.overall <- 0
  count.right <- 0
  count.left <- 0
  difference <- numeric(round)
  for(i in 1:round) {
    ## first, set a seed
    set.seed(123145)
    data.poisson<-rpois(1000,4)
    data.mean <- mean(data.poisson)
    result1 <- NonPara.BCaMethod(data.poisson)
    if(data.mean>=result1[1]&data.mean<=result1[length(result1)]){
      count.overall <- count.overall + 1
    } else if (data.mean>=result1[1]) {
      count.right <- count.right + 1
    } else {
      count.left <- count.left + 1
    }
    difference[i] <- result1[length(result1)] - result1[1]
  }
  print(paste("The percentage that the boostrap CI contains the ture mean is: ", count.overall/round))
  print(paste("The percentage the ture mean is smaller than the CI: ", count.left/round))
  print(paste("The percentage the ture mean is bigger than the CI: ", count.right/round))
  print(paste("The mean of the length of the CI: ", mean(difference)))
  print(paste("The mean of the dataset: ", data.mean, " The mean of the boostrap: ", mean(result1)))
  print(paste("The difference of the means: ", data.mean - mean(result1)))
  
}


#### simulation 7:
#### Different distributions (Normal or Poisson) 
#### with the same sample size 
#### by the same method 
#### with the same alpha
## NonPara.BCaMethod
#  normal datasets
# 10 observations
# alpha = 0.05
simulation7 <- function(round = 10) {
  count.overall <- 0
  count.right <- 0
  count.left <- 0
  difference <- numeric(round)
  for(i in 1:round) {
    ## first, set a seed
    set.seed(123145)
    data<-rnorm(10,4)
    data.mean <- mean(data)
    result1 <- NonPara.BCaMethod(data)
    if(data.mean>=result1[1]&data.mean<=result1[length(result1)]){
      count.overall <- count.overall + 1
    } else if (data.mean>=result1[1]) {
      count.right <- count.right + 1
    } else {
      count.left <- count.left + 1
    }
    difference[i] <- result1[length(result1)] - result1[1]
  }
  print(paste("The percentage that the boostrap CI contains the ture mean is: ", count.overall/round))
  print(paste("The percentage the ture mean is smaller than the CI: ", count.left/round))
  print(paste("The percentage the ture mean is bigger than the CI: ", count.right/round))
  print(paste("The mean of the length of the CI: ", mean(difference)))
  print(paste("The mean of the dataset: ", data.mean, " The mean of the boostrap: ", mean(result1)))
  print(paste("The difference of the means: ", data.mean - mean(result1)))
  
}

#### simulation 8:
#### Different distributions (Normal or Poisson) 
#### with the same sample size 
#### by the same method 
#### with the same alpha
## NonPara.BCaMethod
#  poisson datasets
# 10 observations
# alpha = 0.05
simulation8 <- function(round = 10) {
  count.overall <- 0
  count.right <- 0
  count.left <- 0
  difference <- numeric(round)
  for(i in 1:round) {
    ## first, set a seed
    set.seed(123145)
    data<-rpois(10,4)
    data.mean <- mean(data)
    result1 <- NonPara.BCaMethod(data)
    if(data.mean>=result1[1]&data.mean<=result1[length(result1)]){
      count.overall <- count.overall + 1
    } else if (data.mean>=result1[1]) {
      count.right <- count.right + 1
    } else {
      count.left <- count.left + 1
    }
    difference[i] <- result1[length(result1)] - result1[1]
  }
  print(paste("The percentage that the boostrap CI contains the ture mean is: ", count.overall/round))
  print(paste("The percentage the ture mean is smaller than the CI: ", count.left/round))
  print(paste("The percentage the ture mean is bigger than the CI: ", count.right/round))
  print(paste("The mean of the length of the CI: ", mean(difference)))
  print(paste("The mean of the dataset: ", data.mean, " The mean of the boostrap: ", mean(result1)))
  print(paste("The difference of the means: ", data.mean - mean(result1)))
  
}


#### simulation 9:
#### Different distributions (Normal or Poisson) 
#### with the same sample size 
#### by the same method 
#### with the same alpha
## Para.percentileMethod
#  normal datasets
# 1000 observations
# alpha = 0.05
simulation9 <- function(round = 10) {
  count.overall <- 0
  count.right <- 0
  count.left <- 0
  difference <- numeric(round)
  for(i in 1:round) {
    ## first, set a seed
    set.seed(123145)
    data<-rnorm(1000,4)
    data.mean <- mean(data)
    result1 <- Para.percentileMethod(data)
    if(data.mean>=result1[1]&data.mean<=result1[length(result1)]){
      count.overall <- count.overall + 1
    } else if (data.mean>=result1[1]) {
      count.right <- count.right + 1
    } else {
      count.left <- count.left + 1
    }
    difference[i] <- result1[length(result1)] - result1[1]
  }
  print(paste("The percentage that the boostrap CI contains the ture mean is: ", count.overall/round))
  print(paste("The percentage the ture mean is smaller than the CI: ", count.left/round))
  print(paste("The percentage the ture mean is bigger than the CI: ", count.right/round))
  print(paste("The mean of the length of the CI: ", mean(difference)))
  print(paste("The mean of the dataset: ", data.mean, " The mean of the boostrap: ", mean(result1)))
  print(paste("The difference of the means: ", data.mean - mean(result1)))
  
}

#### simulation 10:
#### Different distributions (Normal or Poisson) 
#### with the same sample size 
#### by the same method 
#### with the same alpha
## Para.percentileMethod
#  poisson datasets
# 1000 observations
# alpha = 0.05
simulation10 <- function(round = 10) {
  count.overall <- 0
  count.right <- 0
  count.left <- 0
  difference <- numeric(round)
  for(i in 1:round) {
    ## first, set a seed
    set.seed(123145)
    data<-rpois(1000,4)
    data.mean <- mean(data)
    result1 <- Para.percentileMethod(data)
    if(data.mean>=result1[1]&data.mean<=result1[length(result1)]){
      count.overall <- count.overall + 1
    } else if (data.mean>=result1[1]) {
      count.right <- count.right + 1
    } else {
      count.left <- count.left + 1
    }
    difference[i] <- result1[length(result1)] - result1[1]
  }
  print(paste("The percentage that the boostrap CI contains the ture mean is: ", count.overall/round))
  print(paste("The percentage the ture mean is smaller than the CI: ", count.left/round))
  print(paste("The percentage the ture mean is bigger than the CI: ", count.right/round))
  print(paste("The mean of the length of the CI: ", mean(difference)))
  print(paste("The mean of the dataset: ", data.mean, " The mean of the boostrap: ", mean(result1)))
  print(paste("The difference of the means: ", data.mean - mean(result1)))
  
}


#### simulation 11:
#### Different distributions (Normal or Poisson) 
#### with the same sample size 
#### by the same method 
#### with the same alpha
## Para.percentileMethod
#  normal datasets
# 10 observations
# alpha = 0.05
simulation11 <- function(round = 10) {
  count.overall <- 0
  count.right <- 0
  count.left <- 0
  difference <- numeric(round)
  for(i in 1:round) {
    ## first, set a seed
    set.seed(123145)
    data<-rnorm(10,4)
    data.mean <- mean(data)
    result1 <- Para.percentileMethod(data)
    if(data.mean>=result1[1]&data.mean<=result1[length(result1)]){
      count.overall <- count.overall + 1
    } else if (data.mean>=result1[1]) {
      count.right <- count.right + 1
    } else {
      count.left <- count.left + 1
    }
    difference[i] <- result1[length(result1)] - result1[1]
  }
  print(paste("The percentage that the boostrap CI contains the ture mean is: ", count.overall/round))
  print(paste("The percentage the ture mean is smaller than the CI: ", count.left/round))
  print(paste("The percentage the ture mean is bigger than the CI: ", count.right/round))
  print(paste("The mean of the length of the CI: ", mean(difference)))
  print(paste("The mean of the dataset: ", data.mean, " The mean of the boostrap: ", mean(result1)))
  print(paste("The difference of the means: ", data.mean - mean(result1)))
  
}

#### simulation 12:
#### Different distributions (Normal or Poisson) 
#### with the same sample size 
#### by the same method 
#### with the same alpha
## Para.percentileMethod
#  poisson datasets
# 10 observations
# alpha = 0.05
simulation12 <- function(round = 10) {
  count.overall <- 0
  count.right <- 0
  count.left <- 0
  difference <- numeric(round)
  for(i in 1:round) {
    ## first, set a seed
    set.seed(123145)
    data<-rpois(10,4)
    data.mean <- mean(data)
    result1 <- Para.percentileMethod(data)
    if(data.mean>=result1[1]&data.mean<=result1[length(result1)]){
      count.overall <- count.overall + 1
    } else if (data.mean>=result1[1]) {
      count.right <- count.right + 1
    } else {
      count.left <- count.left + 1
    }
    difference[i] <- result1[length(result1)] - result1[1]
  }
  print(paste("The percentage that the boostrap CI contains the ture mean is: ", count.overall/round))
  print(paste("The percentage the ture mean is smaller than the CI: ", count.left/round))
  print(paste("The percentage the ture mean is bigger than the CI: ", count.right/round))
  print(paste("The mean of the length of the CI: ", mean(difference)))
  print(paste("The mean of the dataset: ", data.mean, " The mean of the boostrap: ", mean(result1)))
  print(paste("The difference of the means: ", data.mean - mean(result1)))
  
}

#### simulation 13:
#### Different distributions (Normal or Poisson) 
#### with the same sample size 
#### by the same method 
#### with the same alpha

## NonPara.percentileMethod
# Normal datasets
# 1000 observations
# alpha = 0.01
simulation13 <- function(round = 10) {
  count.overall <- 0
  count.right <- 0
  count.left <- 0
  difference <- numeric(round)
  for(i in 1:round) {
    ## first, set a seed
    set.seed(123145)
    data.normal<-rnorm(1000,4,5)
    data.mean <- mean(data.normal)
    result1 <- NonPara.percentileMethod(data.normal, alpha = 0.01)
    if(data.mean>=result1[1] & data.mean<=result1[length(result1)]){
      count.overall <- count.overall + 1
    } else if (data.mean>=result1[1]) {
      count.right <- count.right + 1
    } else {
      count.left <- count.left + 1
    }
    difference[i] <- result1[length(result1)] - result1[1]
  }
  print(paste("The percentage that the boostrap CI contains the ture mean is: ", count.overall/round))
  print(paste("The percentage the ture mean is smaller than the CI: ", count.left/round))
  print(paste("The percentage the ture mean is bigger than the CI: ", count.right/round))
  print(paste("The mean of the length of the CI: ", mean(difference)))
  print(paste("The mean of the dataset: ", data.mean, " The mean of the boostrap: ", mean(result1)))
  print(paste("The difference of the means: ", data.mean - mean(result1)))
  
}


#### simulation 14:
#### Different distributions (Normal or Poisson) 
#### with the same sample size 
#### by the same method 
#### with the same alpha
## NonPara.percentileMethod
# Poisson datasets
# 1000 observations
# alpha = 0.01
simulation14 <- function(round = 10) {
  count.overall <- 0
  count.right <- 0
  count.left <- 0
  difference <- numeric(round)
  for(i in 1:round) {
    ## first, set a seed
    set.seed(123145)
    data.poisson<-rpois(1000,4)
    data.mean <- mean(data.poisson)
    result1 <- NonPara.percentileMethod(data.poisson,alpha = 0.01)
    if(data.mean>=result1[1]&data.mean<=result1[length(result1)]){
      count.overall <- count.overall + 1
    } else if (data.mean>=result1[1]) {
      count.right <- count.right + 1
    } else {
      count.left <- count.left + 1
    }
    difference[i] <- result1[length(result1)] - result1[1]
  }
  print(paste("The percentage that the boostrap CI contains the ture mean is: ", count.overall/round))
  print(paste("The percentage the ture mean is smaller than the CI: ", count.left/round))
  print(paste("The percentage the ture mean is bigger than the CI: ", count.right/round))
  print(paste("The mean of the length of the CI: ", mean(difference)))
  print(paste("The mean of the dataset: ", data.mean, " The mean of the boostrap: ", mean(result1)))
  print(paste("The difference of the means: ", data.mean - mean(result1)))
  
}

#### simulation 15:
#### Different distributions (Normal or Poisson) 
#### with the same sample size 
#### by the same method 
#### with the same alpha
## NonPara.BCaMethod
#  Normal datasets
# 1000 observations
# alpha = 0.01
simulation15 <- function(round = 10) {
  count.overall <- 0
  count.right <- 0
  count.left <- 0
  difference <- numeric(round)
  for(i in 1:round) {
    ## first, set a seed
    set.seed(123145)
    data.normal<-rnorm(1000,4)
    data.mean <- mean(data.normal)
    result1 <- NonPara.BCaMethod(data.normal,alpha = 0.01)
    if(data.mean>=result1[1]&data.mean<=result1[length(result1)]){
      count.overall <- count.overall + 1
    } else if (data.mean>=result1[1]) {
      count.right <- count.right + 1
    } else {
      count.left <- count.left + 1
    }
    difference[i] <- result1[length(result1)] - result1[1]
  }
  print(paste("The percentage that the boostrap CI contains the ture mean is: ", count.overall/round))
  print(paste("The percentage the ture mean is smaller than the CI: ", count.left/round))
  print(paste("The percentage the ture mean is bigger than the CI: ", count.right/round))
  print(paste("The mean of the length of the CI: ", mean(difference)))
  print(paste("The mean of the dataset: ", data.mean, " The mean of the boostrap: ", mean(result1)))
  print(paste("The difference of the means: ", data.mean - mean(result1)))
  
}

#### simulation 16:
#### Different distributions (Normal or Poisson) 
#### with the same sample size 
#### by the same method 
#### with the same alpha
## NonPara.BCaMethod
#  poisson datasets
# 1000 observations
# alpha = 0.01
simulation16 <- function(round = 10) {
  count.overall <- 0
  count.right <- 0
  count.left <- 0
  difference <- numeric(round)
  for(i in 1:round) {
    ## first, set a seed
    set.seed(123145)
    data.poisson<-rpois(1000,4)
    data.mean <- mean(data.poisson)
    result1 <- NonPara.BCaMethod(data.poisson,alpha = 0.01)
    if(data.mean>=result1[1]&data.mean<=result1[length(result1)]){
      count.overall <- count.overall + 1
    } else if (data.mean>=result1[1]) {
      count.right <- count.right + 1
    } else {
      count.left <- count.left + 1
    }
    difference[i] <- result1[length(result1)] - result1[1]
  }
  print(paste("The percentage that the boostrap CI contains the ture mean is: ", count.overall/round))
  print(paste("The percentage the ture mean is smaller than the CI: ", count.left/round))
  print(paste("The percentage the ture mean is bigger than the CI: ", count.right/round))
  print(paste("The mean of the length of the CI: ", mean(difference)))
  print(paste("The mean of the dataset: ", data.mean, " The mean of the boostrap: ", mean(result1)))
  print(paste("The difference of the means: ", data.mean - mean(result1)))
  
}



#### simulation 17:
#### Different distributions (Normal or Poisson) 
#### with the same sample size 
#### by the same method 
#### with the same alpha
## Para.percentileMethod
#  normal datasets
# 1000 observations
# alpha = 0.01
simulation17 <- function(round = 10) {
  count.overall <- 0
  count.right <- 0
  count.left <- 0
  difference <- numeric(round)
  for(i in 1:round) {
    ## first, set a seed
    set.seed(123145)
    data<-rnorm(1000,4)
    data.mean <- mean(data)
    result1 <- Para.percentileMethod(data,alpha = 0.01)
    if(data.mean>=result1[1]&data.mean<=result1[length(result1)]){
      count.overall <- count.overall + 1
    } else if (data.mean>=result1[1]) {
      count.right <- count.right + 1
    } else {
      count.left <- count.left + 1
    }
    difference[i] <- result1[length(result1)] - result1[1]
  }
  print(paste("The percentage that the boostrap CI contains the ture mean is: ", count.overall/round))
  print(paste("The percentage the ture mean is smaller than the CI: ", count.left/round))
  print(paste("The percentage the ture mean is bigger than the CI: ", count.right/round))
  print(paste("The mean of the length of the CI: ", mean(difference)))
  print(paste("The mean of the dataset: ", data.mean, " The mean of the boostrap: ", mean(result1)))
  print(paste("The difference of the means: ", data.mean - mean(result1)))
  
}

#### simulation 18:
#### Different distributions (Normal or Poisson) 
#### with the same sample size 
#### by the same method 
#### with the same alpha
## Para.percentileMethod
#  poisson datasets
# 1000 observations
# alpha = 0.01
simulation18 <- function(round = 10) {
  count.overall <- 0
  count.right <- 0
  count.left <- 0
  difference <- numeric(round)
  for(i in 1:round) {
    ## first, set a seed
    set.seed(123145)
    data<-rpois(1000,4)
    data.mean <- mean(data)
    result1 <- Para.percentileMethod(data,alpha = 0.01)
    if(data.mean>=result1[1]&data.mean<=result1[length(result1)]){
      count.overall <- count.overall + 1
    } else if (data.mean>=result1[1]) {
      count.right <- count.right + 1
    } else {
      count.left <- count.left + 1
    }
    difference[i] <- result1[length(result1)] - result1[1]
  }
  print(paste("The percentage that the boostrap CI contains the ture mean is: ", count.overall/round))
  print(paste("The percentage the ture mean is smaller than the CI: ", count.left/round))
  print(paste("The percentage the ture mean is bigger than the CI: ", count.right/round))
  print(paste("The mean of the length of the CI: ", mean(difference)))
  print(paste("The mean of the dataset: ", data.mean, " The mean of the boostrap: ", mean(result1)))
  print(paste("The difference of the means: ", data.mean - mean(result1)))
  
}



#### simulation 19:
#### Different distributions (Normal or Poisson) 
#### with the same sample size 
#### by the same method 
#### with the same alpha

## NonPara.percentileMethod
# Normal datasets
# 1000 observations
# alpha = 0.02
simulation19 <- function(round = 10) {
  count.overall <- 0
  count.right <- 0
  count.left <- 0
  difference <- numeric(round)
  for(i in 1:round) {
    ## first, set a seed
    set.seed(123145)
    data.normal<-rnorm(1000,4,5)
    data.mean <- mean(data.normal)
    result1 <- NonPara.percentileMethod(data.normal, alpha = 0.02)
    if(data.mean>=result1[1] & data.mean<=result1[length(result1)]){
      count.overall <- count.overall + 1
    } else if (data.mean>=result1[1]) {
      count.right <- count.right + 1
    } else {
      count.left <- count.left + 1
    }
    difference[i] <- result1[length(result1)] - result1[1]
  }
  print(paste("The percentage that the boostrap CI contains the ture mean is: ", count.overall/round))
  print(paste("The percentage the ture mean is smaller than the CI: ", count.left/round))
  print(paste("The percentage the ture mean is bigger than the CI: ", count.right/round))
  print(paste("The mean of the length of the CI: ", mean(difference)))
  print(paste("The mean of the dataset: ", data.mean, " The mean of the boostrap: ", mean(result1)))
  print(paste("The difference of the means: ", data.mean - mean(result1)))
  
}


#### simulation 20:
#### Different distributions (Normal or Poisson) 
#### with the same sample size 
#### by the same method 
#### with the same alpha
## NonPara.percentileMethod
# Poisson datasets
# 1000 observations
# alpha = 0.02
simulation20 <- function(round = 10) {
  count.overall <- 0
  count.right <- 0
  count.left <- 0
  difference <- numeric(round)
  for(i in 1:round) {
    ## first, set a seed
    set.seed(123145)
    data.poisson<-rpois(1000,4)
    data.mean <- mean(data.poisson)
    result1 <- NonPara.percentileMethod(data.poisson,alpha = 0.02)
    if(data.mean>=result1[1]&data.mean<=result1[length(result1)]){
      count.overall <- count.overall + 1
    } else if (data.mean>=result1[1]) {
      count.right <- count.right + 1
    } else {
      count.left <- count.left + 1
    }
    difference[i] <- result1[length(result1)] - result1[1]
  }
  print(paste("The percentage that the boostrap CI contains the ture mean is: ", count.overall/round))
  print(paste("The percentage the ture mean is smaller than the CI: ", count.left/round))
  print(paste("The percentage the ture mean is bigger than the CI: ", count.right/round))
  print(paste("The mean of the length of the CI: ", mean(difference)))
  print(paste("The mean of the dataset: ", data.mean, " The mean of the boostrap: ", mean(result1)))
  print(paste("The difference of the means: ", data.mean - mean(result1)))
  
}

#### simulation 21:
#### Different distributions (Normal or Poisson) 
#### with the same sample size 
#### by the same method 
#### with the same alpha
## NonPara.BCaMethod
#  Normal datasets
# 1000 observations
# alpha = 0.02
simulation21 <- function(round = 10) {
  count.overall <- 0
  count.right <- 0
  count.left <- 0
  difference <- numeric(round)
  for(i in 1:round) {
    ## first, set a seed
    set.seed(123145)
    data.normal<-rnorm(1000,4)
    data.mean <- mean(data.normal)
    result1 <- NonPara.BCaMethod(data.normal,alpha = 0.02)
    if(data.mean>=result1[1]&data.mean<=result1[length(result1)]){
      count.overall <- count.overall + 1
    } else if (data.mean>=result1[1]) {
      count.right <- count.right + 1
    } else {
      count.left <- count.left + 1
    }
    difference[i] <- result1[length(result1)] - result1[1]
  }
  print(paste("The percentage that the boostrap CI contains the ture mean is: ", count.overall/round))
  print(paste("The percentage the ture mean is smaller than the CI: ", count.left/round))
  print(paste("The percentage the ture mean is bigger than the CI: ", count.right/round))
  print(paste("The mean of the length of the CI: ", mean(difference)))
  print(paste("The mean of the dataset: ", data.mean, " The mean of the boostrap: ", mean(result1)))
  print(paste("The difference of the means: ", data.mean - mean(result1)))
  
}

#### simulation 22:
#### Different distributions (Normal or Poisson) 
#### with the same sample size 
#### by the same method 
#### with the same alpha
## NonPara.BCaMethod
#  poisson datasets
# 1000 observations
# alpha = 0.02
simulation22 <- function(round = 10) {
  count.overall <- 0
  count.right <- 0
  count.left <- 0
  difference <- numeric(round)
  for(i in 1:round) {
    ## first, set a seed
    set.seed(123145)
    data.poisson<-rpois(1000,4)
    data.mean <- mean(data.poisson)
    result1 <- NonPara.BCaMethod(data.poisson,alpha = 0.02)
    if(data.mean>=result1[1]&data.mean<=result1[length(result1)]){
      count.overall <- count.overall + 1
    } else if (data.mean>=result1[1]) {
      count.right <- count.right + 1
    } else {
      count.left <- count.left + 1
    }
    difference[i] <- result1[length(result1)] - result1[1]
  }
  print(paste("The percentage that the boostrap CI contains the ture mean is: ", count.overall/round))
  print(paste("The percentage the ture mean is smaller than the CI: ", count.left/round))
  print(paste("The percentage the ture mean is bigger than the CI: ", count.right/round))
  print(paste("The mean of the length of the CI: ", mean(difference)))
  print(paste("The mean of the dataset: ", data.mean, " The mean of the boostrap: ", mean(result1)))
  print(paste("The difference of the means: ", data.mean - mean(result1)))
  
  
}



#### simulation 23:
#### Different distributions (Normal or Poisson) 
#### with the same sample size 
#### by the same method 
#### with the same alpha
## Para.percentileMethod
#  normal datasets
# 1000 observations
# alpha = 0.02
simulation23 <- function(round = 10) {
  count.overall <- 0
  count.right <- 0
  count.left <- 0
  difference <- numeric(round)
  for(i in 1:round) {
    ## first, set a seed
    set.seed(123145)
    data<-rnorm(1000,4)
    data.mean <- mean(data)
    result1 <- Para.percentileMethod(data,alpha = 0.02)
    if(data.mean>=result1[1]&data.mean<=result1[length(result1)]){
      count.overall <- count.overall + 1
    } else if (data.mean>=result1[1]) {
      count.right <- count.right + 1
    } else {
      count.left <- count.left + 1
    }
    difference[i] <- result1[length(result1)] - result1[1]
  }
  print(paste("The percentage that the boostrap CI contains the ture mean is: ", count.overall/round))
  print(paste("The percentage the ture mean is smaller than the CI: ", count.left/round))
  print(paste("The percentage the ture mean is bigger than the CI: ", count.right/round))
  print(paste("The mean of the length of the CI: ", mean(difference)))
  print(paste("The mean of the dataset: ", data.mean, " The mean of the boostrap: ", mean(result1)))
  print(paste("The difference of the means: ", data.mean - mean(result1)))
  
}

#### simulation 24:
#### Different distributions (Normal or Poisson) 
#### with the same sample size 
#### by the same method 
#### with the same alpha
## Para.percentileMethod
#  poisson datasets
# 1000 observations
# alpha = 0.02
simulation24 <- function(round = 10) {
  count.overall <- 0
  count.right <- 0
  count.left <- 0
  difference <- numeric(round)
  for(i in 1:round) {
    ## first, set a seed
    set.seed(123145)
    data<-rpois(1000,4)
    data.mean <- mean(data)
    result1 <- Para.percentileMethod(data,alpha = 0.02)
    if(data.mean>=result1[1]&data.mean<=result1[length(result1)]){
      count.overall <- count.overall + 1
    } else if (data.mean>=result1[1]) {
      count.right <- count.right + 1
    } else {
      count.left <- count.left + 1
    }
    difference[i] <- result1[length(result1)] - result1[1]
  }
  print(paste("The percentage that the boostrap CI contains the ture mean is: ", count.overall/round))
  print(paste("The percentage the ture mean is smaller than the CI: ", count.left/round))
  print(paste("The percentage the ture mean is bigger than the CI: ", count.right/round))
  print(paste("The mean of the length of the CI: ", mean(difference)))
  print(paste("The mean of the dataset: ", data.mean, " The mean of the boostrap: ", mean(result1)))
  print(paste("The difference of the means: ", data.mean - mean(result1)))
  
}

