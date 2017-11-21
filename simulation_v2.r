#### simulation
## source the r file containing bootstrap functions
# source('bootstrapFunction.r')
source('D:/MT4113/bootstrapFunction.r') # under Windows
################################################################
###############################################################
#### simulation.v2:
## inputs:
# n: the sample size
# round: the loop number
# distribution: normal or poisson, it is only used for parametric bootstrap.
# The default value is normal distribution
# lambda: the mean used to generate the dataset
# sd: the sd of the normal distribution used to generate the dataset
# alpha: the confidence level
# B: resample times
simulation.v2 <- function(data, round = 10, distribution = "normal", lambda = 0, alpha = 0.05, B = 1000) {
  dataType <- checkArguments(data,round,distribution,lambda,alpha,B)
  differ1 <- numeric(round)
  differ2 <- numeric(round)
  differ3 <- numeric(round)
  count.overall.set<- numeric(3)
  count.right.set <- numeric(3)
  count.left.set <- numeric(3)
  
  
  method.set <- numeric(round)
  coverage.set <- numeric(round)
  smaller.set <- numeric(round)
  bigger.set <- numeric(round)
  bootmean.set <- numeric(round)
  n.set <- numeric(round)
  B.set <- rep(B,round)
  round.set <- rep(round,round)
  difference.set <- numeric(round)
  length.set <-numeric(round)
  distribution.set <- numeric(round)
  truemean.set <- rep(lambda,round)
  alpha.set <- rep(alpha, round)
  writeflag <- 1
  
  filepath <- "D:/MT4113/Info.txt"
    
  data.mean <- lambda
  
  
    for(i in 1:round) {
      result1 <- NonPara.percentileMethod(dataset = data, alpha = alpha, B = B)
    
      result2 <- NonPara.BCaMethod(dataset = data, alpha = alpha, B = B)
     
      if(dataType == 0) { # normal
        result3 <- Para.percentileMethod(dataset = data, alpha =  alpha, distribution = "normal", B = B)
      } else { # poisson
        result3 <- Para.percentileMethod(dataset = data, alpha = alpha, distribution = "poisson", B = B)
      }
  
      #classify the results
      c1 <- classify(result1, data.mean, count.overall.set[1], count.left.set[1], count.right.set[1])
      c2 <- classify(result2, data.mean, count.overall.set[2], count.left.set[2], count.right.set[2])
      c3 <- classify(result3, data.mean, count.overall.set[3], count.left.set[3], count.right.set[3])
      
      count.overall.set[1] <- c1[[1]]
      count.overall.set[2] <- c2[[1]]
      count.overall.set[3] <- c3[[1]]
      
      count.right.set[1] <- c1[[2]]
      count.right.set[2] <- c2[[2]]
      count.right.set[3] <- c3[[2]]
      
      count.left.set[1] <- c1[[3]]
      count.left.set[2] <- c2[[3]]
      count.left.set[3] <- c3[[3]]
      
      differ1[i] <- c1[[4]]
      differ2[i] <- c2[[4]]
      differ3[i] <- c3[[4]]
      
      # write the information into csv
      for(num in 1 : 3) {
        method.set[writeflag] <- num
        coverage.set[writeflag] <- count.overall.set[num]/round
        smaller.set[writeflag] <- count.left.set[num]/round
        bigger.set[writeflag]<- count.right.set[num]/round
        n.set[writeflag] <- length(data)
        if(num == 1) {
          bootmean.set[writeflag]<- mean(result1) 
          difference.set[writeflag] <- data.mean - mean(result1)
          length.set[writeflag] <- differ1[i]
        } else if (num ==2) {
          bootmean.set[writeflag] <- mean(result2)
          difference.set[writeflag] <- data.mean - mean(result2)
          length.set[writeflag] <- differ2[i]
        } else {
          bootmean.set[writeflag] <- mean(result3)
          difference.set[writeflag] <- data.mean -mean(result3)
          length.set[writeflag] <- differ3[i]
        }
        
        if(dataType ==0) {
          distribution.set[writeflag] <- "normal"
        } else {
          distribution.set[writeflag] <- "poisson"
        }

        writeflag <- writeflag + 1;
      }
      
    }
  
  
  info <- data.frame(method.set,coverage.set,smaller.set,bigger.set,truemean.set, bootmean.set, n.set,  B.set, round.set, difference.set, length.set, distribution.set, alpha.set)
  if(!file.exists(filepath)) {
    options(warn=-1) 
    write.table(info,file = filepath, append = TRUE, row.names = FALSE, sep = "\t")
  } else {
    write.table(info,file = filepath, append = TRUE, row.names = FALSE, sep = "\t", col.names=FALSE)
  }
  
  print("Bootstrap means----------------")
  print("result1---------NonPara.percentileMethod-------")
  print(result1)
  print("result2---------NonPara.BCaMethod-------")
  print(result2)
  print("result3---------Para.percentileMethod-------")
  print(result3)
  
  outputs(1,result1,data.mean,count.overall.set[1],count.left.set[1], count.right.set[1], round, differ1)
  
  outputs(2,result2,data.mean,count.overall.set[2],count.left.set[2], count.right.set[2], round, differ2)
  
  outputs(3,result3,data.mean,count.overall.set[3],count.left.set[3], count.right.set[3], round, differ3)
  
 
}


## check input arguments
## inputs:
# data: the data
# round: the loop number || integer, positive, cannot be Inf
# distribution: normal or poisson
# lambda: the mean used to generate the dataset
# sd: the sd of the normal distribution used to generate the dataset
# alpha: the confidence level
# B: resample times
checkArguments <- function(arg1, arg2, arg3, arg4, arg5, arg6) {
  if(is.numeric(arg1)==FALSE|| length(arg1) < 3  || abs(sum(arg1)) == Inf){
    stop("Invalid arguments1")
  }

  if(is.numeric(arg2)==FALSE|| arg2%%1 != 0  || arg2 < 1){
    stop("Invalid arguments2")
  }

  if(arg3 == "Poisson" || arg3 == "poisson" || arg3 == "POISSON") {
    flag <- 1
  } else if (arg3 == "Normal" || arg3 == "normal" || arg3 == "NORMAL") {
    flag <- 0
  } else {
    stop("Invalid arguments3")
  }
  
  if(is.numeric(arg4)==FALSE || length(arg4) != 1 || abs(arg4) == Inf){
    stop("Invalid arguments4")
  }


  if(is.numeric(arg5)==FALSE || length(arg5) != 1 || abs(arg5) == Inf || arg5 < 0 || arg5 > 1){
    stop("Invalid arguments5")
  }


  if(is.numeric(arg6)==FALSE || arg6%%1 != 0  || abs(arg6) == Inf || arg6 <= 0){
    stop("Invalid arguments6")
  }

  return(flag)
}

## give output of the simulation
# incluiding severl sentences and histograms
outputs <- function(type, bootmean, Tmean, count1, count2, count3, round, difference) {
  if(type == 1) {
    print("NonPara.percentileMethod--------------------------")
  } else if (type ==2) {
    print("NonPara.BCaMethod-----------------------")
  } else {
    print("Para.percentileMethod-------------------")
  }
  print(paste("The percentage that the boostrap CI contains the ture mean is: ", count1/round))
  print(paste("The percentage the ture mean is smaller than the CI: ", count2/round))
  print(paste("The percentage the ture mean is bigger than the CI: ", count3/round))
  print(paste("The mean of the length of the CI: ", mean(difference)))
  print(paste("The mean of the dataset: ", Tmean))
  print(paste("The mean of the boostrap: ", mean(bootmean)))
  print(paste("The difference of the means: ", Tmean - mean(bootmean)))
  
  #hist(bootmean)
  #abline(v = Tmean, col = "green")
  
}

# classify the bootstrap outcomes every round
classify <- function(result, truemean, count.overall, count.left, count.right) {
  if(length(result) != 0) {
    #classify the results
    if(truemean>=result[1] & truemean<=result[length(result)]){
      count.overall <- count.overall + 1
    } else if (truemean>=result[length(result)]) {
      count.right <- count.right + 1
    } else {
      count.left <- count.left + 1
    }
    
    difference <- result[1] - result[length(result)]
  } else{
    difference <- 0
  }
 
  outcome <- list(count.overall,count.right,count.left,difference)
  return(outcome)

}

