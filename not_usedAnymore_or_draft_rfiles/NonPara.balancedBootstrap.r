## Non-parametric balanced bootstrap
## not finished
## input: B: the size of bootstraped sample 
# sets: every subset contains sets values

NonPara.balancedBootstrap <- function(dataset, alpha = 0.05, B = 1000, sets = 10) {
  if((B/sets)%%1 != 0) {
    stop("invalid argument")
  }
  n <- length(dataset) 
  N <- B/n
  # generate the bootstrap data set
  if(N < 1) {
    stop("invalid argument")
  } else {
    #### too complicated so I chose an easy way
    # N <- ceiling(N)
    # data.subset1 <- numeric(n/2)
    # data.subset2 <- numeric(n-length(data.subset1))
    # for(i in 1 : n) {
    #   if(i%%2 == 0) {
    #     data.subset1[i] <- dataset[i]
    #   } else {
    #     data.subset2[i] <- dataset[i]
    #   }
    # }
    # data.subset1 <- rep(data.subset1,(n/2)+1)
    # data.subset2 <- rep(data.subset2,n-(n/2)-1)
    # data.newset <- c(data.subset1,data.subset2)
    # data.newset <- sample(data.newset,B)
    # print(length(data.subset2))
    # print(length(data.subset1))
    #print(data.newset)
    data.newset <- sample(dataset,B,replace = TRUE)
    data.newset <- sample(data.newset,B)
    data.newset <- matrix(data.newset,ncol = sets, byrow = TRUE)
    boot.means.set <- apply(data.newset, 2, mean)
    boot.means.set <- sort(boot.means.set)
    a1 <- (1-alpha/2)  
    a2 <- alpha/2 
    boot.mean.CI <- getCI(boot.means.set, a1, a2)
  }
  
}