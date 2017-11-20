#### simulation
## first, set a seed
set.seed(123145)
## then source the r file containing bootstrap functions
# source('bootstrapFunction.r')
source('D:/MT4113/bootstrapFunction.r') # under Windows
################################################################
###############################################################
#### simulation 1:
#### Different distributions (Normal or Poisson) 
#### with the same sample size 
#### by the same method 
#### with the same alpha
dataset<-rnorm(100, 4, 5)
mean(dataset)
adsfdsfs <- NonPara.percentileMethod(dataset)
mean(adsfdsfs )

sdfsdf <- NonPara.BCaMethod(dataset)
mean(sdfsdf)

werwerw<-Para.percentileMethod(dataset)
mean(werwerw)

ewrdfs <- NonPara.balancedBootstrap(dataset)
mean(ewrdfs)


dataset2 <- rpois(100,5)
mean(dataset2)


lab1 <- NonPara.percentileMethod(dataset2)
mean(lab1)

lab2 <- NonPara.BCaMethod(dataset2)
mean(lab2)

werwersasdw<-Para.percentileMethod(dataset2, distribution = "poisson")
mean(werwersasdw)
