# I confirm that the attached is my own work, except where clearly indicated in the text.
#####################
## this file helps me to analysis results
## refer the end of planning doc.pdf

# filepath <- "D:/MT4113/Info.txt" # the path where I store results under Windows
filepath <- "Info.txt" # Linux's relative path

# read the results
boot.result <- read.table(file = filepath, header = TRUE)

# here are some definitions
# B1 : B =10
# B2 : B ==100
# B3: B ==1000
# round1: round = 10
# round2 : round = 100
# round3 : round = 1000

################################################################
#####simulation1#################################################
#### 

## analyse the different methods for n = 50, alpha = 0.05, and normal distribution
# here I will compare the different Bs or rounds' effects
sim1<- boot.result[which(boot.result$n.set == 50 & boot.result$distribution.set == "normal" & boot.result$alpha.set == 0.05),]

################################################################

##############B1, round1##########################
sim1.B1.round1 <- sim1[which(sim1$B.set == 10 & sim1$round.set == 10),]
sim1.nonPara.percentile.B1.round1  <- sim1.B1.round1[which(sim1$method.set == 1),]
sim1.nonPara.BCa.B1.round1 <- sim1.B1.round1[which(sim1$method.set == 2),]
sim1.Para.B1.round1 <- sim1.B1.round1[which(sim1$method.set == 3),]


print("Non-Parametric Percentile Method----------------")
summary(sim1.nonPara.percentile.B1.round1[c(2,3,4,5,6,10,11)])
print("Non-Parametric BCa------------------------------")
summary(sim1.nonPara.BCa.B1.round1[c(2,3,4,5,6,10,11)])
print("Parametric Percentile Method--------------------")
summary(sim1.Para.B1.round1[c(2,3,4,5,6,10,11)])

######summary results#################################
# > summary(sim1.nonPara.percentile.B1.round1[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :9.888   Min.   : 6.014   Min.   :-2.781   Min.   :-6.541  
#  1st Qu.:0.200   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.888   1st Qu.: 9.129   1st Qu.:-0.858   1st Qu.:-3.778  
#  Median :0.400   Median :0.000   Median :0.000   Median :9.888   Median :10.037   Median :-0.149   Median :-3.123  
#  Mean   :0.444   Mean   :0.043   Mean   :0.063   Mean   :9.888   Mean   : 9.880   Mean   : 0.008   Mean   :-3.204  
#  3rd Qu.:0.700   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:9.888   3rd Qu.:10.746   3rd Qu.: 0.759   3rd Qu.:-2.603  
#  Max.   :1.000   Max.   :1.000   Max.   :1.000   Max.   :9.888   Max.   :12.669   Max.   : 3.874   Max.   :-1.079  
#  NA's   :31000   NA's   :31000   NA's   :31000   NA's   :31000   NA's   :31000    NA's   :31000    NA's   :31000   
# > print("Non-Parametric BCa------------------------------")
# [1] "Non-Parametric BCa------------------------------"
# > summary(sim1.nonPara.BCa.B1.round1[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :9.888   Min.   : 5.820   Min.   :-2.661   Min.   :-5.971  
#  1st Qu.:0.200   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.888   1st Qu.: 9.149   1st Qu.:-0.955   1st Qu.:-3.467  
#  Median :0.400   Median :0.000   Median :0.000   Median :9.888   Median :10.093   Median :-0.205   Median :-2.845  
#  Mean   :0.406   Mean   :0.067   Mean   :0.076   Mean   :9.888   Mean   : 9.919   Mean   :-0.031   Mean   :-2.896  
#  3rd Qu.:0.600   3rd Qu.:0.100   3rd Qu.:0.000   3rd Qu.:9.888   3rd Qu.:10.843   3rd Qu.: 0.739   3rd Qu.:-2.286  
#  Max.   :1.000   Max.   :1.000   Max.   :1.000   Max.   :9.888   Max.   :12.549   Max.   : 4.068   Max.   :-0.156  
#  NA's   :31000   NA's   :31000   NA's   :31000   NA's   :31000   NA's   :31000    NA's   :31000    NA's   :31000   
# > print("Parametric Percentile Method--------------------")
# [1] "Parametric Percentile Method--------------------"
# > summary(sim1.Para.B1.round1[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :9.888   Min.   : 5.518   Min.   :-2.914   Min.   :-7.213  
#  1st Qu.:0.200   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.888   1st Qu.: 9.199   1st Qu.:-0.923   1st Qu.:-3.832  
#  Median :0.400   Median :0.000   Median :0.000   Median :9.888   Median :10.046   Median :-0.158   Median :-3.224  
#  Mean   :0.438   Mean   :0.049   Mean   :0.063   Mean   :9.888   Mean   : 9.921   Mean   :-0.033   Mean   :-3.275  
#  3rd Qu.:0.700   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:9.888   3rd Qu.:10.811   3rd Qu.: 0.689   3rd Qu.:-2.659  
#  Max.   :1.000   Max.   :1.000   Max.   :1.000   Max.   :9.888   Max.   :12.802   Max.   : 4.370   Max.   :-1.105  
#  NA's   :31000   NA's   :31000   NA's   :31000   NA's   :31000   NA's   :31000    NA's   :31000    NA's   :31000  

####hist no obvious difference#####################################
par(mfrow= c(3,1))
hist(sim1.nonPara.percentile.B1.round1$coverage.set)
hist(sim1.nonPara.BCa.B1.round1$coverage.set)
hist(sim1.Para.B1.round1$coverage.set)

######summary conlclusions######################
# 1. coverage m1 and m3 is better 
# 2. not coveraged mean in m1 and m3 usually bigger than the CIs. m2 is balanced
# 3. m1 bootmean is the best. no obvious difference in m1 and m3
# 4. m2's cis' lenght is the smallest


################################################################

############### B1, round2#######################
sim1.B1.round2 <- sim1[which(sim1$B.set == 10 & sim1$round.set == 100),]
sim1.nonPara.percentile.B1.round2  <- sim1.B1.round2[which(sim1$method.set == 1),]
sim1.nonPara.BCa.B1.round2 <- sim1.B1.round2[which(sim1$method.set == 2),]
sim1.Para.B1.round2 <- sim1.B1.round2[which(sim1$method.set == 3),]

print("Non-Parametric Percentile Method----------------")
summary(sim1.nonPara.percentile.B1.round2[c(2,3,4,5,6,10,11)])
print("Non-Parametric BCa------------------------------")
summary(sim1.nonPara.BCa.B1.round2[c(2,3,4,5,6,10,11)])
print("Parametric Percentile Method--------------------")
summary(sim1.Para.B1.round2[c(2,3,4,5,6,10,11)])

######summary results##################################
# > print("Non-Parametric Percentile Method----------------")
# [1] "Non-Parametric Percentile Method----------------"
# > summary(sim1.nonPara.percentile.B1.round2[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :9.888   Min.   : 5.456   Min.   :-3.171   Min.   :-7.534  
#  1st Qu.:0.150   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.888   1st Qu.: 9.146   1st Qu.:-0.931   1st Qu.:-3.724  
#  Median :0.360   Median :0.000   Median :0.000   Median :9.888   Median :10.082   Median :-0.194   Median :-3.132  
#  Mean   :0.394   Mean   :0.052   Mean   :0.059   Mean   :9.888   Mean   : 9.913   Mean   :-0.025   Mean   :-3.194  
#  3rd Qu.:0.610   3rd Qu.:0.050   3rd Qu.:0.020   3rd Qu.:9.888   3rd Qu.:10.819   3rd Qu.: 0.742   3rd Qu.:-2.575  
#  Max.   :1.000   Max.   :0.870   Max.   :1.000   Max.   :9.888   Max.   :13.059   Max.   : 4.432   Max.   :-0.863  
#  NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000    NA's   :22000   
# > print("Non-Parametric BCa------------------------------")
# [1] "Non-Parametric BCa------------------------------"
# > summary(sim1.nonPara.BCa.B1.round2[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.00    Min.   :0.000   Min.   :0.000   Min.   :9.888   Min.   : 5.648   Min.   :-2.834   Min.   :-7.066  
#  1st Qu.:0.13    1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.888   1st Qu.: 9.180   1st Qu.:-0.930   1st Qu.:-3.493  
#  Median :0.33    Median :0.000   Median :0.000   Median :9.888   Median :10.077   Median :-0.189   Median :-2.871  
#  Mean   :0.37    Mean   :0.065   Mean   :0.069   Mean   :9.888   Mean   : 9.913   Mean   :-0.025   Mean   :-2.912  
#  3rd Qu.:0.58    3rd Qu.:0.070   3rd Qu.:0.030   3rd Qu.:9.888   3rd Qu.:10.818   3rd Qu.: 0.708   3rd Qu.:-2.299  
#  Max.   :1.00    Max.   :0.950   Max.   :1.000   Max.   :9.888   Max.   :12.722   Max.   : 4.240   Max.   : 0.000  
#  NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22022    NA's   :22022    NA's   :22000   
# > print("Parametric Percentile Method--------------------")
# [1] "Parametric Percentile Method--------------------"
# > summary(sim1.Para.B1.round2[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :9.888   Min.   : 5.539   Min.   :-3.032   Min.   :-8.263  
#  1st Qu.:0.150   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.888   1st Qu.: 9.140   1st Qu.:-0.940   1st Qu.:-3.771  
#  Median :0.360   Median :0.000   Median :0.000   Median :9.888   Median :10.078   Median :-0.190   Median :-3.159  
#  Mean   :0.396   Mean   :0.051   Mean   :0.058   Mean   :9.888   Mean   : 9.917   Mean   :-0.029   Mean   :-3.228  
#  3rd Qu.:0.610   3rd Qu.:0.040   3rd Qu.:0.020   3rd Qu.:9.888   3rd Qu.:10.828   3rd Qu.: 0.748   3rd Qu.:-2.594  
#  Max.   :1.000   Max.   :0.930   Max.   :1.000   Max.   :9.888   Max.   :12.920   Max.   : 4.349   Max.   :-0.776  
#  NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000    NA's   :22000   

####hist m1 and m3 are better ###################

hist(sim1.nonPara.percentile.B1.round2$coverage.set)
hist(sim1.nonPara.BCa.B1.round2$coverage.set)
hist(sim1.Para.B1.round2$coverage.set)




######summary conlclusions########################
# 1. overall performance worse than B1 round1
# 2. m1, m2, m3 's not coverager are all balanced
# 3. coverage m1 and m3 is better 
# 4. no obvious difference of boot means
# 5. m2's cis' lenght is the smallest

################################################################

######################### B2, round1 #########################
sim1.B2.round1 <- sim1[which(sim1$B.set == 100 & sim1$round.set == 10),]
sim1.nonPara.percentile.B2.round1  <- sim1.B2.round1[which(sim1$method.set == 1),]
sim1.nonPara.BCa.B2.round1 <- sim1.B2.round1[which(sim1$method.set == 2),]
sim1.Para.B2.round1 <- sim1.B2.round1[which(sim1$method.set == 3),]

print("Non-Parametric Percentile Method----------------")
summary(sim1.nonPara.percentile.B2.round1[c(2,3,4,5,6,10,11)])
print("Non-Parametric BCa------------------------------")
summary(sim1.nonPara.BCa.B2.round1[c(2,3,4,5,6,10,11)])
print("Parametric Percentile Method--------------------")
summary(sim1.Para.B2.round1[c(2,3,4,5,6,10,11)])

######summary results#######################
# > print("Non-Parametric Percentile Method----------------")
# [1] "Non-Parametric Percentile Method----------------"
# > summary(sim1.nonPara.percentile.B2.round1[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :9.888   Min.   : 6.131   Min.   :-2.496   Min.   :-6.218  
#  1st Qu.:0.300   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.888   1st Qu.: 9.196   1st Qu.:-0.922   1st Qu.:-4.629  
#  Median :0.500   Median :0.000   Median :0.000   Median :9.888   Median :10.077   Median :-0.189   Median :-4.238  
#  Mean   :0.517   Mean   :0.007   Mean   :0.026   Mean   :9.888   Mean   : 9.914   Mean   :-0.026   Mean   :-4.254  
#  3rd Qu.:0.800   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:9.888   3rd Qu.:10.810   3rd Qu.: 0.692   3rd Qu.:-3.833  
#  Max.   :1.000   Max.   :1.000   Max.   :1.000   Max.   :9.888   Max.   :12.384   Max.   : 3.757   Max.   :-2.731  
#  NA's   :31000   NA's   :31000   NA's   :31000   NA's   :31000   NA's   :31000    NA's   :31000    NA's   :31000   
# > print("Non-Parametric BCa------------------------------")
# [1] "Non-Parametric BCa------------------------------"
# > summary(sim1.nonPara.BCa.B2.round1[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :9.888   Min.   : 6.072   Min.   :-2.350   Min.   :-7.083  
#  1st Qu.:0.200   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.888   1st Qu.: 9.217   1st Qu.:-0.945   1st Qu.:-4.576  
#  Median :0.500   Median :0.000   Median :0.000   Median :9.888   Median :10.048   Median :-0.160   Median :-4.199  
#  Mean   :0.511   Mean   :0.005   Mean   :0.033   Mean   :9.888   Mean   : 9.916   Mean   :-0.028   Mean   :-4.225  
#  3rd Qu.:0.800   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:9.888   3rd Qu.:10.833   3rd Qu.: 0.671   3rd Qu.:-3.823  
#  Max.   :1.000   Max.   :0.900   Max.   :1.000   Max.   :9.888   Max.   :12.238   Max.   : 3.816   Max.   :-2.797  
#  NA's   :31000   NA's   :31000   NA's   :31000   NA's   :31000   NA's   :31000    NA's   :31000    NA's   :31000   
# > print("Parametric Percentile Method--------------------")
# [1] "Parametric Percentile Method--------------------"
# > summary(sim1.Para.B2.round1[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.000   Min.   :0.000   Min.   :0.00    Min.   :9.888   Min.   : 6.125   Min.   :-2.621   Min.   :-6.591  
#  1st Qu.:0.200   1st Qu.:0.000   1st Qu.:0.00    1st Qu.:9.888   1st Qu.: 9.195   1st Qu.:-0.936   1st Qu.:-4.659  
#  Median :0.500   Median :0.000   Median :0.00    Median :9.888   Median :10.065   Median :-0.177   Median :-4.262  
#  Mean   :0.514   Mean   :0.006   Mean   :0.03    Mean   :9.888   Mean   : 9.907   Mean   :-0.019   Mean   :-4.285  
#  3rd Qu.:0.800   3rd Qu.:0.000   3rd Qu.:0.00    3rd Qu.:9.888   3rd Qu.:10.824   3rd Qu.: 0.693   3rd Qu.:-3.854  
#  Max.   :1.000   Max.   :0.900   Max.   :1.00    Max.   :9.888   Max.   :12.509   Max.   : 3.763   Max.   :-2.842  
#  NA's   :31000   NA's   :31000   NA's   :31000   NA's   :31000   NA's   :31000    NA's   :31000    NA's   :31000   
# > 

####hist on obvious difference############################################
hist(sim1.nonPara.percentile.B2.round1$coverage.set)
abline(h = c(80,85,90,95,100), col = "green")
hist(sim1.nonPara.BCa.B2.round1$coverage.set)
abline(h = c(80,85,90,95,100), col = "green")
hist(sim1.Para.B2.round1$coverage.set)
abline(h = c(80,85,90,95,100), col = "green")
######summary conlclusion####################
# 1. B increases, all coverage performance are better than B = 10 even round = 10
# 2. no obvious difference for coverage
# 3. all mehtods' not coveraged means tend to be bigger than the CIs
# 4. boot mean no obvious difference
# 5. ci lenght no obvious diff
################################################################


####################### B2, round2#################
sim1.B2.round2 <- sim1[which(sim1$B.set == 100 & sim1$round.set == 100),]
sim1.nonPara.percentile.B2.round2  <- sim1.B2.round2[which(sim1$method.set == 1),]
sim1.nonPara.BCa.B2.round2 <- sim1.B2.round2[which(sim1$method.set == 2),]
sim1.Para.B2.round2 <- sim1.B2.round2[which(sim1$method.set == 3),]

print("Non-Parametric Percentile Method----------------")
summary(sim1.nonPara.percentile.B2.round2[c(2,3,4,5,6,10,11)])
print("Non-Parametric BCa------------------------------")
summary(sim1.nonPara.BCa.B2.round2[c(2,3,4,5,6,10,11)])
print("Parametric Percentile Method--------------------")
summary(sim1.Para.B2.round2[c(2,3,4,5,6,10,11)])

######summary results########################
# > print("Non-Parametric Percentile Method----------------")
# [1] "Non-Parametric Percentile Method----------------"
# > summary(sim1.nonPara.percentile.B2.round2[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :9.888   Min.   : 5.990   Min.   :-2.670   Min.   :-6.549  
#  1st Qu.:0.210   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.888   1st Qu.: 9.194   1st Qu.:-0.943   1st Qu.:-4.620  
#  Median :0.460   Median :0.000   Median :0.000   Median :9.888   Median :10.061   Median :-0.173   Median :-4.212  
#  Mean   :0.472   Mean   :0.006   Mean   :0.026   Mean   :9.888   Mean   : 9.914   Mean   :-0.026   Mean   :-4.240  
#  3rd Qu.:0.730   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:9.888   3rd Qu.:10.831   3rd Qu.: 0.694   3rd Qu.:-3.835  
#  Max.   :1.000   Max.   :0.920   Max.   :1.000   Max.   :9.888   Max.   :12.558   Max.   : 3.898   Max.   :-2.435  
#  NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000    NA's   :22000   
# > print("Non-Parametric BCa------------------------------")
# [1] "Non-Parametric BCa------------------------------"
# > summary(sim1.nonPara.BCa.B2.round2[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :9.888   Min.   : 6.064   Min.   :-2.520   Min.   :-6.672  
#  1st Qu.:0.210   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.888   1st Qu.: 9.201   1st Qu.:-0.954   1st Qu.:-4.612  
#  Median :0.460   Median :0.000   Median :0.000   Median :9.888   Median :10.057   Median :-0.169   Median :-4.193  
#  Mean   :0.469   Mean   :0.008   Mean   :0.028   Mean   :9.888   Mean   : 9.915   Mean   :-0.027   Mean   :-4.227  
#  3rd Qu.:0.720   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:9.888   3rd Qu.:10.842   3rd Qu.: 0.687   3rd Qu.:-3.808  
#  Max.   :1.000   Max.   :0.880   Max.   :1.000   Max.   :9.888   Max.   :12.408   Max.   : 3.824   Max.   :-2.395  
#  NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000    NA's   :22000   
# > print("Parametric Percentile Method--------------------")
# [1] "Parametric Percentile Method--------------------"
# > summary(sim1.Para.B2.round2[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :9.888   Min.   : 6.003   Min.   :-2.664   Min.   :-6.884  
#  1st Qu.:0.210   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.888   1st Qu.: 9.202   1st Qu.:-0.936   1st Qu.:-4.655  
#  Median :0.460   Median :0.000   Median :0.000   Median :9.888   Median :10.073   Median :-0.185   Median :-4.252  
#  Mean   :0.472   Mean   :0.006   Mean   :0.027   Mean   :9.888   Mean   : 9.914   Mean   :-0.026   Mean   :-4.280  
#  3rd Qu.:0.730   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:9.888   3rd Qu.:10.824   3rd Qu.: 0.686   3rd Qu.:-3.869  
#  Max.   :1.000   Max.   :0.870   Max.   :1.000   Max.   :9.888   Max.   :12.552   Max.   : 3.885   Max.   :-2.556  
#  NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000    NA's   :22000   
# > 
####hist non-para seems better than para ############################
hist(sim1.nonPara.percentile.B2.round2$coverage.set)
abline(h = c(500,600,700), col = "green")
hist(sim1.nonPara.BCa.B2.round2$coverage.set)
abline(h = c(500,600,700), col = "green")
hist(sim1.Para.B2.round2$coverage.set)
abline(h = c(500,600,700), col = "green")
######summary conlclusion####################
# 1. round increases, the coverage seems not better than b = 10, round = 10
# 2. all mehtods' not coveraged means tend to be bigger than the CIs
# 3. no obvious difference for coverage
# 4. boot mean no obvious difference
# 5. ci lenght no obvious diff

################################################################


#################### B3, round3#####################
sim1.B3.round3 <- sim1[which(sim1$B.set == 1000 & sim1$round.set == 1000),]
sim1.nonPara.percentile.B3.round3  <- sim1.B3.round3[which(sim1$method.set == 1),]
sim1.nonPara.BCa.B3.round3 <- sim1.B3.round3[which(sim1$method.set == 2),]
sim1.Para.B3.round3 <- sim1.B3.round3[which(sim1$method.set == 3),]

print("Non-Parametric Percentile Method----------------")
summary(sim1.nonPara.percentile.B3.round3[c(2,3,4,5,6,10,11)])
print("Non-Parametric BCa------------------------------")
summary(sim1.nonPara.BCa.B3.round3[c(2,3,4,5,6,10,11)])
print("Parametric Percentile Method--------------------")
summary(sim1.Para.B3.round3[c(2,3,4,5,6,10,11)])
######summary results###################
# > print("Non-Parametric Percentile Method----------------")
# [1] "Non-Parametric Percentile Method----------------"
# > summary(sim1.nonPara.percentile.B3.round3[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set   difference.set    length.set    
#  Min.   :0.001   Min.   :0       Min.   :0       Min.   :9.888   Min.   :8.796   Min.   :0.786   Min.   :-6.080  
#  1st Qu.:0.251   1st Qu.:0       1st Qu.:0       1st Qu.:9.888   1st Qu.:8.929   1st Qu.:0.901   1st Qu.:-5.531  
#  Median :0.500   Median :0       Median :0       Median :9.888   Median :8.958   Median :0.930   Median :-5.422  
#  Mean   :0.500   Mean   :0       Mean   :0       Mean   :9.888   Mean   :8.958   Mean   :0.930   Mean   :-5.424  
#  3rd Qu.:0.750   3rd Qu.:0       3rd Qu.:0       3rd Qu.:9.888   3rd Qu.:8.987   3rd Qu.:0.959   3rd Qu.:-5.314  
#  Max.   :1.000   Max.   :0       Max.   :0       Max.   :9.888   Max.   :9.102   Max.   :1.092   Max.   :-4.838  
#  NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   
# > print("Non-Parametric BCa------------------------------")
# [1] "Non-Parametric BCa------------------------------"
# > summary(sim1.nonPara.BCa.B3.round3[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set   difference.set    length.set    
#  Min.   :0.001   Min.   :0       Min.   :0       Min.   :9.888   Min.   :8.839   Min.   :0.829   Min.   :-6.027  
#  1st Qu.:0.251   1st Qu.:0       1st Qu.:0       1st Qu.:9.888   1st Qu.:8.933   1st Qu.:0.916   1st Qu.:-5.536  
#  Median :0.500   Median :0       Median :0       Median :9.888   Median :8.952   Median :0.936   Median :-5.424  
#  Mean   :0.500   Mean   :0       Mean   :0       Mean   :9.888   Mean   :8.952   Mean   :0.936   Mean   :-5.428  
#  3rd Qu.:0.750   3rd Qu.:0       3rd Qu.:0       3rd Qu.:9.888   3rd Qu.:8.972   3rd Qu.:0.955   3rd Qu.:-5.317  
#  Max.   :1.000   Max.   :0       Max.   :0       Max.   :9.888   Max.   :9.059   Max.   :1.049   Max.   :-4.801  
#  NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   
# > print("Parametric Percentile Method--------------------")
# [1] "Parametric Percentile Method--------------------"
# > summary(sim1.Para.B3.round3[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set   difference.set    length.set    
#  Min.   :0.001   Min.   :0       Min.   :0       Min.   :9.888   Min.   :8.799   Min.   :0.765   Min.   :-6.158  
#  1st Qu.:0.251   1st Qu.:0       1st Qu.:0       1st Qu.:9.888   1st Qu.:8.925   1st Qu.:0.903   1st Qu.:-5.590  
#  Median :0.500   Median :0       Median :0       Median :9.888   Median :8.954   Median :0.934   Median :-5.482  
#  Mean   :0.500   Mean   :0       Mean   :0       Mean   :9.888   Mean   :8.955   Mean   :0.933   Mean   :-5.482  
#  3rd Qu.:0.750   3rd Qu.:0       3rd Qu.:0       3rd Qu.:9.888   3rd Qu.:8.985   3rd Qu.:0.963   3rd Qu.:-5.369  
#  Max.   :1.000   Max.   :0       Max.   :0       Max.   :9.888   Max.   :9.123   Max.   :1.089   Max.   :-4.907  
#  NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   
######hist m1 has the lowest not-coverage rate, m2 and m3 no obvious difference######################
hist(sim1.nonPara.percentile.B3.round3$coverage.set)
hist(sim1.nonPara.BCa.B3.round3$coverage.set)
hist(sim1.Para.B3.round3$coverage.set)
######summary conlclusion###################
# 1. coverage becomes better, 0.5, no obvious difference between methods
# 2. all mehtods' not coveraged means tend to be bigger than the CIs
# 3. boot mean no obvious difference
# 4. ci lenght no obvious diff

################################################################

################################################################
####simulation2##################################################
#### 
## analyse the different methods for n = 50, alpha = 0.05, and poisson distribution
# here I will compare the different Bs or rounds' effects
sim2<- boot.result[which(boot.result$n.set == 50 & boot.result$distribution.set == "poisson" & boot.result$alpha.set == 0.05),]
################################################################

################ B1, round1##########################
sim2.B1.round1 <- sim2[which(sim2$B.set == 10 & sim2$round.set == 10),]
sim2.nonPara.percentile.B1.round1  <- sim2.B1.round1[which(sim2$method.set == 1),]
sim2.nonPara.BCa.B1.round1 <- sim2.B1.round1[which(sim2$method.set == 2),]
sim2.Para.B1.round1 <- sim2.B1.round1[which(sim2$method.set == 3),]

print("Non-Parametric Percentile Method----------------")
summary(sim2.nonPara.percentile.B1.round1[c(2,3,4,5,6,10,11)])
print("Non-Parametric BCa------------------------------")
summary(sim2.nonPara.BCa.B1.round1[c(2,3,4,5,6,10,11)])
print("Parametric Percentile Method--------------------")
summary(sim2.Para.B1.round1[c(2,3,4,5,6,10,11)])
######summary results##########################
# > print("Non-Parametric Percentile Method----------------")
# [1] "Non-Parametric Percentile Method----------------"
# > summary(sim2.nonPara.percentile.B1.round1[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :9.8     Min.   : 8.520   Min.   :-1.334   Min.   :-2.592  
#  1st Qu.:0.200   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.8     1st Qu.: 9.477   1st Qu.:-0.283   1st Qu.:-1.484  
#  Median :0.400   Median :0.000   Median :0.000   Median :9.8     Median : 9.783   Median : 0.017   Median :-1.240  
#  Mean   :0.436   Mean   :0.051   Mean   :0.063   Mean   :9.8     Mean   : 9.787   Mean   : 0.013   Mean   :-1.258  
#  3rd Qu.:0.700   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:9.8     3rd Qu.:10.083   3rd Qu.: 0.323   3rd Qu.:-0.995  
#  Max.   :1.000   Max.   :0.900   Max.   :1.000   Max.   :9.8     Max.   :11.134   Max.   : 1.280   Max.   :-0.282  
#  NA's   :31000   NA's   :31000   NA's   :31000   NA's   :31000   NA's   :31000    NA's   :31000    NA's   :31000   
# > print("Non-Parametric BCa------------------------------")
# [1] "Non-Parametric BCa------------------------------"
# > summary(sim2.nonPara.BCa.B1.round1[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :9.8     Min.   : 8.597   Min.   :-1.213   Min.   :-2.431  
#  1st Qu.:0.200   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.8     1st Qu.: 9.483   1st Qu.:-0.279   1st Qu.:-1.384  
#  Median :0.400   Median :0.000   Median :0.000   Median :9.8     Median : 9.795   Median : 0.005   Median :-1.113  
#  Mean   :0.418   Mean   :0.057   Mean   :0.076   Mean   :9.8     Mean   : 9.786   Mean   : 0.014   Mean   :-1.141  
#  3rd Qu.:0.700   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:9.8     3rd Qu.:10.079   3rd Qu.: 0.317   3rd Qu.:-0.876  
#  Max.   :1.000   Max.   :0.900   Max.   :1.000   Max.   :9.8     Max.   :11.013   Max.   : 1.203   Max.   : 0.000  
#  NA's   :31000   NA's   :31000   NA's   :31000   NA's   :31000   NA's   :31001    NA's   :31001    NA's   :31000   
# > print("Parametric Percentile Method--------------------")
# [1] "Parametric Percentile Method--------------------"
# > summary(sim2.Para.B1.round1[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :9.8     Min.   : 8.541   Min.   :-1.253   Min.   :-2.482  
#  1st Qu.:0.200   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.8     1st Qu.: 9.467   1st Qu.:-0.283   1st Qu.:-1.458  
#  Median :0.400   Median :0.000   Median :0.000   Median :9.8     Median : 9.770   Median : 0.030   Median :-1.254  
#  Mean   :0.428   Mean   :0.052   Mean   :0.069   Mean   :9.8     Mean   : 9.780   Mean   : 0.020   Mean   :-1.267  
#  3rd Qu.:0.700   3rd Qu.:0.000   3rd Qu.:0.100   3rd Qu.:9.8     3rd Qu.:10.083   3rd Qu.: 0.333   3rd Qu.:-1.041  
#  Max.   :1.000   Max.   :1.000   Max.   :1.000   Max.   :9.8     Max.   :11.053   Max.   : 1.259   Max.   :-0.442  
#  NA's   :31000   NA's   :31000   NA's   :31000   NA's   :31000   NA's   :31000    NA's   :31000    NA's   :31000   
######hist m1 has the lowest not-coverage rate, m2 and m3 no obvious difference##########################
hist(sim2.nonPara.percentile.B1.round1$coverage.set)
hist(sim2.nonPara.BCa.B1.round1$coverage.set)
hist(sim2.Para.B1.round1$coverage.set)
######summary conlclusion###################
# 1. coverage mean 0.4, no obvious difference between methods
# 2. all mehtods' not coveraged means tend to be balanced
# 3. boot mean m1 and m2 better
# 4. ci lenght m2 better

################################################################


######################## B1, round2#####################
sim2.B1.round2 <- sim2[which(sim2$B.set == 10 & sim2$round.set == 100),]
sim2.nonPara.percentile.B1.round2  <- sim2.B1.round2[which(sim2$method.set == 1),]
sim2.nonPara.BCa.B1.round2 <- sim2.B1.round2[which(sim2$method.set == 2),]
sim2.Para.B1.round2 <- sim2.B1.round2[which(sim2$method.set == 3),]

print("Non-Parametric Percentile Method----------------")
summary(sim2.nonPara.percentile.B1.round2[c(2,3,4,5,6,10,11)])
print("Non-Parametric BCa------------------------------")
summary(sim2.nonPara.BCa.B1.round2[c(2,3,4,5,6,10,11)])
print("Parametric Percentile Method--------------------")
summary(sim2.Para.B1.round2[c(2,3,4,5,6,10,11)])

######summary results#####################
# > print("Non-Parametric Percentile Method----------------")
# [1] "Non-Parametric Percentile Method----------------"
# > summary(sim2.nonPara.percentile.B1.round2[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :9.8     Min.   : 8.382   Min.   :-1.380   Min.   :-3.091  
#  1st Qu.:0.140   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.8     1st Qu.: 9.481   1st Qu.:-0.291   1st Qu.:-1.456  
#  Median :0.350   Median :0.000   Median :0.000   Median :9.8     Median : 9.792   Median : 0.008   Median :-1.212  
#  Mean   :0.391   Mean   :0.053   Mean   :0.062   Mean   :9.8     Mean   : 9.789   Mean   : 0.011   Mean   :-1.240  
#  3rd Qu.:0.620   3rd Qu.:0.020   3rd Qu.:0.020   3rd Qu.:9.8     3rd Qu.:10.091   3rd Qu.: 0.319   3rd Qu.:-0.995  
#  Max.   :1.000   Max.   :0.860   Max.   :1.000   Max.   :9.8     Max.   :11.180   Max.   : 1.418   Max.   :-0.273  
#  NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000    NA's   :22000   
# > print("Non-Parametric BCa------------------------------")
# [1] "Non-Parametric BCa------------------------------"
# > summary(sim2.nonPara.BCa.B1.round2[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :9.8     Min.   : 8.448   Min.   :-1.299   Min.   :-3.312  
#  1st Qu.:0.130   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.8     1st Qu.: 9.492   1st Qu.:-0.278   1st Qu.:-1.373  
#  Median :0.320   Median :0.000   Median :0.000   Median :9.8     Median : 9.795   Median : 0.005   Median :-1.115  
#  Mean   :0.372   Mean   :0.057   Mean   :0.074   Mean   :9.8     Mean   : 9.786   Mean   : 0.014   Mean   :-1.133  
#  3rd Qu.:0.590   3rd Qu.:0.020   3rd Qu.:0.050   3rd Qu.:9.8     3rd Qu.:10.078   3rd Qu.: 0.308   3rd Qu.:-0.878  
#  Max.   :1.000   Max.   :0.890   Max.   :1.000   Max.   :9.8     Max.   :11.099   Max.   : 1.352   Max.   : 0.000  
#  NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22022    NA's   :22022    NA's   :22000   
# > print("Parametric Percentile Method--------------------")
# [1] "Parametric Percentile Method--------------------"
# > summary(sim2.Para.B1.round2[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.000   Min.   :0.00    Min.   :0.000   Min.   :9.8     Min.   : 8.443   Min.   :-1.322   Min.   :-2.682  
#  1st Qu.:0.150   1st Qu.:0.00    1st Qu.:0.000   1st Qu.:9.8     1st Qu.: 9.487   1st Qu.:-0.284   1st Qu.:-1.450  
#  Median :0.360   Median :0.00    Median :0.000   Median :9.8     Median : 9.792   Median : 0.008   Median :-1.235  
#  Mean   :0.399   Mean   :0.05    Mean   :0.056   Mean   :9.8     Mean   : 9.791   Mean   : 0.009   Mean   :-1.254  
#  3rd Qu.:0.620   3rd Qu.:0.02    3rd Qu.:0.030   3rd Qu.:9.8     3rd Qu.:10.084   3rd Qu.: 0.313   3rd Qu.:-1.037  
#  Max.   :1.000   Max.   :0.88    Max.   :0.960   Max.   :9.8     Max.   :11.122   Max.   : 1.357   Max.   :-0.338  
#  NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000    NA's   :22000   
# > 
########hist m2 has the highest not-covaerage rate#########################
hist(sim2.nonPara.percentile.B1.round2$coverage.set)
hist(sim2.nonPara.BCa.B1.round2$coverage.set)
hist(sim2.Para.B1.round2$coverage.set)
######summary conlclusion
######summary conlclusion###################
# 1. m1 and m3 coverage mean 0.4, m2 0.37
# 2. all mehtods' not coveraged means tend to be balanced
# 3. boot mean m3 the best
# 4. ci lenght m2 better

################################################################


################ B2, round1#######################
sim2.B2.round1 <- sim2[which(sim2$B.set == 100 & sim2$round.set == 10),]
sim2.nonPara.percentile.B2.round1  <- sim2.B2.round1[which(sim2$method.set == 1),]
sim2.nonPara.BCa.B2.round1 <- sim2.B2.round1[which(sim2$method.set == 2),]
sim2.Para.B2.round1 <- sim2.B2.round1[which(sim2$method.set == 3),]

print("Non-Parametric Percentile Method----------------")
summary(sim2.nonPara.percentile.B2.round1[c(2,3,4,5,6,10,11)])
print("Non-Parametric BCa------------------------------")
summary(sim2.nonPara.BCa.B2.round1[c(2,3,4,5,6,10,11)])
print("Parametric Percentile Method--------------------")
summary(sim2.Para.B2.round1[c(2,3,4,5,6,10,11)])
######summary results#####################
# > print("Non-Parametric Percentile Method----------------")
# [1] "Non-Parametric Percentile Method----------------"
# > summary(sim2.nonPara.percentile.B2.round1[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.000   Min.   :0.000   Min.   :0.00    Min.   :9.8     Min.   : 8.667   Min.   :-1.093   Min.   :-2.762  
#  1st Qu.:0.200   1st Qu.:0.000   1st Qu.:0.00    1st Qu.:9.8     1st Qu.: 9.524   1st Qu.:-0.286   1st Qu.:-1.801  
#  Median :0.500   Median :0.000   Median :0.00    Median :9.8     Median : 9.805   Median :-0.005   Median :-1.643  
#  Mean   :0.504   Mean   :0.016   Mean   :0.03    Mean   :9.8     Mean   : 9.792   Mean   : 0.008   Mean   :-1.649  
#  3rd Qu.:0.800   3rd Qu.:0.000   3rd Qu.:0.00    3rd Qu.:9.8     3rd Qu.:10.086   3rd Qu.: 0.276   3rd Qu.:-1.489  
#  Max.   :1.000   Max.   :0.800   Max.   :1.00    Max.   :9.8     Max.   :10.893   Max.   : 1.133   Max.   :-0.801  
#  NA's   :31000   NA's   :31000   NA's   :31000   NA's   :31000   NA's   :31000    NA's   :31000    NA's   :31000   
# > print("Non-Parametric BCa------------------------------")
# [1] "Non-Parametric BCa------------------------------"
# > summary(sim2.nonPara.BCa.B2.round1[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.0     Min.   :0.000   Min.   :0.000   Min.   :9.8     Min.   : 8.696   Min.   :-1.030   Min.   :-2.530  
#  1st Qu.:0.2     1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.8     1st Qu.: 9.516   1st Qu.:-0.279   1st Qu.:-1.783  
#  Median :0.5     Median :0.000   Median :0.000   Median :9.8     Median : 9.805   Median :-0.005   Median :-1.628  
#  Mean   :0.5     Mean   :0.017   Mean   :0.034   Mean   :9.8     Mean   : 9.785   Mean   : 0.015   Mean   :-1.629  
#  3rd Qu.:0.8     3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:9.8     3rd Qu.:10.079   3rd Qu.: 0.284   3rd Qu.:-1.472  
#  Max.   :1.0     Max.   :0.700   Max.   :1.000   Max.   :9.8     Max.   :10.830   Max.   : 1.104   Max.   :-0.966  
#  NA's   :31000   NA's   :31000   NA's   :31000   NA's   :31000   NA's   :31000    NA's   :31000    NA's   :31000   
# > print("Parametric Percentile Method--------------------")
# [1] "Parametric Percentile Method--------------------"
# > summary(sim2.Para.B2.round1[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :9.8     Min.   : 8.681   Min.   :-1.107   Min.   :-2.213  
#  1st Qu.:0.200   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.8     1st Qu.: 9.534   1st Qu.:-0.270   1st Qu.:-1.752  
#  Median :0.500   Median :0.000   Median :0.000   Median :9.8     Median : 9.804   Median :-0.004   Median :-1.651  
#  Mean   :0.506   Mean   :0.019   Mean   :0.025   Mean   :9.8     Mean   : 9.790   Mean   : 0.010   Mean   :-1.650  
#  3rd Qu.:0.800   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:9.8     3rd Qu.:10.070   3rd Qu.: 0.266   3rd Qu.:-1.541  
#  Max.   :1.000   Max.   :1.000   Max.   :1.000   Max.   :9.8     Max.   :10.907   Max.   : 1.119   Max.   :-1.201  
#  NA's   :31000   NA's   :31000   NA's   :31000   NA's   :31000   NA's   :31000    NA's   :31000    NA's   :31000   
# > 
########hist no obvious difference#################
hist(sim2.nonPara.percentile.B2.round1$coverage.set)
hist(sim2.nonPara.BCa.B2.round1$coverage.set)
hist(sim2.Para.B2.round1$coverage.set)
######summary conlclusion###################
# 1. no obvious differ coverage 0.5
# 2. m3' not coveraged means tend to be balanced, m1 and m2 tend to be bigger
# 3. boot mean no obvious differ
# 4. ci lenght m2 better

################################################################


############# B2, round2#####################

sim2.B2.round2 <- sim2[which(sim2$B.set == 100 & sim2$round.set == 100),]
sim2.nonPara.percentile.B2.round2  <- sim2.B2.round2[which(sim2$method.set == 1),]
sim2.nonPara.BCa.B2.round2 <- sim2.B2.round2[which(sim2$method.set == 2),]
sim2.Para.B2.round2 <- sim2.B2.round2[which(sim2$method.set == 3),]

print("Non-Parametric Percentile Method----------------")
summary(sim2.nonPara.percentile.B2.round2[c(2,3,4,5,6,10,11)])
print("Non-Parametric BCa------------------------------")
summary(sim2.nonPara.BCa.B2.round2[c(2,3,4,5,6,10,11)])
print("Parametric Percentile Method--------------------")
summary(sim2.Para.B2.round2[c(2,3,4,5,6,10,11)])
######summary results###################
# > print("Non-Parametric Percentile Method----------------")
# [1] "Non-Parametric Percentile Method----------------"
# > summary(sim2.nonPara.percentile.B2.round2[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :9.8     Min.   : 8.592   Min.   :-1.093   Min.   :-2.764  
#  1st Qu.:0.200   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.8     1st Qu.: 9.522   1st Qu.:-0.279   1st Qu.:-1.801  
#  Median :0.450   Median :0.000   Median :0.000   Median :9.8     Median : 9.814   Median :-0.014   Median :-1.642  
#  Mean   :0.463   Mean   :0.014   Mean   :0.027   Mean   :9.8     Mean   : 9.790   Mean   : 0.010   Mean   :-1.644  
#  3rd Qu.:0.720   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:9.8     3rd Qu.:10.079   3rd Qu.: 0.278   3rd Qu.:-1.483  
#  Max.   :1.000   Max.   :0.600   Max.   :1.000   Max.   :9.8     Max.   :10.893   Max.   : 1.208   Max.   :-0.934  
#  NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000    NA's   :22000   
# > print("Non-Parametric BCa------------------------------")
# [1] "Non-Parametric BCa------------------------------"
# > summary(sim2.nonPara.BCa.B2.round2[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.000   Min.   :0.000   Min.   :0.00    Min.   :9.8     Min.   : 8.663   Min.   :-1.053   Min.   :-2.753  
#  1st Qu.:0.200   1st Qu.:0.000   1st Qu.:0.00    1st Qu.:9.8     1st Qu.: 9.527   1st Qu.:-0.278   1st Qu.:-1.800  
#  Median :0.440   Median :0.000   Median :0.00    Median :9.8     Median : 9.814   Median :-0.014   Median :-1.636  
#  Mean   :0.457   Mean   :0.018   Mean   :0.03    Mean   :9.8     Mean   : 9.786   Mean   : 0.014   Mean   :-1.638  
#  3rd Qu.:0.710   3rd Qu.:0.000   3rd Qu.:0.00    3rd Qu.:9.8     3rd Qu.:10.078   3rd Qu.: 0.273   3rd Qu.:-1.473  
#  Max.   :1.000   Max.   :0.610   Max.   :1.00    Max.   :9.8     Max.   :10.853   Max.   : 1.137   Max.   :-0.825  
#  NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000    NA's   :22000   
# > print("Parametric Percentile Method--------------------")
# [1] "Parametric Percentile Method--------------------"
# > summary(sim2.Para.B2.round2[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :9.8     Min.   : 8.605   Min.   :-1.112   Min.   :-2.361  
#  1st Qu.:0.200   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.8     1st Qu.: 9.524   1st Qu.:-0.277   1st Qu.:-1.761  
#  Median :0.450   Median :0.000   Median :0.000   Median :9.8     Median : 9.811   Median :-0.011   Median :-1.655  
#  Mean   :0.466   Mean   :0.017   Mean   :0.022   Mean   :9.8     Mean   : 9.791   Mean   : 0.009   Mean   :-1.661  
#  3rd Qu.:0.720   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:9.8     3rd Qu.:10.077   3rd Qu.: 0.276   3rd Qu.:-1.554  
#  Max.   :1.000   Max.   :0.890   Max.   :1.000   Max.   :9.8     Max.   :10.912   Max.   : 1.195   Max.   :-1.172  
#  NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000    NA's   :22000  
##############hist m3 has the lowest rate of low coverage, no obvi diff for m1 and m2#######################
hist(sim2.nonPara.percentile.B2.round2$coverage.set)
hist(sim2.nonPara.BCa.B2.round2$coverage.set)
hist(sim2.Para.B2.round2$coverage.set)
######summary conlclusion###################
# 1. the coverage: m1 the best, m3 varies 0.46
# 2. m3' not coveraged means tend to be balanced, m1 and m2 tend to be bigger
# 3. m3 has the best boot mean, m2 the second best(may because 4)
# 4. ci lenght m2 better

################################################################




############### B3, round3####################
sim2.B3.round3 <- sim2[which(sim2$B.set == 1000 & sim2$round.set == 1000),]
sim2.nonPara.percentile.B3.round3  <- sim2.B3.round3[which(sim2$method.set == 1),]
sim2.nonPara.BCa.B3.round3 <- sim2.B3.round3[which(sim2$method.set == 2),]
sim2.Para.B3.round3 <- sim2.B3.round3[which(sim2$method.set == 3),]

print("Non-Parametric Percentile Method----------------")
summary(sim2.nonPara.percentile.B3.round3[c(2,3,4,5,6,10,11)])
print("Non-Parametric BCa------------------------------")
summary(sim2.nonPara.BCa.B3.round3[c(2,3,4,5,6,10,11)])
print("Parametric Percentile Method--------------------")
summary(sim2.Para.B3.round3[c(2,3,4,5,6,10,11)])
######summary results####################
# > print("Non-Parametric Percentile Method----------------")
# [1] "Non-Parametric Percentile Method----------------"
# > summary(sim2.nonPara.percentile.B3.round3[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set   difference.set    length.set    
#  Min.   :0.001   Min.   :0       Min.   :0       Min.   :9.8     Min.   :9.532   Min.   :0.138   Min.   :-2.140  
#  1st Qu.:0.251   1st Qu.:0       1st Qu.:0       1st Qu.:9.8     1st Qu.:9.589   1st Qu.:0.191   1st Qu.:-1.921  
#  Median :0.500   Median :0       Median :0       Median :9.8     Median :9.599   Median :0.201   Median :-1.881  
#  Mean   :0.500   Mean   :0       Mean   :0       Mean   :9.8     Mean   :9.599   Mean   :0.201   Mean   :-1.883  
#  3rd Qu.:0.750   3rd Qu.:0       3rd Qu.:0       3rd Qu.:9.8     3rd Qu.:9.609   3rd Qu.:0.211   3rd Qu.:-1.840  
#  Max.   :1.000   Max.   :0       Max.   :0       Max.   :9.8     Max.   :9.662   Max.   :0.268   Max.   :-1.661  
#  NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   
# > print("Non-Parametric BCa------------------------------")
# [1] "Non-Parametric BCa------------------------------"
# > summary(sim2.nonPara.BCa.B3.round3[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set   difference.set    length.set    
#  Min.   :0.001   Min.   :0       Min.   :0       Min.   :9.8     Min.   :9.561   Min.   :0.163   Min.   :-2.092  
#  1st Qu.:0.251   1st Qu.:0       1st Qu.:0       1st Qu.:9.8     1st Qu.:9.589   1st Qu.:0.197   1st Qu.:-1.920  
#  Median :0.500   Median :0       Median :0       Median :9.8     Median :9.596   Median :0.204   Median :-1.880  
#  Mean   :0.500   Mean   :0       Mean   :0       Mean   :9.8     Mean   :9.596   Mean   :0.204   Mean   :-1.883  
#  3rd Qu.:0.750   3rd Qu.:0       3rd Qu.:0       3rd Qu.:9.8     3rd Qu.:9.603   3rd Qu.:0.211   3rd Qu.:-1.840  
#  Max.   :1.000   Max.   :0       Max.   :0       Max.   :9.8     Max.   :9.637   Max.   :0.239   Max.   :-1.687  
#  NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   
# > print("Parametric Percentile Method--------------------")
# [1] "Parametric Percentile Method--------------------"
# > summary(sim2.Para.B3.round3[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set   difference.set    length.set    
#  Min.   :0.001   Min.   :0       Min.   :0       Min.   :9.8     Min.   :9.548   Min.   :0.140   Min.   :-1.901  
#  1st Qu.:0.251   1st Qu.:0       1st Qu.:0       1st Qu.:9.8     1st Qu.:9.590   1st Qu.:0.192   1st Qu.:-1.740  
#  Median :0.500   Median :0       Median :0       Median :9.8     Median :9.599   Median :0.201   Median :-1.701  
#  Mean   :0.500   Mean   :0       Mean   :0       Mean   :9.8     Mean   :9.599   Mean   :0.201   Mean   :-1.710  
#  3rd Qu.:0.750   3rd Qu.:0       3rd Qu.:0       3rd Qu.:9.8     3rd Qu.:9.608   3rd Qu.:0.210   3rd Qu.:-1.680  
#  Max.   :1.000   Max.   :0       Max.   :0       Max.   :9.8     Max.   :9.660   Max.   :0.252   Max.   :-1.520  
#  NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   
# > 
##############hist##########################
hist(sim2.nonPara.percentile.B3.round3$coverage.set)
hist(sim2.nonPara.BCa.B3.round3$coverage.set)
hist(sim2.Para.B3.round3$coverage.set)
######summary conlclusion###################
# 1. the coverage almost the same
# 2. m3 has the best boot mean, m2 the second best(may because 4)
# 3. ci lenght m3 the best, m1 has the longest

################################################################


################################################################

#####simulation3#################################################
#### 

## analys for n = 10, alpha = 0.05, and normal distribution
# here I will not compare the different Bs or rounds' effects
# I will focus on the sample size, only take round2, round3 and B2, B3
sim3<- boot.result[which(boot.result$n.set == 10 & boot.result$distribution.set == "normal" & boot.result$alpha.set == 0.05),]
################################################################

############## B2, round2#####################
sim3.B2.round2 <- sim3[which(sim3$B.set == 100 & sim3$round.set == 100),]
sim3.nonPara.percentile.B2.round2  <- sim3.B2.round2[which(sim3$method.set == 1),]
sim3.nonPara.BCa.B2.round2 <- sim3.B2.round2[which(sim3$method.set == 2),]
sim3.Para.B2.round2 <- sim3.B2.round2[which(sim3$method.set == 3),]

print("Non-Parametric Percentile Method----------------")
summary(sim3.nonPara.percentile.B2.round2[c(2,3,4,5,6,10,11)])
print("Non-Parametric BCa------------------------------")
summary(sim3.nonPara.BCa.B2.round2[c(2,3,4,5,6,10,11)])
print("Parametric Percentile Method--------------------")
summary(sim3.Para.B2.round2[c(2,3,4,5,6,10,11)])
######summary results###################################
# > print("Non-Parametric Percentile Method----------------")
# [1] "Non-Parametric Percentile Method----------------"
# > summary(sim3.nonPara.percentile.B2.round2[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :9.8     Min.   : 8.467   Min.   :-1.711   Min.   :-4.147  
#  1st Qu.:0.190   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.8     1st Qu.: 9.314   1st Qu.:-0.284   1st Qu.:-2.506  
#  Median :0.450   Median :0.000   Median :0.000   Median :9.8     Median : 9.745   Median : 0.055   Median :-2.106  
#  Mean   :0.463   Mean   :0.015   Mean   :0.027   Mean   :9.8     Mean   : 9.716   Mean   : 0.084   Mean   :-2.148  
#  3rd Qu.:0.720   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:9.8     3rd Qu.:10.084   3rd Qu.: 0.486   3rd Qu.:-1.760  
#  Max.   :1.000   Max.   :1.000   Max.   :1.000   Max.   :9.8     Max.   :11.511   Max.   : 1.333   Max.   :-0.825  
#  NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000    NA's   :22000   
# > print("Non-Parametric BCa------------------------------")
# [1] "Non-Parametric BCa------------------------------"
# > summary(sim3.nonPara.BCa.B2.round2[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.000   Min.   :0.000   Min.   :0.00    Min.   :9.8     Min.   : 8.516   Min.   :-1.621   Min.   :-4.169  
#  1st Qu.:0.190   1st Qu.:0.000   1st Qu.:0.00    1st Qu.:9.8     1st Qu.: 9.308   1st Qu.:-0.275   1st Qu.:-2.496  
#  Median :0.450   Median :0.000   Median :0.00    Median :9.8     Median : 9.745   Median : 0.055   Median :-2.106  
#  Mean   :0.459   Mean   :0.015   Mean   :0.03    Mean   :9.8     Mean   : 9.715   Mean   : 0.085   Mean   :-2.145  
#  3rd Qu.:0.710   3rd Qu.:0.000   3rd Qu.:0.00    3rd Qu.:9.8     3rd Qu.:10.075   3rd Qu.: 0.492   3rd Qu.:-1.750  
#  Max.   :1.000   Max.   :0.990   Max.   :0.99    Max.   :9.8     Max.   :11.421   Max.   : 1.284   Max.   :-0.803  
#  NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000    NA's   :22000   
# > print("Parametric Percentile Method--------------------")
# [1] "Parametric Percentile Method--------------------"
# > summary(sim3.Para.B2.round2[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :9.8     Min.   : 8.487   Min.   :-1.744   Min.   :-4.176  
#  1st Qu.:0.200   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.8     1st Qu.: 9.317   1st Qu.:-0.284   1st Qu.:-2.646  
#  Median :0.460   Median :0.000   Median :0.000   Median :9.8     Median : 9.743   Median : 0.057   Median :-2.227  
#  Mean   :0.471   Mean   :0.013   Mean   :0.021   Mean   :9.8     Mean   : 9.717   Mean   : 0.083   Mean   :-2.273  
#  3rd Qu.:0.730   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:9.8     3rd Qu.:10.084   3rd Qu.: 0.483   3rd Qu.:-1.858  
#  Max.   :1.000   Max.   :0.990   Max.   :1.000   Max.   :9.8     Max.   :11.544   Max.   : 1.313   Max.   :-0.830  
#  NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000    NA's   :22000   
# > 
##############hist m3 performs best for low rate of low coverage and high rate of high coverage##############################
hist(sim3.nonPara.percentile.B2.round2$coverage.set)
hist(sim3.nonPara.BCa.B2.round2$coverage.set)
hist(sim3.Para.B2.round2$coverage.set)
######summary conlclusion###################
# 1. m3 best but no obvious difference
# 2. all mehtods' not coveraged means tend to a little bit bigger set
# 3. boot mean no obvious difference
# 4. ci lenght no obvious difference

################################################################

###############  B3, round3############## 
sim3.B3.round3 <- sim3[which(sim3$B.set == 1000 & sim3$round.set == 1000),]
sim3.nonPara.percentile.B3.round3  <- sim3.B3.round3[which(sim3$method.set == 1),]
sim3.nonPara.BCa.B3.round3 <- sim3.B3.round3[which(sim3$method.set == 2),]
sim3.Para.B3.round3 <- sim3.B3.round3[which(sim3$method.set == 3),]

print("Non-Parametric Percentile Method----------------")
summary(sim3.nonPara.percentile.B3.round3[c(2,3,4,5,6,10,11)])
print("Non-Parametric BCa------------------------------")
summary(sim3.nonPara.BCa.B3.round3[c(2,3,4,5,6,10,11)])
print("Parametric Percentile Method--------------------")
summary(sim3.Para.B3.round3[c(2,3,4,5,6,10,11)])
######summary results#######
# > print("Non-Parametric Percentile Method----------------")
# [1] "Non-Parametric Percentile Method----------------"
# > summary(sim3.nonPara.percentile.B3.round3[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.001   Min.   :0       Min.   :0       Min.   :9.8     Min.   : 9.947   Min.   :-0.329   Min.   :-2.946  
#  1st Qu.:0.251   1st Qu.:0       1st Qu.:0       1st Qu.:9.8     1st Qu.:10.018   1st Qu.:-0.247   1st Qu.:-2.669  
#  Median :0.500   Median :0       Median :0       Median :9.8     Median :10.032   Median :-0.232   Median :-2.616  
#  Mean   :0.500   Mean   :0       Mean   :0       Mean   :9.8     Mean   :10.032   Mean   :-0.232   Mean   :-2.617  
#  3rd Qu.:0.750   3rd Qu.:0       3rd Qu.:0       3rd Qu.:9.8     3rd Qu.:10.047   3rd Qu.:-0.218   3rd Qu.:-2.564  
#  Max.   :1.000   Max.   :0       Max.   :0       Max.   :9.8     Max.   :10.129   Max.   :-0.147   Max.   :-2.347  
#  NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000    NA's   :22000   
# > print("Non-Parametric BCa------------------------------")
# [1] "Non-Parametric BCa------------------------------"
# > summary(sim3.nonPara.BCa.B3.round3[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.001   Min.   :0       Min.   :0       Min.   :9.8     Min.   : 9.955   Min.   :-0.325   Min.   :-2.995  
#  1st Qu.:0.251   1st Qu.:0       1st Qu.:0       1st Qu.:9.8     1st Qu.:10.020   1st Qu.:-0.250   1st Qu.:-2.678  
#  Median :0.500   Median :0       Median :0       Median :9.8     Median :10.035   Median :-0.235   Median :-2.625  
#  Mean   :0.500   Mean   :0       Mean   :0       Mean   :9.8     Mean   :10.035   Mean   :-0.235   Mean   :-2.626  
#  3rd Qu.:0.750   3rd Qu.:0       3rd Qu.:0       3rd Qu.:9.8     3rd Qu.:10.050   3rd Qu.:-0.220   3rd Qu.:-2.572  
#  Max.   :1.000   Max.   :0       Max.   :0       Max.   :9.8     Max.   :10.125   Max.   :-0.155   Max.   :-2.323  
#  NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000    NA's   :22000   
# > print("Parametric Percentile Method--------------------")
# [1] "Parametric Percentile Method--------------------"
# > summary(sim3.Para.B3.round3[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.001   Min.   :0       Min.   :0       Min.   :9.8     Min.   : 9.953   Min.   :-0.319   Min.   :-3.106  
#  1st Qu.:0.251   1st Qu.:0       1st Qu.:0       1st Qu.:9.8     1st Qu.:10.019   1st Qu.:-0.249   1st Qu.:-2.827  
#  Median :0.500   Median :0       Median :0       Median :9.8     Median :10.034   Median :-0.234   Median :-2.770  
#  Mean   :0.500   Mean   :0       Mean   :0       Mean   :9.8     Mean   :10.034   Mean   :-0.234   Mean   :-2.772  
#  3rd Qu.:0.750   3rd Qu.:0       3rd Qu.:0       3rd Qu.:9.8     3rd Qu.:10.049   3rd Qu.:-0.219   3rd Qu.:-2.714  
#  Max.   :1.000   Max.   :0       Max.   :0       Max.   :9.8     Max.   :10.119   Max.   :-0.153   Max.   :-2.449  
#  NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000    NA's   :22000   
##############hist#######
hist(sim3.nonPara.percentile.B3.round3$coverage.set)
hist(sim3.nonPara.BCa.B3.round3$coverage.set)
hist(sim3.Para.B3.round3$coverage.set)
######summary conlclusion###################
# 1. coverage no obvious difference
# 2. all mehtods' not coveraged means tend to be balanced
# 3. boot mean no obvious difference
# 4. ci lenght m1 and m2 better, no obvious difference

################################################################

#####simulation4################################################
#### 

## analys for n = 10, alpha = 0.05, and poisson distribution
# here I will not compare the different Bs or rounds' effects
# I will focus on the sample size, only take round2, round3 and B2, B3
sim4<- boot.result[which(boot.result$n.set == 10 & boot.result$distribution.set == "poisson" & boot.result$alpha.set == 0.05),]

############ B2, round2###########
sim4.B2.round2 <- sim4[which(sim4$B.set == 100 & sim4$round.set == 100),]
sim4.nonPara.percentile.B2.round2  <- sim4.B2.round2[which(sim4$method.set == 1),]
sim4.nonPara.BCa.B2.round2 <- sim4.B2.round2[which(sim4$method.set == 2),]
sim4.Para.B2.round2 <- sim4.B2.round2[which(sim4$method.set == 3),]

print("Non-Parametric Percentile Method----------------")
summary(sim4.nonPara.percentile.B2.round2[c(2,3,4,5,6,10,11)])
print("Non-Parametric BCa------------------------------")
summary(sim4.nonPara.BCa.B2.round2[c(2,3,4,5,6,10,11)])
print("Parametric Percentile Method--------------------")
summary(sim4.Para.B2.round2[c(2,3,4,5,6,10,11)])
######summary results###########
# > print("Non-Parametric Percentile Method----------------")
# [1] "Non-Parametric Percentile Method----------------"
# > summary(sim4.nonPara.percentile.B2.round2[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :9.8     Min.   : 7.162   Min.   :-2.733   Min.   :-7.820  
#  1st Qu.:0.180   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.8     1st Qu.: 9.099   1st Qu.:-0.554   1st Qu.:-4.058  
#  Median :0.450   Median :0.000   Median :0.000   Median :9.8     Median : 9.801   Median :-0.001   Median :-3.458  
#  Mean   :0.459   Mean   :0.025   Mean   :0.021   Mean   :9.8     Mean   : 9.770   Mean   : 0.030   Mean   :-3.512  
#  3rd Qu.:0.720   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:9.8     3rd Qu.:10.354   3rd Qu.: 0.701   3rd Qu.:-2.905  
#  Max.   :1.000   Max.   :1.000   Max.   :1.000   Max.   :9.8     Max.   :12.533   Max.   : 2.638   Max.   :-1.000  
#  NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000    NA's   :22000   
# > print("Non-Parametric BCa------------------------------")
# [1] "Non-Parametric BCa------------------------------"
# > summary(sim4.nonPara.BCa.B2.round2[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :9.8     Min.   : 7.235   Min.   :-2.656   Min.   :-8.455  
#  1st Qu.:0.180   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.8     1st Qu.: 9.071   1st Qu.:-0.528   1st Qu.:-4.031  
#  Median :0.440   Median :0.000   Median :0.000   Median :9.8     Median : 9.776   Median : 0.024   Median :-3.434  
#  Mean   :0.454   Mean   :0.023   Mean   :0.028   Mean   :9.8     Mean   : 9.745   Mean   : 0.055   Mean   :-3.494  
#  3rd Qu.:0.710   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:9.8     3rd Qu.:10.328   3rd Qu.: 0.729   3rd Qu.:-2.891  
#  Max.   :1.000   Max.   :1.000   Max.   :1.000   Max.   :9.8     Max.   :12.456   Max.   : 2.565   Max.   :-1.076  
#  NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000    NA's   :22000   
# > print("Parametric Percentile Method--------------------")
# [1] "Parametric Percentile Method--------------------"
# > summary(sim4.Para.B2.round2[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :9.8     Min.   : 7.151   Min.   :-2.779   Min.   :-5.258  
#  1st Qu.:0.230   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.8     1st Qu.: 9.094   1st Qu.:-0.558   1st Qu.:-3.953  
#  Median :0.480   Median :0.000   Median :0.000   Median :9.8     Median : 9.799   Median : 0.001   Median :-3.705  
#  Mean   :0.482   Mean   :0.008   Mean   :0.014   Mean   :9.8     Mean   : 9.769   Mean   : 0.031   Mean   :-3.706  
#  3rd Qu.:0.730   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:9.8     3rd Qu.:10.358   3rd Qu.: 0.706   3rd Qu.:-3.453  
#  Max.   :1.000   Max.   :0.950   Max.   :0.990   Max.   :9.8     Max.   :12.579   Max.   : 2.649   Max.   :-2.505  
#  NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000    NA's   :22000   
# > 
##############hist m3 has the lowest low coverage but m1 and m2 are better a lot for high coverage###########
hist(sim4.nonPara.percentile.B2.round2$coverage.set)
hist(sim4.nonPara.BCa.B2.round2$coverage.set)
hist(sim4.Para.B2.round2$coverage.set)
######summary conlclusion###################
# 1. coverage no obvious difference
# 2. all mehtods' not coveraged means tend to be balanced
# 3. boot mean no obvious difference, the range of m2's means are the smallest
# 4. ci lenght m1 and m2 better, no obvious difference

################################################################

################### B3, round3##################
sim4.B3.round3 <- sim4[which(sim4$B.set == 1000 & sim4$round.set == 1000),]
sim4.nonPara.percentile.B3.round3  <- sim4.B3.round3[which(sim4$method.set == 1),]
sim4.nonPara.BCa.B3.round3 <- sim4.B3.round3[which(sim4$method.set == 2),]
sim4.Para.B3.round3 <- sim4.B3.round3[which(sim4$method.set == 3),]

print("Non-Parametric Percentile Method----------------")
summary(sim4.nonPara.percentile.B3.round3[c(2,3,4,5,6,10,11)])
print("Non-Parametric BCa------------------------------")
summary(sim4.nonPara.BCa.B3.round3[c(2,3,4,5,6,10,11)])
print("Parametric Percentile Method--------------------")
summary(sim4.Para.B3.round3[c(2,3,4,5,6,10,11)])
######summary results##################
# > print("Non-Parametric Percentile Method----------------")
# [1] "Non-Parametric Percentile Method----------------"
# > summary(sim4.nonPara.percentile.B3.round3[c(2,3,4,5,6,10,11)])
# coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
# Min.   :0.001   Min.   :0       Min.   :0       Min.   :9.8     Min.   : 9.871   Min.   :-0.308   Min.   :-4.300  
# 1st Qu.:0.251   1st Qu.:0       1st Qu.:0       1st Qu.:9.8     1st Qu.: 9.976   1st Qu.:-0.219   1st Qu.:-3.902  
# Median :0.500   Median :0       Median :0       Median :9.8     Median : 9.998   Median :-0.198   Median :-3.900  
# Mean   :0.500   Mean   :0       Mean   :0       Mean   :9.8     Mean   : 9.998   Mean   :-0.198   Mean   :-3.850  
# 3rd Qu.:0.750   3rd Qu.:0       3rd Qu.:0       3rd Qu.:9.8     3rd Qu.:10.019   3rd Qu.:-0.176   3rd Qu.:-3.800  
# Max.   :1.000   Max.   :0       Max.   :0       Max.   :9.8     Max.   :10.108   Max.   :-0.071   Max.   :-3.400  
# NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000    NA's   :22000   
# > print("Non-Parametric BCa------------------------------")
# [1] "Non-Parametric BCa------------------------------"
# > summary(sim4.nonPara.BCa.B3.round3[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.001   Min.   :0       Min.   :0       Min.   :9.8     Min.   : 9.815   Min.   :-0.308   Min.   :-4.383  
#  1st Qu.:0.251   1st Qu.:0       1st Qu.:0       1st Qu.:9.8     1st Qu.: 9.945   1st Qu.:-0.193   1st Qu.:-3.903  
#  Median :0.500   Median :0       Median :0       Median :9.8     Median : 9.969   Median :-0.169   Median :-3.854  
#  Mean   :0.500   Mean   :0       Mean   :0       Mean   :9.8     Mean   : 9.969   Mean   :-0.169   Mean   :-3.853  
#  3rd Qu.:0.750   3rd Qu.:0       3rd Qu.:0       3rd Qu.:9.8     3rd Qu.: 9.993   3rd Qu.:-0.145   3rd Qu.:-3.800  
#  Max.   :1.000   Max.   :0       Max.   :0       Max.   :9.8     Max.   :10.108   Max.   :-0.015   Max.   :-3.400  
#  NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000    NA's   :22000   
# > print("Parametric Percentile Method--------------------")
# [1] "Parametric Percentile Method--------------------"
# > summary(sim4.Para.B3.round3[c(2,3,4,5,6,10,11)])
# coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
# Min.   :0.001   Min.   :0       Min.   :0       Min.   :9.8     Min.   : 9.880   Min.   :-0.321   Min.   :-4.400  
# 1st Qu.:0.251   1st Qu.:0       1st Qu.:0       1st Qu.:9.8     1st Qu.: 9.975   1st Qu.:-0.218   1st Qu.:-4.000  
# Median :0.500   Median :0       Median :0       Median :9.8     Median : 9.997   Median :-0.197   Median :-3.900  
# Mean   :0.500   Mean   :0       Mean   :0       Mean   :9.8     Mean   : 9.997   Mean   :-0.197   Mean   :-3.899  
# 3rd Qu.:0.750   3rd Qu.:0       3rd Qu.:0       3rd Qu.:9.8     3rd Qu.:10.018   3rd Qu.:-0.175   3rd Qu.:-3.800  
# Max.   :1.000   Max.   :0       Max.   :0       Max.   :9.8     Max.   :10.121   Max.   :-0.080   Max.   :-3.500  
# NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000    NA's   :22000   
# > 
##############hist no obvious differ##################
hist(sim4.nonPara.percentile.B3.round3$coverage.set)
hist(sim4.nonPara.BCa.B3.round3$coverage.set)
hist(sim4.Para.B3.round3$coverage.set)
######summary conlclusion###################
# 1. coverage no obvious difference
# 2. all mehtods' not coveraged means tend to be balanced
# 3. boot mean m2 best
# 4. ci lenght no obvious difference

################################################################


######simulation5################################################
#### 

## analyse the different alpha for n = 50, alpha = 0.01, and normal distribution
# here I will not  compare the different Bs or rounds' effects
# so I will only use B2, round2, B3 round 3
sim5<- boot.result[which(boot.result$n.set == 50 & boot.result$distribution.set == "normal" & boot.result$alpha.set == 0.01),]
################################################################

####################### B2, round2######################
sim5.B2.round2 <- sim5[which(sim5$B.set == 100 & sim5$round.set == 100),]
sim5.nonPara.percentile.B2.round2  <- sim5.B2.round2[which(sim5$method.set == 1),]
sim5.nonPara.BCa.B2.round2 <- sim5.B2.round2[which(sim5$method.set == 2),]
sim5.Para.B2.round2 <- sim5.B2.round2[which(sim5$method.set == 3),]

print("Non-Parametric Percentile Method----------------")
summary(sim5.nonPara.percentile.B2.round2[c(2,3,4,5,6,10,11)])
print("Non-Parametric BCa------------------------------")
summary(sim5.nonPara.BCa.B2.round2[c(2,3,4,5,6,10,11)])
print("Parametric Percentile Method--------------------")
summary(sim5.Para.B2.round2[c(2,3,4,5,6,10,11)])
######summary result###############
# > print("Non-Parametric Percentile Method----------------")
# [1] "Non-Parametric Percentile Method----------------"
# > summary(sim5.nonPara.percentile.B2.round2[c(2,3,4,5,6,10,11)])
# coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
# Min.   :0.000   Min.   :0.00    Min.   :0.000   Min.   :9.8     Min.   : 8.850   Min.   :-0.768   Min.   :-2.080  
# 1st Qu.:0.220   1st Qu.:0.00    1st Qu.:0.000   1st Qu.:9.8     1st Qu.: 9.555   1st Qu.:-0.152   1st Qu.:-1.417  
# Median :0.480   Median :0.00    Median :0.000   Median :9.8     Median : 9.780   Median : 0.020   Median :-1.287  
# Mean   :0.481   Mean   :0.01    Mean   :0.014   Mean   :9.8     Mean   : 9.759   Mean   : 0.041   Mean   :-1.301  
# 3rd Qu.:0.730   3rd Qu.:0.00    3rd Qu.:0.000   3rd Qu.:9.8     3rd Qu.: 9.952   3rd Qu.: 0.245   3rd Qu.:-1.170  
# Max.   :1.000   Max.   :1.00    Max.   :0.940   Max.   :9.8     Max.   :10.568   Max.   : 0.950   Max.   :-0.788  
# NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000    NA's   :22000   
# > print("Non-Parametric BCa------------------------------")
# [1] "Non-Parametric BCa------------------------------"
# > summary(sim5.nonPara.BCa.B2.round2[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
#  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :9.8     Min.   : 8.880   Min.   :-0.753   Min.   :-2.051  
#  1st Qu.:0.220   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.8     1st Qu.: 9.557   1st Qu.:-0.152   1st Qu.:-1.401  
#  Median :0.470   Median :0.000   Median :0.000   Median :9.8     Median : 9.778   Median : 0.022   Median :-1.270  
#  Mean   :0.478   Mean   :0.011   Mean   :0.015   Mean   :9.8     Mean   : 9.759   Mean   : 0.041   Mean   :-1.284  
#  3rd Qu.:0.730   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:9.8     3rd Qu.: 9.952   3rd Qu.: 0.243   3rd Qu.:-1.152  
#  Max.   :1.000   Max.   :1.000   Max.   :0.960   Max.   :9.8     Max.   :10.553   Max.   : 0.920   Max.   :-0.743  
#  NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000    NA's   :22000   
# > print("Parametric Percentile Method--------------------")
# [1] "Parametric Percentile Method--------------------"
# > summary(sim5.Para.B2.round2[c(2,3,4,5,6,10,11)])
# coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
# Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :9.8     Min.   : 8.861   Min.   :-0.756   Min.   :-2.419  
# 1st Qu.:0.220   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.8     1st Qu.: 9.557   1st Qu.:-0.152   1st Qu.:-1.433  
# Median :0.480   Median :0.000   Median :0.000   Median :9.8     Median : 9.779   Median : 0.021   Median :-1.298  
# Mean   :0.481   Mean   :0.009   Mean   :0.015   Mean   :9.8     Mean   : 9.760   Mean   : 0.040   Mean   :-1.315  
# 3rd Qu.:0.730   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:9.8     3rd Qu.: 9.952   3rd Qu.: 0.243   3rd Qu.:-1.186  
# Max.   :1.000   Max.   :0.970   Max.   :0.930   Max.   :9.8     Max.   :10.556   Max.   : 0.939   Max.   :-0.763  
# NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000    NA's   :22000   
##############hist m3 has the lowest low coverage rate###############
hist(sim5.nonPara.percentile.B2.round2$coverage.set)
hist(sim5.nonPara.BCa.B2.round2$coverage.set)
hist(sim5.Para.B2.round2$coverage.set)
######summary conlclusion###################
# 1. coverage no obvious difference, m1 and m3 are a little better
# 2. all mehtods' not coveraged means tend to be balanced
# 3. boot mean m2 best
# 4. ci lenght no obvious difference, m2 better a little

################################################################

################## B3, round3#################
sim5.B3.round3 <- sim5[which(sim5$B.set == 1000 & sim5$round.set == 1000),]
sim5.nonPara.percentile.B3.round3  <- sim5.B3.round3[which(sim5$method.set == 1),]
sim5.nonPara.BCa.B3.round3 <- sim5.B3.round3[which(sim5$method.set == 2),]
sim5.Para.B3.round3 <- sim5.B3.round3[which(sim5$method.set == 3),]

print("Non-Parametric Percentile Method----------------")
summary(sim5.nonPara.percentile.B3.round3[c(2,3,4,5,6,10,11)])
print("Non-Parametric BCa------------------------------")
summary(sim5.nonPara.BCa.B3.round3[c(2,3,4,5,6,10,11)])
print("Parametric Percentile Method--------------------")
summary(sim5.Para.B3.round3[c(2,3,4,5,6,10,11)])
######summary result#################
# > print("Non-Parametric Percentile Method----------------")
# [1] "Non-Parametric Percentile Method----------------"
# > summary(sim5.nonPara.percentile.B3.round3[c(2,3,4,5,6,10,11)])
# coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set   difference.set    length.set    
# Min.   :0.000   Min.   :0       Min.   :0.001   Min.   :9.8     Min.   :8.894   Min.   :0.832   Min.   :-1.731  
# 1st Qu.:0.003   1st Qu.:0       1st Qu.:0.247   1st Qu.:9.8     1st Qu.:8.925   1st Qu.:0.863   1st Qu.:-1.532  
# Median :0.005   Median :0       Median :0.494   Median :9.8     Median :8.931   Median :0.869   Median :-1.490  
# Mean   :0.006   Mean   :0       Mean   :0.494   Mean   :9.8     Mean   :8.931   Mean   :0.869   Mean   :-1.492  
# 3rd Qu.:0.009   3rd Qu.:0       3rd Qu.:0.741   3rd Qu.:9.8     3rd Qu.:8.937   3rd Qu.:0.875   3rd Qu.:-1.449  
# Max.   :0.018   Max.   :0       Max.   :0.994   Max.   :9.8     Max.   :8.968   Max.   :0.906   Max.   :-1.284  
# NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   
# > print("Non-Parametric BCa------------------------------")
# [1] "Non-Parametric BCa------------------------------"
# > summary(sim5.nonPara.BCa.B3.round3[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set   difference.set    length.set    
#  Min.   :0.000   Min.   :0       Min.   :0.001   Min.   :9.8     Min.   :8.903   Min.   :0.837   Min.   :-1.745  
#  1st Qu.:0.006   1st Qu.:0       1st Qu.:0.245   1st Qu.:9.8     1st Qu.:8.926   1st Qu.:0.863   1st Qu.:-1.532  
#  Median :0.011   Median :0       Median :0.489   Median :9.8     Median :8.931   Median :0.869   Median :-1.491  
#  Mean   :0.012   Mean   :0       Mean   :0.488   Mean   :9.8     Mean   :8.931   Mean   :0.869   Mean   :-1.492  
#  3rd Qu.:0.019   3rd Qu.:0       3rd Qu.:0.733   3rd Qu.:9.8     3rd Qu.:8.937   3rd Qu.:0.874   3rd Qu.:-1.450  
#  Max.   :0.032   Max.   :0       Max.   :0.983   Max.   :9.8     Max.   :8.963   Max.   :0.897   Max.   :-1.288  
#  NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   
# > print("Parametric Percentile Method--------------------")
# [1] "Parametric Percentile Method--------------------"
# > summary(sim5.Para.B3.round3[c(2,3,4,5,6,10,11)])
# coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set   difference.set    length.set    
# Min.   :0.000   Min.   :0       Min.   :0.001   Min.   :9.8     Min.   :8.895   Min.   :0.831   Min.   :-1.810  
# 1st Qu.:0.002   1st Qu.:0       1st Qu.:0.248   1st Qu.:9.8     1st Qu.:8.925   1st Qu.:0.862   1st Qu.:-1.553  
# Median :0.005   Median :0       Median :0.495   Median :9.8     Median :8.931   Median :0.869   Median :-1.509  
# Mean   :0.005   Mean   :0       Mean   :0.496   Mean   :9.8     Mean   :8.931   Mean   :0.869   Mean   :-1.511  
# 3rd Qu.:0.008   3rd Qu.:0       3rd Qu.:0.743   3rd Qu.:9.8     3rd Qu.:8.938   3rd Qu.:0.875   3rd Qu.:-1.468  
# Max.   :0.017   Max.   :0       Max.   :0.996   Max.   :9.8     Max.   :8.969   Max.   :0.905   Max.   :-1.309  
# NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   
# > 
##############hist#################
hist(sim5.nonPara.percentile.B3.round3$coverage.set)
hist(sim5.nonPara.BCa.B3.round3$coverage.set)
hist(sim5.Para.B3.round3$coverage.set)
###super bad###summary conlclusion###################
# 1. coverage are very bad, 0.02
# 2. all mehtods' not coveraged means tend to be bigger than CI, very high
# 3. boot mean no obvious difference
# 4. ci lenght no obvious difference

################################################################


####simulation6##################################################
#### 

## analyse the different methods for n = 50, alpha = 0.01, and poisson distribution
# here I will not  compare the different Bs or rounds' effects
# so I will only use B2, round2, B3 round 3
sim6<- boot.result[which(boot.result$n.set == 50 & boot.result$distribution.set == "poisson" & boot.result$alpha.set == 0.01),]
################################################################

################### B2, round2##################

sim6.B2.round2 <- sim6[which(sim6$B.set == 100 & sim6$round.set == 100),]
sim6.nonPara.percentile.B2.round2  <- sim6.B2.round2[which(sim6$method.set == 1),]
sim6.nonPara.BCa.B2.round2 <- sim6.B2.round2[which(sim6$method.set == 2),]
sim6.Para.B2.round2 <- sim6.B2.round2[which(sim6$method.set == 3),]

print("Non-Parametric Percentile Method----------------")
summary(sim6.nonPara.percentile.B2.round2[c(2,3,4,5,6,10,11)])
print("Non-Parametric BCa------------------------------")
summary(sim6.nonPara.BCa.B2.round2[c(2,3,4,5,6,10,11)])
print("Parametric Percentile Method--------------------")
summary(sim6.Para.B2.round2[c(2,3,4,5,6,10,11)])
######summary result#############
# > print("Non-Parametric Percentile Method----------------")
# [1] "Non-Parametric Percentile Method----------------"
# > summary(sim6.nonPara.percentile.B2.round2[c(2,3,4,5,6,10,11)])
# coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
# Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :9.8     Min.   : 8.640   Min.   :-1.135   Min.   :-3.303  
# 1st Qu.:0.250   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.8     1st Qu.: 9.600   1st Qu.:-0.291   1st Qu.:-2.234  
# Median :0.490   Median :0.000   Median :0.000   Median :9.8     Median : 9.826   Median :-0.026   Median :-2.031  
# Mean   :0.496   Mean   :0.002   Mean   :0.008   Mean   :9.8     Mean   : 9.829   Mean   :-0.029   Mean   :-2.049  
# 3rd Qu.:0.742   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:9.8     3rd Qu.:10.091   3rd Qu.: 0.200   3rd Qu.:-1.844  
# Max.   :1.000   Max.   :0.140   Max.   :0.690   Max.   :9.8     Max.   :10.935   Max.   : 1.160   Max.   :-1.241  
# NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000    NA's   :22000   
# > print("Non-Parametric BCa------------------------------")
# [1] "Non-Parametric BCa------------------------------"
# > summary(sim6.nonPara.BCa.B2.round2[c(2,3,4,5,6,10,11)])
# coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
# Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :9.8     Min.   : 8.675   Min.   :-1.171   Min.   :-3.229  
# 1st Qu.:0.240   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.8     1st Qu.: 9.599   1st Qu.:-0.289   1st Qu.:-2.204  
# Median :0.490   Median :0.000   Median :0.000   Median :9.8     Median : 9.823   Median :-0.023   Median :-1.999  
# Mean   :0.493   Mean   :0.003   Mean   :0.009   Mean   :9.8     Mean   : 9.828   Mean   :-0.028   Mean   :-2.016  
# 3rd Qu.:0.740   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:9.8     3rd Qu.:10.089   3rd Qu.: 0.201   3rd Qu.:-1.810  
# Max.   :1.000   Max.   :0.240   Max.   :0.810   Max.   :9.8     Max.   :10.971   Max.   : 1.125   Max.   :-1.189  
# NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000    NA's   :22000   
# > print("Parametric Percentile Method--------------------")
# [1] "Parametric Percentile Method--------------------"
# > summary(sim6.Para.B2.round2[c(2,3,4,5,6,10,11)])
# coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
# Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :9.8     Min.   : 8.633   Min.   :-1.118   Min.   :-3.087  
# 1st Qu.:0.250   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.8     1st Qu.: 9.601   1st Qu.:-0.294   1st Qu.:-2.202  
# Median :0.490   Median :0.000   Median :0.000   Median :9.8     Median : 9.826   Median :-0.026   Median :-2.052  
# Mean   :0.496   Mean   :0.003   Mean   :0.006   Mean   :9.8     Mean   : 9.829   Mean   :-0.029   Mean   :-2.064  
# 3rd Qu.:0.740   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:9.8     3rd Qu.:10.094   3rd Qu.: 0.199   3rd Qu.:-1.913  
# Max.   :1.000   Max.   :0.410   Max.   :0.710   Max.   :9.8     Max.   :10.918   Max.   : 1.167   Max.   :-1.442  
# NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000    NA's   :22000  
##############hist all coverage rates from 0 to 1 are even#############
hist(sim6.nonPara.percentile.B2.round2$coverage.set)
hist(sim6.nonPara.BCa.B2.round2$coverage.set)
hist(sim6.Para.B2.round2$coverage.set)
######summary conlclusion###################
# 1. coverage becomes normal
# 2. all mehtods' not coveraged means tend to be balanced
# 3. boot mean no obvious difference
# 4. ci lenght m2 best

################################################################

#################### B3, round3###################
sim6.B3.round3 <- sim6[which(sim6$B.set == 1000 & sim6$round.set == 1000),]
sim6.nonPara.percentile.B3.round3  <- sim6.B3.round3[which(sim6$method.set == 1),]
sim6.nonPara.BCa.B3.round3 <- sim6.B3.round3[which(sim6$method.set == 2),]
sim6.Para.B3.round3 <- sim6.B3.round3[which(sim6$method.set == 3),]

print("Non-Parametric Percentile Method----------------")
summary(sim6.nonPara.percentile.B3.round3[c(2,3,4,5,6,10,11)])
print("Non-Parametric BCa------------------------------")
summary(sim6.nonPara.BCa.B3.round3[c(2,3,4,5,6,10,11)])
print("Parametric Percentile Method--------------------")
summary(sim6.Para.B3.round3[c(2,3,4,5,6,10,11)])
######summary result######
# > print("Non-Parametric Percentile Method----------------")
# [1] "Non-Parametric Percentile Method----------------"
# > summary(sim6.nonPara.percentile.B3.round3[c(2,3,4,5,6,10,11)])
# coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set   difference.set    length.set    
# Min.   :0.001   Min.   :0       Min.   :0       Min.   :9.8     Min.   :9.486   Min.   :0.212   Min.   :-2.500  
# 1st Qu.:0.251   1st Qu.:0       1st Qu.:0       1st Qu.:9.8     1st Qu.:9.531   1st Qu.:0.251   1st Qu.:-2.220  
# Median :0.500   Median :0       Median :0       Median :9.8     Median :9.540   Median :0.260   Median :-2.160  
# Mean   :0.500   Mean   :0       Mean   :0       Mean   :9.8     Mean   :9.540   Mean   :0.260   Mean   :-2.156  
# 3rd Qu.:0.750   3rd Qu.:0       3rd Qu.:0       3rd Qu.:9.8     3rd Qu.:9.549   3rd Qu.:0.269   3rd Qu.:-2.100  
# Max.   :1.000   Max.   :0       Max.   :0       Max.   :9.8     Max.   :9.588   Max.   :0.314   Max.   :-1.820  
# NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   
# > print("Non-Parametric BCa------------------------------")
# [1] "Non-Parametric BCa------------------------------"
# > summary(sim6.nonPara.BCa.B3.round3[c(2,3,4,5,6,10,11)])
# coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set   difference.set    length.set    
# Min.   :0.001   Min.   :0       Min.   :0       Min.   :9.8     Min.   :9.499   Min.   :0.220   Min.   :-2.495  
# 1st Qu.:0.251   1st Qu.:0       1st Qu.:0       1st Qu.:9.8     1st Qu.:9.531   1st Qu.:0.254   1st Qu.:-2.217  
# Median :0.500   Median :0       Median :0       Median :9.8     Median :9.539   Median :0.261   Median :-2.158  
# Mean   :0.500   Mean   :0       Mean   :0       Mean   :9.8     Mean   :9.539   Mean   :0.261   Mean   :-2.159  
# 3rd Qu.:0.750   3rd Qu.:0       3rd Qu.:0       3rd Qu.:9.8     3rd Qu.:9.546   3rd Qu.:0.269   3rd Qu.:-2.100  
# Max.   :1.000   Max.   :0       Max.   :0       Max.   :9.8     Max.   :9.580   Max.   :0.301   Max.   :-1.873  
# NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   
# > print("Parametric Percentile Method--------------------")
# [1] "Parametric Percentile Method--------------------"
# > summary(sim6.Para.B3.round3[c(2,3,4,5,6,10,11)])
# coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set   difference.set    length.set    
# Min.   :0.001   Min.   :0       Min.   :0       Min.   :9.8     Min.   :9.481   Min.   :0.212   Min.   :-2.560  
# 1st Qu.:0.251   1st Qu.:0       1st Qu.:0       1st Qu.:9.8     1st Qu.:9.530   1st Qu.:0.251   1st Qu.:-2.280  
# Median :0.500   Median :0       Median :0       Median :9.8     Median :9.540   Median :0.260   Median :-2.220  
# Mean   :0.500   Mean   :0       Mean   :0       Mean   :9.8     Mean   :9.540   Mean   :0.260   Mean   :-2.219  
# 3rd Qu.:0.750   3rd Qu.:0       3rd Qu.:0       3rd Qu.:9.8     3rd Qu.:9.549   3rd Qu.:0.270   3rd Qu.:-2.160  
# Max.   :1.000   Max.   :0       Max.   :0       Max.   :9.8     Max.   :9.588   Max.   :0.319   Max.   :-1.940  
# NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   
##############hist######
hist(sim6.nonPara.percentile.B3.round3$coverage.set)
hist(sim6.nonPara.BCa.B3.round3$coverage.set)
hist(sim6.Para.B3.round3$coverage.set)
######summary conlclusion###################
# 1. coverage becomes normal
# 2. all mehtods' not coveraged means tend to be balanced
# 3. boot mean no obvious difference
# 4. ci lenght  no obvious difference

################################################################



####simulation7##################################################
#### 

## analyse the different methods for n = 50, alpha = 0.02, and normal distribution
# here I will not  compare the different Bs or rounds' effects
# so I will only use B2, round2, B3 round 3
sim7<- boot.result[which(boot.result$n.set == 50 & boot.result$distribution.set == "normal" & boot.result$alpha.set == 0.02),]
################################################################

###################### B2, round2#####################
sim7.B2.round2 <- sim7[which(sim7$B.set == 100 & sim7$round.set == 100),]
sim7.nonPara.percentile.B2.round2  <- sim7.B2.round2[which(sim7$method.set == 1),]
sim7.nonPara.BCa.B2.round2 <- sim7.B2.round2[which(sim7$method.set == 2),]
sim7.Para.B2.round2 <- sim7.B2.round2[which(sim7$method.set == 3),]

print("Non-Parametric Percentile Method----------------")
summary(sim7.nonPara.percentile.B2.round2[c(2,3,4,5,6,10,11)])
print("Non-Parametric BCa------------------------------")
summary(sim7.nonPara.BCa.B2.round2[c(2,3,4,5,6,10,11)])
print("Parametric Percentile Method--------------------")
summary(sim7.Para.B2.round2[c(2,3,4,5,6,10,11)])
######summary result############
# > print("Non-Parametric Percentile Method----------------")
# [1] "Non-Parametric Percentile Method----------------"
# > summary(sim7.nonPara.percentile.B2.round2[c(2,3,4,5,6,10,11)])
# coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set   difference.set     length.set    
# Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :123.3   Min.   :122.7   Min.   :-0.840   Min.   :-2.057  
# 1st Qu.:0.230   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:123.3   1st Qu.:123.1   1st Qu.:-0.180   1st Qu.:-1.329  
# Median :0.480   Median :0.000   Median :0.000   Median :123.3   Median :123.3   Median :-0.002   Median :-1.197  
# Mean   :0.488   Mean   :0.013   Mean   :0.004   Mean   :123.3   Mean   :123.3   Mean   :-0.022   Mean   :-1.205  
# 3rd Qu.:0.740   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:123.3   3rd Qu.:123.5   3rd Qu.: 0.154   3rd Qu.:-1.075  
# Max.   :1.000   Max.   :0.940   Max.   :0.790   Max.   :123.3   Max.   :124.1   Max.   : 0.551   Max.   :-0.706  
# NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000   
# > print("Non-Parametric BCa------------------------------")
# [1] "Non-Parametric BCa------------------------------"
# > summary(sim7.nonPara.BCa.B2.round2[c(2,3,4,5,6,10,11)])
# coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set   difference.set     length.set    
# Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :123.3   Min.   :122.7   Min.   :-0.837   Min.   :-1.917  
# 1st Qu.:0.230   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:123.3   1st Qu.:123.1   1st Qu.:-0.179   1st Qu.:-1.321  
# Median :0.480   Median :0.000   Median :0.000   Median :123.3   Median :123.3   Median : 0.001   Median :-1.194  
# Mean   :0.488   Mean   :0.013   Mean   :0.004   Mean   :123.3   Mean   :123.3   Mean   :-0.022   Mean   :-1.204  
# 3rd Qu.:0.740   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:123.3   3rd Qu.:123.5   3rd Qu.: 0.153   3rd Qu.:-1.076  
# Max.   :1.000   Max.   :0.960   Max.   :0.780   Max.   :123.3   Max.   :124.1   Max.   : 0.552   Max.   :-0.656  
# NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000   
# > print("Parametric Percentile Method--------------------")
# [1] "Parametric Percentile Method--------------------"
# > summary(sim7.Para.B2.round2[c(2,3,4,5,6,10,11)])
# coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set   difference.set     length.set    
# Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :123.3   Min.   :122.8   Min.   :-0.840   Min.   :-1.939  
# 1st Qu.:0.230   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:123.3   1st Qu.:123.1   1st Qu.:-0.181   1st Qu.:-1.336  
# Median :0.480   Median :0.000   Median :0.000   Median :123.3   Median :123.3   Median :-0.003   Median :-1.205  
# Mean   :0.489   Mean   :0.013   Mean   :0.003   Mean   :123.3   Mean   :123.3   Mean   :-0.022   Mean   :-1.215  
# 3rd Qu.:0.740   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:123.3   3rd Qu.:123.5   3rd Qu.: 0.156   3rd Qu.:-1.084  
# Max.   :1.000   Max.   :0.980   Max.   :0.640   Max.   :123.3   Max.   :124.1   Max.   : 0.550   Max.   :-0.711  
# NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000   
##############hist############
hist(sim7.nonPara.percentile.B2.round2$coverage.set)
hist(sim7.nonPara.BCa.B2.round2$coverage.set)
hist(sim7.Para.B2.round2$coverage.set)
######summary conlclusion###################
# 1. coverage becomes normal
# 2. all mehtods' not coveraged means tend to be smaller than CI
# 3. boot mean no obvious difference
# 4. ci lenght  no obvious difference

################################################################

###################### B3, round3###################
sim7.B3.round3 <- sim7[which(sim7$B.set == 1000 & sim7$round.set == 1000),]
sim7.nonPara.percentile.B3.round3  <- sim7.B3.round3[which(sim7$method.set == 1),]
sim7.nonPara.BCa.B3.round3 <- sim7.B3.round3[which(sim7$method.set == 2),]
sim7.Para.B3.round3 <- sim7.B3.round3[which(sim7$method.set == 3),]

print("Non-Parametric Percentile Method----------------")
summary(sim7.nonPara.percentile.B3.round3[c(2,3,4,5,6,10,11)])
print("Non-Parametric BCa------------------------------")
summary(sim7.nonPara.BCa.B3.round3[c(2,3,4,5,6,10,11)])
print("Parametric Percentile Method--------------------")
summary(sim7.Para.B3.round3[c(2,3,4,5,6,10,11)])
######summary result#####################
# > print("Non-Parametric Percentile Method----------------")
# [1] "Non-Parametric Percentile Method----------------"
# > summary(sim7.nonPara.percentile.B3.round3[c(2,3,4,5,6,10,11)])
# coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set   difference.set     length.set    
# Min.   :0.001   Min.   :0       Min.   :0       Min.   :123.3   Min.   :123.4   Min.   :-0.207   Min.   :-1.551  
# 1st Qu.:0.251   1st Qu.:0       1st Qu.:0       1st Qu.:123.3   1st Qu.:123.5   1st Qu.:-0.167   1st Qu.:-1.403  
# Median :0.500   Median :0       Median :0       Median :123.3   Median :123.5   Median :-0.160   Median :-1.370  
# Mean   :0.500   Mean   :0       Mean   :0       Mean   :123.3   Mean   :123.5   Mean   :-0.160   Mean   :-1.371  
# 3rd Qu.:0.750   3rd Qu.:0       3rd Qu.:0       3rd Qu.:123.3   3rd Qu.:123.5   3rd Qu.:-0.154   3rd Qu.:-1.338  
# Max.   :1.000   Max.   :0       Max.   :0       Max.   :123.3   Max.   :123.5   Max.   :-0.124   Max.   :-1.200  
# NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000   
# > print("Non-Parametric BCa------------------------------")
# [1] "Non-Parametric BCa------------------------------"
# > summary(sim7.nonPara.BCa.B3.round3[c(2,3,4,5,6,10,11)])
# coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set   difference.set     length.set    
# Min.   :0.001   Min.   :0       Min.   :0       Min.   :123.3   Min.   :123.4   Min.   :-0.188   Min.   :-1.550  
# 1st Qu.:0.251   1st Qu.:0       1st Qu.:0       1st Qu.:123.3   1st Qu.:123.5   1st Qu.:-0.165   1st Qu.:-1.402  
# Median :0.500   Median :0       Median :0       Median :123.3   Median :123.5   Median :-0.160   Median :-1.370  
# Mean   :0.500   Mean   :0       Mean   :0       Mean   :123.3   Mean   :123.5   Mean   :-0.160   Mean   :-1.371  
# 3rd Qu.:0.750   3rd Qu.:0       3rd Qu.:0       3rd Qu.:123.3   3rd Qu.:123.5   3rd Qu.:-0.155   3rd Qu.:-1.338  
# Max.   :1.000   Max.   :0       Max.   :0       Max.   :123.3   Max.   :123.5   Max.   :-0.133   Max.   :-1.206  
# NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000   
# > print("Parametric Percentile Method--------------------")
# [1] "Parametric Percentile Method--------------------"
# > summary(sim7.Para.B3.round3[c(2,3,4,5,6,10,11)])
# coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set   difference.set     length.set    
# Min.   :0.001   Min.   :0       Min.   :0       Min.   :123.3   Min.   :123.4   Min.   :-0.194   Min.   :-1.579  
# 1st Qu.:0.251   1st Qu.:0       1st Qu.:0       1st Qu.:123.3   1st Qu.:123.5   1st Qu.:-0.167   1st Qu.:-1.419  
# Median :0.500   Median :0       Median :0       Median :123.3   Median :123.5   Median :-0.160   Median :-1.385  
# Mean   :0.500   Mean   :0       Mean   :0       Mean   :123.3   Mean   :123.5   Mean   :-0.160   Mean   :-1.387  
# 3rd Qu.:0.750   3rd Qu.:0       3rd Qu.:0       3rd Qu.:123.3   3rd Qu.:123.5   3rd Qu.:-0.154   3rd Qu.:-1.353  
# Max.   :1.000   Max.   :0       Max.   :0       Max.   :123.3   Max.   :123.5   Max.   :-0.124   Max.   :-1.227  
# NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000   
##############hist#####################
hist(sim7.nonPara.percentile.B3.round3$coverage.set)
hist(sim7.nonPara.BCa.B3.round3$coverage.set)
hist(sim7.Para.B3.round3$coverage.set)
######summary conlclusion###################
# 1. coverage becomes normal
# 2. all mehtods' not coveraged means tend to be balanced
# 3. boot mean no obvious difference
# 4. ci lenght  no obvious difference, m1 and m2 better a little

################################################################


#######simulation8###############################################
#### 

## analyse the different methods for n = 50, alpha = 0.02, and poisson distribution
# here I will not  compare the different Bs or rounds' effects
# so I will only use B2, round2, B3 round 3
sim8<- boot.result[which(boot.result$n.set == 50 & boot.result$distribution.set == "poisson" & boot.result$alpha.set == 0.02),]
#####################################################

############# B2, round2############
sim8.B2.round2 <- sim8[which(sim8$B.set == 100 & sim8$round.set == 100),]
sim8.nonPara.percentile.B2.round2  <- sim8.B2.round2[which(sim8$method.set == 1),]
sim8.nonPara.BCa.B2.round2 <- sim8.B2.round2[which(sim8$method.set == 2),]
sim8.Para.B2.round2 <- sim8.B2.round2[which(sim8$method.set == 3),]

print("Non-Parametric Percentile Method----------------")
summary(sim8.nonPara.percentile.B2.round2[c(2,3,4,5,6,10,11)])
print("Non-Parametric BCa------------------------------")
summary(sim8.nonPara.BCa.B2.round2[c(2,3,4,5,6,10,11)])
print("Parametric Percentile Method--------------------")
summary(sim8.Para.B2.round2[c(2,3,4,5,6,10,11)])
######summary result############
# > print("Non-Parametric Percentile Method----------------")
# [1] "Non-Parametric Percentile Method----------------"
# > summary(sim8.nonPara.percentile.B2.round2[c(2,3,4,5,6,10,11)])
# coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
# Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :9.8     Min.   : 8.688   Min.   :-1.107   Min.   :-3.121  
# 1st Qu.:0.238   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.8     1st Qu.: 9.446   1st Qu.:-0.168   1st Qu.:-2.061  
# Median :0.480   Median :0.000   Median :0.000   Median :9.8     Median : 9.704   Median : 0.096   Median :-1.861  
# Mean   :0.485   Mean   :0.007   Mean   :0.013   Mean   :9.8     Mean   : 9.718   Mean   : 0.082   Mean   :-1.873  
# 3rd Qu.:0.730   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:9.8     3rd Qu.: 9.968   3rd Qu.: 0.354   3rd Qu.:-1.681  
# Max.   :1.000   Max.   :0.610   Max.   :0.990   Max.   :9.8     Max.   :10.907   Max.   : 1.112   Max.   :-1.103  
# NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000    NA's   :22000   
# > print("Non-Parametric BCa------------------------------")
# [1] "Non-Parametric BCa------------------------------"
# > summary(sim8.nonPara.BCa.B2.round2[c(2,3,4,5,6,10,11)])
# coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
# Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :9.8     Min.   : 8.753   Min.   :-1.042   Min.   :-3.117  
# 1st Qu.:0.238   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.8     1st Qu.: 9.441   1st Qu.:-0.163   1st Qu.:-2.050  
# Median :0.470   Median :0.000   Median :0.000   Median :9.8     Median : 9.704   Median : 0.096   Median :-1.860  
# Mean   :0.484   Mean   :0.007   Mean   :0.014   Mean   :9.8     Mean   : 9.717   Mean   : 0.083   Mean   :-1.873  
# 3rd Qu.:0.730   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:9.8     3rd Qu.: 9.963   3rd Qu.: 0.359   3rd Qu.:-1.680  
# Max.   :1.000   Max.   :0.490   Max.   :0.970   Max.   :9.8     Max.   :10.842   Max.   : 1.047   Max.   :-1.091  
# NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000    NA's   :22000   
# > print("Parametric Percentile Method--------------------")
# [1] "Parametric Percentile Method--------------------"
# > summary(sim8.Para.B2.round2[c(2,3,4,5,6,10,11)])
# coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set    difference.set     length.set    
# Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :9.8     Min.   : 8.697   Min.   :-1.043   Min.   :-2.902  
# 1st Qu.:0.240   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:9.8     1st Qu.: 9.445   1st Qu.:-0.162   1st Qu.:-2.022  
# Median :0.490   Median :0.000   Median :0.000   Median :9.8     Median : 9.703   Median : 0.097   Median :-1.883  
# Mean   :0.494   Mean   :0.005   Mean   :0.006   Mean   :9.8     Mean   : 9.718   Mean   : 0.082   Mean   :-1.895  
# 3rd Qu.:0.740   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:9.8     3rd Qu.: 9.962   3rd Qu.: 0.355   3rd Qu.:-1.762  
# Max.   :1.000   Max.   :0.380   Max.   :0.670   Max.   :9.8     Max.   :10.843   Max.   : 1.103   Max.   :-1.282  
# NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000    NA's   :22000 
##############hist m3 has the lowest low coverage############
hist(sim8.nonPara.percentile.B2.round2$coverage.set)
hist(sim8.nonPara.BCa.B2.round2$coverage.set)
hist(sim8.Para.B2.round2$coverage.set)
######summary conlclusion###################
# 1. coverage becomes normal
# 2. all mehtods' not coveraged means tend to be balanced
# 3. boot mean no obvious difference
# 4. ci lenght  no obvious difference

################################################################

##################### B3, round3####################
sim8.B3.round3 <- sim8[which(sim8$B.set == 1000 & sim8$round.set == 1000),]
sim8.nonPara.percentile.B3.round3  <- sim8.B3.round3[which(sim8$method.set == 1),]
sim8.nonPara.BCa.B3.round3 <- sim8.B3.round3[which(sim8$method.set == 2),]
sim8.Para.B3.round3 <- sim8.B3.round3[which(sim8$method.set == 3),]

print("Non-Parametric Percentile Method----------------")
summary(sim8.nonPara.percentile.B3.round3[c(2,3,4,5,6,10,11)])
print("Non-Parametric BCa------------------------------")
summary(sim8.nonPara.BCa.B3.round3[c(2,3,4,5,6,10,11)])
print("Parametric Percentile Method--------------------")
summary(sim8.Para.B3.round3[c(2,3,4,5,6,10,11)])
######summary result####################
# > print("Non-Parametric Percentile Method----------------")
# [1] "Non-Parametric Percentile Method----------------"
# > summary(sim8.nonPara.percentile.B3.round3[c(2,3,4,5,6,10,11)])
# coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set   difference.set     length.set    
# Min.   :0.001   Min.   :0       Min.   :0       Min.   :9.8     Min.   :10.45   Min.   :-0.742   Min.   :-2.081  
# 1st Qu.:0.251   1st Qu.:0       1st Qu.:0       1st Qu.:9.8     1st Qu.:10.49   1st Qu.:-0.708   1st Qu.:-1.880  
# Median :0.500   Median :0       Median :0       Median :9.8     Median :10.50   Median :-0.700   Median :-1.840  
# Mean   :0.500   Mean   :0       Mean   :0       Mean   :9.8     Mean   :10.50   Mean   :-0.700   Mean   :-1.842  
# 3rd Qu.:0.750   3rd Qu.:0       3rd Qu.:0       3rd Qu.:9.8     3rd Qu.:10.51   3rd Qu.:-0.691   3rd Qu.:-1.800  
# Max.   :1.000   Max.   :0       Max.   :0       Max.   :9.8     Max.   :10.54   Max.   :-0.653   Max.   :-1.620  
# NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000   
# > print("Non-Parametric BCa------------------------------")
# [1] "Non-Parametric BCa------------------------------"
# > summary(sim8.nonPara.BCa.B3.round3[c(2,3,4,5,6,10,11)])
#   coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set   difference.set     length.set    
#  Min.   :0.001   Min.   :0       Min.   :0       Min.   :9.8     Min.   :10.46   Min.   :-0.734   Min.   :-2.110  
#  1st Qu.:0.251   1st Qu.:0       1st Qu.:0       1st Qu.:9.8     1st Qu.:10.49   1st Qu.:-0.704   1st Qu.:-1.882  
#  Median :0.500   Median :0       Median :0       Median :9.8     Median :10.50   Median :-0.698   Median :-1.840  
#  Mean   :0.500   Mean   :0       Mean   :0       Mean   :9.8     Mean   :10.50   Mean   :-0.697   Mean   :-1.841  
#  3rd Qu.:0.750   3rd Qu.:0       3rd Qu.:0       3rd Qu.:9.8     3rd Qu.:10.50   3rd Qu.:-0.691   3rd Qu.:-1.799  
#  Max.   :1.000   Max.   :0       Max.   :0       Max.   :9.8     Max.   :10.53   Max.   :-0.659   Max.   :-1.621  
#  NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000   
# > print("Parametric Percentile Method--------------------")
# [1] "Parametric Percentile Method--------------------"
# > summary(sim8.Para.B3.round3[c(2,3,4,5,6,10,11)])
# coverage.set    smaller.set      bigger.set     truemean.set    bootmean.set   difference.set     length.set    
# Min.   :0.001   Min.   :0       Min.   :0       Min.   :9.8     Min.   :10.44   Min.   :-0.755   Min.   :-2.402  
# 1st Qu.:0.251   1st Qu.:0       1st Qu.:0       1st Qu.:9.8     1st Qu.:10.49   1st Qu.:-0.709   1st Qu.:-2.160  
# Median :0.500   Median :0       Median :0       Median :9.8     Median :10.50   Median :-0.699   Median :-2.120  
# Mean   :0.500   Mean   :0       Mean   :0       Mean   :9.8     Mean   :10.50   Mean   :-0.700   Mean   :-2.112  
# 3rd Qu.:0.750   3rd Qu.:0       3rd Qu.:0       3rd Qu.:9.8     3rd Qu.:10.51   3rd Qu.:-0.690   3rd Qu.:-2.060  
# Max.   :1.000   Max.   :0       Max.   :0       Max.   :9.8     Max.   :10.55   Max.   :-0.643   Max.   :-1.840  
# NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000   NA's   :22000    NA's   :22000   
##############hist####################
hist(sim8.nonPara.percentile.B3.round3$coverage.set)
hist(sim8.nonPara.BCa.B3.round3$coverage.set)
hist(sim8.Para.B3.round3$coverage.set)
######summary conlclusion###################
# 1. coverage becomes normal
# 2. all mehtods' not coveraged means tend to be balanced
# 3. boot mean no obvious difference
# 4. ci lenght m1 and m2 are better, m1 and m2 no obvious difference

################################################################




