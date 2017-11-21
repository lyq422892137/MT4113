filepath <- "D:/MT4113/Info2.txt"
boot.result <- read.table(file = filepath, header = TRUE)
# wangle °¢¶û·¨£¬xiacijiashang


# B1 : B =10
# B2 : B ==100
# B3: B ==1000
# round1: round = 10
# round2 : round = 100
# round3 : round = 1000

######################################################
#### simulation1

## analyse the different methods for n = 50, alpha = 0.05, and normal distribution
# here I will compare the different Bs or rounds' effects
sim1<- boot.result[which(boot.result$n.set == 50 & boot.result$distribution.set == "normal"),]

# B1, round1
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

hist(sim1.nonPara.percentile.B1.round1$coverage.set)
hist(sim1.nonPara.BCa.B1.round1$coverage.set)
hist(sim1.Para.B1.round1$coverage.set)

# B1, round2
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

hist(sim1.nonPara.percentile.B1.round2$coverage.set)
hist(sim1.nonPara.BCa.B1.round2$coverage.set)
hist(sim1.Para.B1.round1$coverage.set)

# B2, round1
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

hist(sim1.nonPara.percentile.B2.round1$coverage.set)
hist(sim1.nonPara.BCa.B2.round1$coverage.set)
hist(sim1.Para.B2.round1$coverage.set)

# B2, round2
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

hist(sim1.nonPara.percentile.B2.round2$coverage.set)
hist(sim1.nonPara.BCa.B2.round2$coverage.set)
hist(sim1.Para.B2.round2$coverage.set)

# B3, round3
sim1.B3.round3 <- sim1[which(sim1$B.set == 100 & sim1$round.set == 10),]
sim1.nonPara.percentile.B3.round3  <- sim1.B3.round3[which(sim1$method.set == 1),]
sim1.nonPara.BCa.B3.round3 <- sim1.B3.round3[which(sim1$method.set == 2),]
sim1.Para.B3.round3 <- sim1.B3.round3[which(sim1$method.set == 3),]

print("Non-Parametric Percentile Method----------------")
summary(sim1.nonPara.percentile.B3.round3[c(2,3,4,5,6,10,11)])
print("Non-Parametric BCa------------------------------")
summary(sim1.nonPara.BCa.B3.round3[c(2,3,4,5,6,10,11)])
print("Parametric Percentile Method--------------------")
summary(sim1.Para.B3.round3[c(2,3,4,5,6,10,11)])

hist(sim1.nonPara.percentile.B3.round3$coverage.set)
hist(sim1.nonPara.BCa.B3.round3$coverage.set)
hist(sim1.Para.B3.round3$coverage.set)

######################################################
#### simulation2

## analyse the different methods for n = 50, alpha = 0.05, and poisson distribution
# here I will compare the different Bs or rounds' effects
sim2<- boot.result[which(boot.result$n.set == 50 & boot.result$distribution.set == "poisson"),]

# B1, round1
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

hist(sim2.nonPara.percentile.B1.round1$coverage.set)
hist(sim2.nonPara.BCa.B1.round1$coverage.set)
hist(sim2.Para.B1.round1$coverage.set)

# B1, round2
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

hist(sim2.nonPara.percentile.B1.round2$coverage.set)
hist(sim2.nonPara.BCa.B1.round2$coverage.set)
hist(sim2.Para.B1.round1$coverage.set)

# B2, round1
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

hist(sim2.nonPara.percentile.B2.round1$coverage.set)
hist(sim2.nonPara.BCa.B2.round1$coverage.set)
hist(sim2.Para.B2.round1$coverage.set)

# B2, round2
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

hist(sim2.nonPara.percentile.B2.round2$coverage.set)
hist(sim2.nonPara.BCa.B2.round2$coverage.set)
hist(sim2.Para.B2.round2$coverage.set)

# B3, round3
sim2.B3.round3 <- sim2[which(sim2$B.set == 100 & sim2$round.set == 10),]
sim2.nonPara.percentile.B3.round3  <- sim2.B3.round3[which(sim2$method.set == 1),]
sim2.nonPara.BCa.B3.round3 <- sim2.B3.round3[which(sim2$method.set == 2),]
sim2.Para.B3.round3 <- sim2.B3.round3[which(sim2$method.set == 3),]

print("Non-Parametric Percentile Method----------------")
summary(sim2.nonPara.percentile.B3.round3[c(2,3,4,5,6,10,11)])
print("Non-Parametric BCa------------------------------")
summary(sim2.nonPara.BCa.B3.round3[c(2,3,4,5,6,10,11)])
print("Parametric Percentile Method--------------------")
summary(sim2.Para.B3.round3[c(2,3,4,5,6,10,11)])

hist(sim2.nonPara.percentile.B3.round3$coverage.set)
hist(sim2.nonPara.BCa.B3.round3$coverage.set)
hist(sim2.Para.B3.round3$coverage.set)


######################################################
#### simulation3

## analys for n = 10, alpha = 0.05, and normal distribution
# here I will compare the different Bs or rounds' effects
sim3<- boot.result[which(boot.result$n.set == 10 & boot.result$distribution.set == "normal"),]

# B1, round1
sim3.B1.round1 <- sim3[which(sim3$B.set == 10 & sim3$round.set == 10),]
sim3.nonPara.percentile.B1.round1  <- sim3.B1.round1[which(sim3$method.set == 1),]
sim3.nonPara.BCa.B1.round1 <- sim3.B1.round1[which(sim3$method.set == 2),]
sim3.Para.B1.round1 <- sim3.B1.round1[which(sim3$method.set == 3),]

print("Non-Parametric Percentile Method----------------")
summary(sim3.nonPara.percentile.B1.round1[c(2,3,4,5,6,10,11)])
print("Non-Parametric BCa------------------------------")
summary(sim3.nonPara.BCa.B1.round1[c(2,3,4,5,6,10,11)])
print("Parametric Percentile Method--------------------")
summary(sim3.Para.B1.round1[c(2,3,4,5,6,10,11)])

hist(sim3.nonPara.percentile.B1.round1$coverage.set)
hist(sim3.nonPara.BCa.B1.round1$coverage.set)
hist(sim3.Para.B1.round1$coverage.set)

# B1, round2
sim3.B1.round2 <- sim3[which(sim3$B.set == 10 & sim3$round.set == 100),]
sim3.nonPara.percentile.B1.round2  <- sim3.B1.round2[which(sim3$method.set == 1),]
sim3.nonPara.BCa.B1.round2 <- sim3.B1.round2[which(sim3$method.set == 2),]
sim3.Para.B1.round2 <- sim3.B1.round2[which(sim3$method.set == 3),]

print("Non-Parametric Percentile Method----------------")
summary(sim3.nonPara.percentile.B1.round2[c(2,3,4,5,6,10,11)])
print("Non-Parametric BCa------------------------------")
summary(sim3.nonPara.BCa.B1.round2[c(2,3,4,5,6,10,11)])
print("Parametric Percentile Method--------------------")
summary(sim3.Para.B1.round2[c(2,3,4,5,6,10,11)])

hist(sim3.nonPara.percentile.B1.round2$coverage.set)
hist(sim3.nonPara.BCa.B1.round2$coverage.set)
hist(sim3.Para.B1.round1$coverage.set)

# B2, round1
sim3.B2.round1 <- sim3[which(sim3$B.set == 100 & sim3$round.set == 10),]
sim3.nonPara.percentile.B2.round1  <- sim3.B2.round1[which(sim3$method.set == 1),]
sim3.nonPara.BCa.B2.round1 <- sim3.B2.round1[which(sim3$method.set == 2),]
sim3.Para.B2.round1 <- sim3.B2.round1[which(sim3$method.set == 3),]

print("Non-Parametric Percentile Method----------------")
summary(sim3.nonPara.percentile.B2.round1[c(2,3,4,5,6,10,11)])
print("Non-Parametric BCa------------------------------")
summary(sim3.nonPara.BCa.B2.round1[c(2,3,4,5,6,10,11)])
print("Parametric Percentile Method--------------------")
summary(sim3.Para.B2.round1[c(2,3,4,5,6,10,11)])

hist(sim3.nonPara.percentile.B2.round1$coverage.set)
hist(sim3.nonPara.BCa.B2.round1$coverage.set)
hist(sim3.Para.B2.round1$coverage.set)

# B2, round2
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

hist(sim3.nonPara.percentile.B2.round2$coverage.set)
hist(sim3.nonPara.BCa.B2.round2$coverage.set)
hist(sim3.Para.B2.round2$coverage.set)

# B3, round3
sim3.B3.round3 <- sim3[which(sim3$B.set == 100 & sim3$round.set == 10),]
sim3.nonPara.percentile.B3.round3  <- sim3.B3.round3[which(sim3$method.set == 1),]
sim3.nonPara.BCa.B3.round3 <- sim3.B3.round3[which(sim3$method.set == 2),]
sim3.Para.B3.round3 <- sim3.B3.round3[which(sim3$method.set == 3),]

print("Non-Parametric Percentile Method----------------")
summary(sim3.nonPara.percentile.B3.round3[c(2,3,4,5,6,10,11)])
print("Non-Parametric BCa------------------------------")
summary(sim3.nonPara.BCa.B3.round3[c(2,3,4,5,6,10,11)])
print("Parametric Percentile Method--------------------")
summary(sim3.Para.B3.round3[c(2,3,4,5,6,10,11)])

hist(sim3.nonPara.percentile.B3.round3$coverage.set)
hist(sim3.nonPara.BCa.B3.round3$coverage.set)
hist(sim3.Para.B3.round3$coverage.set)

#####################################################
#### simulation4

## analys for n = 10, alpha = 0.05, and poisson distribution
# here I will compare the different Bs or rounds' effects
sim4<- boot.result[which(boot.result$n.set == 10 & boot.result$distribution.set == "poisson"),]

# B1, round1
sim4.B1.round1 <- sim4[which(sim4$B.set == 10 & sim4$round.set == 10),]
sim4.nonPara.percentile.B1.round1  <- sim4.B1.round1[which(sim4$method.set == 1),]
sim4.nonPara.BCa.B1.round1 <- sim4.B1.round1[which(sim4$method.set == 2),]
sim4.Para.B1.round1 <- sim4.B1.round1[which(sim4$method.set == 3),]

print("Non-Parametric Percentile Method----------------")
summary(sim4.nonPara.percentile.B1.round1[c(2,3,4,5,6,10,11)])
print("Non-Parametric BCa------------------------------")
summary(sim4.nonPara.BCa.B1.round1[c(2,3,4,5,6,10,11)])
print("Parametric Percentile Method--------------------")
summary(sim4.Para.B1.round1[c(2,3,4,5,6,10,11)])

hist(sim4.nonPara.percentile.B1.round1$coverage.set)
hist(sim4.nonPara.BCa.B1.round1$coverage.set)
hist(sim4.Para.B1.round1$coverage.set)

# B1, round2
sim4.B1.round2 <- sim4[which(sim4$B.set == 10 & sim4$round.set == 100),]
sim4.nonPara.percentile.B1.round2  <- sim4.B1.round2[which(sim4$method.set == 1),]
sim4.nonPara.BCa.B1.round2 <- sim4.B1.round2[which(sim4$method.set == 2),]
sim4.Para.B1.round2 <- sim4.B1.round2[which(sim4$method.set == 3),]

print("Non-Parametric Percentile Method----------------")
summary(sim4.nonPara.percentile.B1.round2[c(2,3,4,5,6,10,11)])
print("Non-Parametric BCa------------------------------")
summary(sim4.nonPara.BCa.B1.round2[c(2,3,4,5,6,10,11)])
print("Parametric Percentile Method--------------------")
summary(sim4.Para.B1.round2[c(2,3,4,5,6,10,11)])

hist(sim4.nonPara.percentile.B1.round2$coverage.set)
hist(sim4.nonPara.BCa.B1.round2$coverage.set)
hist(sim4.Para.B1.round1$coverage.set)

# B2, round1
sim4.B2.round1 <- sim4[which(sim4$B.set == 100 & sim4$round.set == 10),]
sim4.nonPara.percentile.B2.round1  <- sim4.B2.round1[which(sim4$method.set == 1),]
sim4.nonPara.BCa.B2.round1 <- sim4.B2.round1[which(sim4$method.set == 2),]
sim4.Para.B2.round1 <- sim4.B2.round1[which(sim4$method.set == 3),]

print("Non-Parametric Percentile Method----------------")
summary(sim4.nonPara.percentile.B2.round1[c(2,3,4,5,6,10,11)])
print("Non-Parametric BCa------------------------------")
summary(sim4.nonPara.BCa.B2.round1[c(2,3,4,5,6,10,11)])
print("Parametric Percentile Method--------------------")
summary(sim4.Para.B2.round1[c(2,3,4,5,6,10,11)])

hist(sim4.nonPara.percentile.B2.round1$coverage.set)
hist(sim4.nonPara.BCa.B2.round1$coverage.set)
hist(sim4.Para.B2.round1$coverage.set)

# B2, round2
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

hist(sim4.nonPara.percentile.B2.round2$coverage.set)
hist(sim4.nonPara.BCa.B2.round2$coverage.set)
hist(sim4.Para.B2.round2$coverage.set)

# B3, round3
sim4.B3.round3 <- sim4[which(sim4$B.set == 100 & sim4$round.set == 10),]
sim4.nonPara.percentile.B3.round3  <- sim4.B3.round3[which(sim4$method.set == 1),]
sim4.nonPara.BCa.B3.round3 <- sim4.B3.round3[which(sim4$method.set == 2),]
sim4.Para.B3.round3 <- sim4.B3.round3[which(sim4$method.set == 3),]

print("Non-Parametric Percentile Method----------------")
summary(sim4.nonPara.percentile.B3.round3[c(2,3,4,5,6,10,11)])
print("Non-Parametric BCa------------------------------")
summary(sim4.nonPara.BCa.B3.round3[c(2,3,4,5,6,10,11)])
print("Parametric Percentile Method--------------------")
summary(sim4.Para.B3.round3[c(2,3,4,5,6,10,11)])

hist(sim4.nonPara.percentile.B3.round3$coverage.set)
hist(sim4.nonPara.BCa.B3.round3$coverage.set)
hist(sim4.Para.B3.round3$coverage.set)





