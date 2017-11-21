source('D:/MT4113/simulation_v2.r')

# generate a dataset
generateData <- function(type = 0, n = 30, lambda = 0, sd = 1, seed = 999) {
  #set.seed(seed)
  if(type == 0) { # normal
    data.set <-rnorm(n * 10,lambda,sd)
  } else { # poisson
    data.set <- rpois(n * 10 ,lambda)
  }
  newdata <- sample(data.set, n, replace = FALSE)
  return(newdata)
}


######################################################################

sim.data <- c(1:1000)
sim.data <- sample(sim.data,100)
simulation.v2(data = sim.data, lambda = mean(sim.data), alpha =  0.05,  round = 10, B = 1000)

#####################################################################
## simulation 1
sink('D:/MT4113/Simulation1.txt')
for(u in 1:200){
  print("                                                      ")
  sim.data <- generateData(n = 50, lambda = 9.888, sd = 4)
  mean(sim.data)
  print("    round 10     ")
  simulation.v2(data = sim.data, lambda = 9.888, round = 10)
  print("    round 100     ")
  simulation.v2(data = sim.data, lambda = 9.888, round = 100)
  print("                                                      ")
}

for(k in 1:10) {
  simulation.v2(data = sim.data, lambda = 9.888,  round = 1000)
}

sink()

#####################################################################
## simulation 2
sink('D:/MT4113/Simulation2.txt')
for(u in 1:200){
  print("                                                      ")
sim.data <- generateData(type = 1, n = 50, lambda = 9.8, sd = 2)
mean(sim.data)
print("    round 10     ")
simulation.v2(data = sim.data, lambda = 9.8, round = 10, distribution = "poisson")
print("    round 10     ")
simulation.v2(data = sim.data, lambda = 9.8, round = 100, distribution = "poisson")
print("                                                      ")
}
for(k in 1:10) {
simulation.v2(data = sim.data, lambda = 9.8,  round = 1000, distribution = "poisson")
}
sink()

#####################################################################
## simulation 3
sink('D:/MT4113/Simulation3.txt')
for(u in 1:200){
  print("                                                      ")
  
sim.data <- generateData(n = 10, lambda = 9.8, sd = 2)
mean(sim.data)
print("    round 10     ")
simulation.v2(data = sim.data, lambda = 9.8, round = 10)
print("    round 100     ")
simulation.v2(data = sim.data, lambda = 9.8, round = 100)
print("                                                      ")
}

for(k in 1:10) {
simulation.v2(data = sim.data, lambda = 9.8,  round = 1000)
}
sink()

#####################################################################
## simulation 4
sink('D:/MT4113/Simulation4.txt')
for(u in 1:200){
  print("                                                      ")
sim.data <- generateData(type = 1, n = 10, lambda = 9.8, sd = 2)
mean(sim.data)
print("    round 10     ")
simulation.v2(data = sim.data, lambda = 9.8, round = 10, distribution = "poisson")
print("    round 100     ")
simulation.v2(data = sim.data, lambda = 9.8, round = 100, distribution = "poisson")
print("                                                      ")
}
for(k in 1:10) {
simulation.v2(data = sim.data, lambda = 9.8,  round = 1000, distribution = "poisson")
}
sink()
#####################################################################
## simulation 5
sink('D:/MT4113/Simulation5.txt')
for(u in 1:200){
print("                                                      ")
sim.data <- generateData(n = 50, lambda = 9.8, sd = 2)
mean(sim.data)
print("    round 10     ")
simulation.v2(data = sim.data, lambda = 9.8, round = 10, alpha = 0.01)
print("    round 100     ")
simulation.v2(data = sim.data, lambda = 9.8, round = 100, alpha = 0.01)

print("                                                      ")
}
  for(k in 1:10) {
simulation.v2(data = sim.data, lambda = 9.8,  round = 1000, alpha = 0.01)
  }
sink()
#####################################################################
## simulation 6
sink('D:/MT4113/Simulation6.txt')
for(u in 1:200){
  print("                                                      ")
sim.data <- generateData(type = 1, n = 50, lambda = 9.8, sd = 2)
mean(sim.data)
print("    round 10     ")
simulation.v2(data = sim.data, lambda = 9.8, round = 10, distribution = "poisson", alpha = 0.01)

print("    round 100     ")
simulation.v2(data = sim.data, lambda = 9.8, round = 100, distribution = "poisson", alpha = 0.01)

print("                                                      ")
}
    for(k in 1:10) {
simulation.v2(data = sim.data, lambda = 9.8,  round = 1000, distribution = "poisson", alpha = 0.01)

    }
sink()
#####################################################################
## simulation 7
sink('D:/MT4113/Simulation7.txt')
for(u in 1:200){
  print("                                                      ")
sim.data <- generateData(n = 50, lambda = 123.3, sd = 2)
mean(sim.data)
print("    round 10     ")
simulation.v2(data = sim.data, lambda = 123.3, round = 10, alpha = 0.02)
print("    round 100     ")
simulation.v2(data = sim.data, lambda = 123.3, round = 100, alpha = 0.02)
print("                                                      ")
}

for(k in 1:10) {
simulation.v2(data = sim.data, lambda = 123.3,  round = 1000, alpha = 0.02)
      }
sink()
#####################################################################
## simulation 8
sink('D:/MT4113/Simulation8.txt')
for(u in 1:200){
  print("                                                      ")
sim.data <- generateData(type = 1, n = 50, lambda = 9.8, sd = 2)
mean(sim.data)
print("    round 10     ")
simulation.v2(data = sim.data, lambda = 9.8, round = 10, distribution = "poisson", alpha = 0.02)
print("    round 100    ")
simulation.v2(data = sim.data, lambda = 9.8, round = 100, distribution = "poisson", alpha = 0.02)
print("                                                      ")
}
for(k in 1:10) {
simulation.v2(data = sim.data, lambda = 9.8,  round = 1000, distribution = "poisson", alpha = 0.02)

}
sink()