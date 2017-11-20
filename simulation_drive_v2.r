source('D:/MT4113/simulation_v2.r')

# generate a dataset
generateData <- function(type = 0, n = 30, lambda = 0, sd = 1, seed = 999) {
  #set.seed(seed)
  if(type == 0) { # normal
    data.set <-rnorm(n,lambda,sd)
  } else { # poisson
    data.set <- rpois(n,lambda)
  }
  newdata <- sample(data.set, n)
  return(data.set)
}


######################################################################

sim.data <- c(1:1000)
sim.data <- sample(sim.data,100)
simulation.v2(data = sim.data, lambda = mean(sim.data), alpha =  0.05,  round = 10, B = 1000)

#####################################################################
## simulation 1
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




simulation.v2(data = sim.data, lambda = 9.888,  round = 1000)

#####################################################################
## simulation 2


sim.data <- generateData(type = 1, n = 50, lambda = 9.8, sd = 2)
mean(sim.data)

simulation.v2(data = sim.data, lambda = 9.8, round = 100, distribution = "poisson")

simulation.v2(data = sim.data, lambda = 9.8,  round = 1000, distribution = "poisson")

#####################################################################
## simulation 3

sim.data <- generateData(n = 10, lambda = 9.8, sd = 2)
mean(sim.data)

simulation.v2(data = sim.data, lambda = 9.8, round = 100)

simulation.v2(data = sim.data, lambda = 9.8,  round = 1000)


#####################################################################
## simulation 4

sim.data <- generateData(type = 1, n = 10, lambda = 9.8, sd = 2)
mean(sim.data)

simulation.v2(data = sim.data, lambda = 9.8, round = 100, distribution = "poisson")

simulation.v2(data = sim.data, lambda = 9.8,  round = 1000, distribution = "poisson")


#####################################################################
## simulation 5

sim.data <- generateData(n = 50, lambda = 9.8, sd = 2)
mean(sim.data)

simulation.v2(data = sim.data, lambda = 9.8, round = 100, alpha = 0.01)

simulation.v2(data = sim.data, lambda = 9.8,  round = 1000, alpha = 0.01)

#####################################################################
## simulation 6

sim.data <- generateData(type = 1, n = 50, lambda = 9.8, sd = 2)
mean(sim.data)

simulation.v2(data = sim.data, lambda = 9.8, round = 100, distribution = "poisson", alpha = 0.01)

simulation.v2(data = sim.data, lambda = 9.8,  round = 1000, distribution = "poisson", alpha = 0.01)


#####################################################################
## simulation 7

sim.data <- generateData(n = 50, lambda = 9.8, sd = 2)
mean(sim.data)

simulation.v2(data = sim.data, lambda = 9.8, round = 100, alpha = 0.02)

simulation.v2(data = sim.data, lambda = 9.8,  round = 1000, alpha = 0.02)

#####################################################################
## simulation 8

sim.data <- generateData(type = 1, n = 50, lambda = 9.8, sd = 2)
mean(sim.data)

simulation.v2(data = sim.data, lambda = 9.8, round = 100, distribution = "poisson", alpha = 0.02)

simulation.v2(data = sim.data, lambda = 9.8,  round = 1000, distribution = "poisson", alpha = 0.02)


