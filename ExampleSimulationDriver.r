#Some example code to drive the functions in ExampleSimulation.r
source("ExampleSimulation.r")

#Run all the simulations for a range of trim levels from 0 to 0.5
alpha.vec<-seq(0,0.5,length=24)

#All simulations have a large sample size
n<-300
#All simulations have 1000 reps
M<-1000

#Try a simulation where all the distribution comes from the normal
res<-do.sim(mix.par=1,alpha.vec=alpha.vec,M=M,n=n)
res
#What do we expect this to look like.  Does it?
plot(alpha.vec,res$MSE,type="b",main="Simulation from normal distribution",xlab="level of trimming (alpha)",ylab="MSE")
#(could add code to print the alpha for which the MSE is minimized; could put the output generating code
#  into some post-processing function; etc etc...)

#Try a simulation where all the distribution comes from the t part
res<-do.sim(mix.par=0,alpha.vec=alpha.vec,M=M,n=n)
#What do we expect this to look like.  Does it?
plot(alpha.vec,res$MSE,type="b",main="Simulation from t-distribution",xlab="level of trimming (alpha)",ylab="MSE")

#Now try a half-and-half mixture
res<-do.sim(mix.par=0.5,alpha.vec=alpha.vec,M=M,n=n)
#What do we expect this to look like.  Does it?
pdf("mix_out.pdf",width=8,height=5)
plot(alpha.vec,res$MSE,type="b",main="Simulation from mixture-distribution (mix.par=0.5)",xlab="level of trimming (alpha)",ylab="MSE")
dev.off()

#Lots more could be done with the simluation code, but I'll stop for now!...
