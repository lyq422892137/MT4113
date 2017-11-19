n<-100; B<-10000
#Ensure same results each time (because I'm going to demonstrate a 2nd coding of this)
set.seed(61349)
#Simulate data
x<-rnorm(n,0,10)
#Report Standard error from data
print(sd(x)/sqrt(n))

#Create bootstrap datasets and store the means
x.star.means<-numeric(B)
for(i in 1:B){
  x.boot<-sample(x,n,replace=TRUE)
  x.star.means[i]<-mean(x.boot)
} 
#Calculate standard error of bootstrap means
se.star<-sd(x.star.means)
print(se.star)

#########################################

n<-100; B<-10000
set.seed(61349)
#Simulate data
x<-rnorm(n,0,10)
#Report Standard error from data
print(sd(x)/sqrt(n))


#Create bootstrap datasets
x.star<-replicate(B,x[sample.int(n,replace=TRUE)])
#Calculate standard error over bootstrap reps
x.star.means<-apply(x.star,2,mean)
se.star<-sd(x.star.means)
print(se.star)


###############################################
n<-100; B<-10000
#Simulate data
x<-rnorm(n,0,10)
#Create bootstrap datasets
x.star<-replicate(B,x[sample.int(n,replace=TRUE)])
#Calculate standard error over bootstrap reps
x.star.meds<-apply(x.star,2,median)
se.star<-sd(x.star.meds)
print(se.star)


###############################################
n<-100; B<-10000
#Simulate data
x<-rnorm(n,0,10)
#Create bootstrap datasets
x.star<-replicate(B,x[sample.int(n,replace=TRUE)])
#Calculate standard error over bootstrap reps
x.star.vars<-apply(x.star,2,var)
se.star<-sd(x.star.vars)
print(se.star)



##################################################
n<-100; B<-10000
#Simulate data
x<-rnorm(n,0,10)


#Create bootstrap datasets
x.star<-replicate(B,x[sample.int(n,replace=TRUE)])
x.star.means<-apply(x.star,1,mean)
#Estimate bias
b.hat<-mean(x.star.means)-mean(x)
print(b.hat)


####################################################

biased.mean<-function(x) return(mean(x)+10)
n<-100; B<-10000
#Simulate data
x<-rnorm(n,0,10)
#Create bootstrap datasets
x.star<-replicate(B,x[sample.int(n,replace=TRUE)])
x.star.means<-apply(x.star,1,biased.mean)
#Estimate bias
b.hat<-mean(x.star.means)-mean(x)
print(b.hat)
