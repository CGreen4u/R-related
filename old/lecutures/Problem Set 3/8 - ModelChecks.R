##########################################################
# ECON 5529: Applied Bayesian Statistics for Economcis   #
# UMKC, Fall 2017                                        #
# Ellis Scharfenaker                                     #
# Lecture 8                                              #
##########################################################
library(shinystan)
library(rstan)


D<-read.csv(file.choose())
attach(D)
DD<-list(N=length(X),y=log(L),x=log(X))

reg1.model<-'
data {
  int<lower=0> N;
  vector[N] x;
  vector[N] y;
} 
parameters {
  real beta0;
  real beta1;
  real<lower=0> sigma;
} 
transformed parameters {
  vector[N] x_hat; //xhat is a length, N vector of integers
for (i in 1:N)
x_hat[i] = A[country[i]] + a[country[i]] * k[i];

model {
  y~ normal(beta0 + beta1*x, sigma);
}
'
set.seed(123)
STAN<-stan(model_code=reg1.model,data=DD,iter=10000,chains=3)
launch_shinystan(STAN)

# Extract MCMC samples
params1<-extract(STAN)
b0<-params1$beta0
b1<-params1$beta1
sigmas<- params1$sigma
nsims <- length(params1$sigma)

# produce the replications from posterior and inspect
N<-length(X)
yRep <- sapply(1:nsims, function(i) rnorm(N, b0+b1*log(X), sigmas[i])) 


# Check min, max, and mean
min_rep <- apply(yRep, 2, min)
max_rep <- apply(yRep,2,max)
mean_rep <- apply(yRep,2,mean)
sd_rep <- apply(yRep,2,sd)


# Plot posterior mins against actual min
hist(min_rep, main='',breaks = 50)
abline(v=min(log(L)),lwd=3) 

# Plot posterior maxs against actual maxs
hist(max_rep, main='',xlim=c(11.8,12.05),breaks = 50)
abline(v=max(log(L)),lwd=3) 

# Plot posterior sds against actual sds
hist(sd_rep, main='',breaks = 50)
abline(v=sd(log(L)),lwd=3) 

# Plot predicted data
hist(log(L),breaks=50,prob=T,xlim=c(10.6,12),col="red")

# Compare to predicted data
for(i in 1:50){
  lines(density(yRep[,i]))
}

#-------------------------------------------------------
# Bayes Factor
#-------------------------------------------------------

n<-100
y <- 0:n
plot(y,dbinom(y,n,0.7),type="h",col="red")
lines(y,dbinom(y,n,0.6),type="h",col="blue")

dbinom(62,n,0.6)
dbinom(62,n,0.7)

plot(y, dbinom(y, n, 0.7), type = "h",col="red")
lines(y, dbinom(y, n, 0.6), type = "h",col="blue")
segments(62,0,62,dbinom(62, n, 0.6),lwd=3,col="blue")
segments(62,0,62,dbinom(62, n, 0.7),lwd=3,col="red")

dbinom(62, n, 0.6)/dbinom(62, n, 0.7)

head(data)
head(yrep)
list(growth)
