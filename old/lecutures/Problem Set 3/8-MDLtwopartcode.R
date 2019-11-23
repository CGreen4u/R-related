#==============================================
# Ellis Scharfenaker
# ECON 5529
#==============================================
# MDL: Two Part Code for regression


setwd("~/Dropbox/UMKC/ECON5529/R")
D<-read.csv("KV.csv")
attach(D)
plot(log(D$X),log(D$L))
plot(rnorm(200))

library(rstan)
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
model {
  y~ normal(beta0 + beta1*x, sigma);
}
'
set.seed(123)
STAN<-stan(model_code=reg1.model,data=DD,iter=1000,chains=3)
params1<-extract(STAN)
b0<-mean(params1$beta0)
b1<-mean(params1$beta1)
errors1<-L-exp(b0)*X^b1
sqrt(mean(errors1^2)) # Model has a root mean squared error of 3,956,067 people
errorsd1<-sd(errors1)
plot(errors1,type="l")
abline(h=0)

# If we were to code each individual observation using the rule 
# that one floating point number takes 16 bytes (128 bits) to code, then the
# employment data would take 32,768 bits to code.
128*length(L)

# Using the standard normal distribution to calculate the likelihood, 
# we can see that the length of the second part of the two-part code,
# representing the log likelihood, is:
two<-ceiling(-sum(log(dnorm(errors1,0,errorsd1),base=2)))

# The length of the first part of the two-part code is number of bits 
# required to transmit the parameters.
one<-2*128+ceiling(log(errorsd1,base=2))

one+two
# Thus compared to simply transmitting the employment data directly, 
# which costs 32768 bits, the two part code requires 268+3583=3851 bits, 
# saving 28917 bits, and compressing the data by over 88%.

# Adding a cubic term.
reg2.model<-'
data {
  int<lower=0> N;
  vector[N] x;
  vector[N] y;
}
parameters {
  real beta0;
  real beta1;
  real beta2;
  real<lower=0> sigma;
} model {
  for(n in 1:N)
  y[n] ~ normal(beta0 + beta1*x[n] + beta2*pow(x[n],3), sigma);
}'

set.seed(123)
STAN<-stan(model_code=reg2.model,data=DD,iter=1000,chains=3)
params2<-extract(STAN)
b0<-mean(params2$beta0)
b1<-mean(params2$beta1)
b2<-mean(params2$beta2)
errors2<-L-exp(b0)*X^b1*exp(b2*log(X)^3)
plot(X,L,pch=20)
curve(exp(b0)*x^b1*exp(b2*log(x-8.486)^3),add=T,col="red",lwd=2)

plot(errors1,type="l")
lines(errors2,col="red")
abline(h=0)

errorsd2<-sd(errors2)

# The length of the second part of the two part code is now 3498 compared
# to 3583, so our data fits the model better. Thus the additional term  
# saves us 3583-3490=93 bits
two<-ceiling(-sum(log(dnorm(errors2,0,errorsd2),base=2)))

# The length of the first part of the two-part code is number of bits 
# required to transmit the parameters which now includes the cubic term,
# hence it now costs 396 bits compared to the 268 bits without the additional
# model complexity.

one<-3*128+ceiling(log(errorsd2,base=2))

# The additional parameter does not justify the additional 128 bit cost
one+two

