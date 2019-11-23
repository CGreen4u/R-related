# A Bank Has Made 100 mortages of a new type, and all have been oustanding 5 years. of these 100, 5
# have defaulted. the bank would like to estimate the probability of (theta) of default in the first 
# five years this type of mortage, and get some idea of how much uncertainty there is about the
# the probability, given the observed data. these being a new type of mortgage, the bank assigns
# a uniform prior over (theta).

# What is the likelihood of P(x|theat)?

library("rmeta")


library(manipulate)
# Programming the kernel of the Binomial distribution
binom.kern <- function(theta,n,k){
  (theta^k)*(1-theta)^(n-k)
}

# Programming the kernel of the Beta distribution
beta.kern <- function(theta,a,b){
  (theta)^(a-1)*(1-theta)^(b-1)
}
# Programming the kernel of the Binomial distribution
binom<-function(theta,n,k){
  choose(n,k)*(theta)^(k)*(1-theta)^(n-k)}

k<-5
n<-100


# Prior
curve(dbeta(x,5,100),xlab=expression(theta),
      ylab=expression(paste("p[",theta,"]")),main="Beta Prior")

# Likelihood
curve(dbinom(5,100,x),xlab=expression(theta),
      ylab=expression(paste("p[x|",theta,"]")),main="Binomial Likelihood")


#Find max likelihood
# define the log likelihood:

log.post<-function(p,n,k){
  log(choose(n,k)*(p^k)*(1-p)^(n-k))
} 

# then create a set of values with which to evaluate that function
pvals<-seq(0,1,.0001)

# use the optimize function, once log.post and pvals are in
optimize(log.post,interval=c(0,1),n, k,maximum=T)

n<- 10
k<-3

# Plot the function using "curve." x is variable you plot over
?curve
curve(binom.kern(x,100,5), from=0, to=1, xlab="x",ylab="p[x|theta]", main="Posterior Distribution")


# Use quantile function to get confidence interval
ci <- qbeta(c(0.025,0.975),(5+1),(100-5)+1)

# Add vertical CI lines
abline(v=ci[1],lty=2)
abline(v=ci[2],lty=2)



# Shortest interval
library(TeachingDemos)

TeachingDemos::hpd(qbeta(c(0.025,0.975),(5+1),(100-5)+1),conf = .95,ci[1],ci[2])


#-----------------------------------------------------------------------

#Queston 2

#P(x|G,R)
#r=red
#g=green=100-r
#P(g) 1-p(r)

n <-length(x) # number of data
r <-

rnorm(100,50,1)
x<-rnorm(100)
plot(x)
plot(x,type="l")

# The likelihood for theta is the Negative Binomial distribution
neg.binom<-function(theta,n,k){
  choose(n-1,k-1)*(theta)^(k)*(1-theta)^(n-k)}

# The likelihood for theta is the Binomial distribution
curve(binom(x,100,50),xlab=expression(theta),ylab=expression(paste("p[x|",theta,"]")),main="Binomial Likelihood")

# Test Null of theta=0.5
sum(sapply(0:5, function(i) choose(100,i)))*0.5^100
# Null cannot be rejected at the 10% "significance" level



-----------
  # The likelihood for theta is the Negative Binomial distribution
  neg.binom<-function(theta,n,k){
    choose(n-1,k-1)*(theta)^(k)*(1-theta)^(n-k)}

curve(neg.binom(x,100,100),xlab=expression(theta),ylab=expression(paste("p[x|",theta,"]")),
      main="Negative Binomial Likelihood", add=F,col=2)

# Test Null of theta=1
1-sum(sapply(0:100, function(i) choose(100+i,100)*(1^100)*(1^i)))
# Null can be rejected at the 10% "significance" level

  


# Likelihood
curve(dbinom(7,10,x),xlab=expression(theta),
      ylab=expression(paste("p[x|",theta,"]")),main="Binomial Likelihood")

===============================
  # None of these posteriors are proper probability distributions
  # We need to add the normalizing constant
  
  # Programming the kernel of the Binomial distribution
  binom<-function(theta,n,k){
    choose(n,k)*(theta)^(k)*(1-theta)^(n-k)}

# Notice only the y-axis changes between the kernel and PDF
curve(binom.kern(x,10,3))
curve(binom(x,7,10))

# Using the R functions to plot the posterior with uniform Beta prior
curve(dbeta(x,1,1)*dbinom(3,10,x),0,1, xlab=expression(theta),
      ylab=expression(paste("p[",theta,"|x]")),main="Beta-Binomial Posterior")

#Find max likelihood
# define the log likelihood:

log.post<-function(p,n,k){
  log(choose(n,k)*(p^k)*(1-p)^(n-k))
} 

# then create a set of values with which to evaluate that function
pvals<-seq(0,1,.0001)

# use the optimize function, once log.post and pvals are in
optimize(log.post,interval=c(0,1),n, k,maximum=T)

n<- 10
k<-3
