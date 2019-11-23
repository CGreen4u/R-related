
library(manipulate)

# None of these posteriors are proper probability distributions
# We need to add the normalizing constant

# Programming the kernel of the Binomial distribution
binom<-function(theta,n,k){
  choose(n,k)*(theta)^(k)*(1-theta)^(n-k)}

# Notice only the y-axis changes between the kernel and PDF
curve(binom.kern(x,11,4))
curve(binom(x,11,4))

# Using the R functions to plot the posterior with uniform Beta prior
curve(dbeta(x,1,5)*dbinom(4,11,x),0,1, xlab=expression(theta),
      ylab=expression(paste("p[",theta,"|x]")),main="Beta-Binomial Posterior")


#----------------------------------------------------------
# Informative Priors
#----------------------------------------------------------

# Prior
curve(dbeta(x,5,100),xlab=expression(theta),
      ylab=expression(paste("p[",theta,"]")),main="Beta Prior")

# Likelihood
curve(dbinom(5,100,x),xlab=expression(theta),
      ylab=expression(paste("p[x|",theta,"]")),main="Binomial Likelihood")

# Posterior
curve(dbeta(x,2,2)*dbinom(5,100,x),0,1, xlab=expression(theta),ylab=expression(paste("p[",theta,"|x]")),main="Beta-Binomial Posterior",col="red")
curve(dbeta(x,1,1)*dbinom(5,100,x),0,1, lty=2, xlab=expression(theta),ylab=expression(paste("p[",theta,"|x]")),main="Beta-Binomial Posterior",add=T)
legend(.45,.35,c("Informative Posterior","Uninformative Posterior"),lty=c(1,2),col=c(2,1),bty="n")


--------------------------
  #proper graph
  
  # one way of finding the maximum likelihood is to use first 
  # define the log likelihood:
  
  log.post<-function(p,n,k){
    log(choose(n,k)*(p^k)*(1-p)^(n-k))
  } 

# then create a set of values with which to evaluate that function
pvals<-seq(0,1,.0001)
out<-log.post(pvals,n=100,k=5)
# bind those values and find the slot in which the log likelihood 
# is maximized
v<-cbind(seq(0,1,.0001),log.post(pvals,n=100,k=5))
ml<-v[which.max(v[,2]),1]

# Alternatively use the optimize function
optimize(log.post,interval=c(0,1),n=100,k=5,maximum=T)

# Plot posterior with modal theta and symmetric confidence intervals
n<-100
k<-5
curve(binom(x,n,k),xlab=expression(theta),ylab=expression(paste("p[x|",theta,"]")))

# Add MLE as a point
points(ml,binom(ml,n,k),pch=20,col="red",cex=2)

# Use quantile function to get confidence interval
ci <- qbeta(c(0.025,0.975),k+1,(n-k)+1)

# Add vertical CI lines
abline(v=ci[1],lty=2)
abline(v=ci[2],lty=2)


# The likelihood for n = 5, N = 100 is 
n<-5
N<-100

db<-curve(dbeta(x,n+1,N-n+1),xlim=c(0,1),xlab="theta",ylab="p[x|theta]",main="Likelihood")
attributes(db)
abline(v=db$x[db$y==max(db$y)],col="red")
abline(v=(n+1)/(n+1+N-n+1),col="blue")

# If we cut off the upper and lower 2.5% tails of this distribution,
# we get a 95% probability interval of (.0221, .1118). 

ci<-qbeta(c(0.025,0.975),n+1,N-n+1)
abline(v=ci[1],lty=3)
abline(v=ci[2],lty=3)

# Shortest interval
library(TeachingDemos)

di<-hpd(qbeta, shape1=n+1, shape2=N-n+1)

abline(v=di[1])
abline(v=di[2])

---------------------------------
  The likelihood for theta is the Negative Binomial distribution
neg.binom<-function(theta,n,k){
  choose(n-1,k-1)*(theta)^(k)*(1-theta)^(n-k)}

curve(neg.binom(x,50,100),xlab=expression(theta),ylab=expression(paste("p[x|",theta,"]")),
      main="Negative Binomial Likelihood", add=F,col=2)

# Test Null of theta=1
1-sum(sapply(0:50, function(i) choose(0+i,50)*(0.5^7)*(.5^i)))
# Null can be accepted at the 10% "significance" level  
  
  
  