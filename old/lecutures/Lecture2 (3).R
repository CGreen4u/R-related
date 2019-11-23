##########################################################
# ECON 5525: Econometric Methods                         #
# UMKC, Spring 2018                                      #
# Ellis Scharfenaker                                     #
# Lecture 2                                              #
##########################################################


# Mean and Variance

x<-c(1,2,3,4,5,6)
p<-rep(1/6,6)

m<-sum(x*p)
v<-sum(p*(x-m)^2)
s<-sqrt(v)

(1/5)*sum((x-m)^2) # sample variance
var(x) # R calculates an unbiased estimate 


# Write a function for the kernel of the Binomial distribution
binom.kern <- function(theta,n,k){
  (theta^k)*(1-theta)^(n-k)
}

# In the mortgage example n=100 and k=11
n <- 100
k <- 11

# Plot the likelihood over theta using the "curve" function
?curve
curve(binom.kern(x,n,k),from=0,to=1)

# Notice that when we multiply the kernel by any constant it only changes the scale
const <- choose(n,k)
curve(const*binom.kern(x,n,k),from=0,to=1)

#================================================
# Maximum Likekihood Estimation

# The MLE of theta is
theta.MLE <- k/n

# Which we could also find through numerical optimization
optimize(binom.kern,interval=c(0,1),n=n,k=k,maximum=T)

# Add MLE as a point
curve(binom.kern(x,n,k),from=0,to=1)
points(theta.MLE,binom.kern(theta.MLE,n,k),pch=20,col="red",cex=1)


#================================================
# Frequentist treatment of Bernoulli model


# The probability of seeing 11 defaults in 100 mortgages if the true 
# default rate was 20% is p[X<11|theta=0.2]
pbinom(11,100,.05)

#================================================
# Bayesian treatment of Bernoulli model


# Oberve the Beta distribution for a=1, b=1 is uniform over (0,1)
curve(dbeta(x,1,1),0,1) 

# Programming the kernel of the Beta distribution
beta.kern <- function(theta,alpha,beta){
  (theta)^(alpha-1)*(1-theta)^(beta-1)
}

a <- 1 # prior parameter alpha
b <- 1 # prior parameter beta

# Plot the posterior with a uniform prior by multiplying prior and likelihood
curve(beta.kern(x,a,b)*binom.kern(x,n,k),0,1,
      xlab=expression(theta),ylab=expression(paste("p[",theta,"|x]")),main="Beta-Binomial Posterior")

# Or, from conjugacy, just plot the beta posterior Beta(a+k,b+(n-k))
curve(beta.kern(x,a+k,b+n-k),0,1,xlab=expression(theta),ylab=expression(paste("p[",theta,"|x]")),
      main="Beta-Binomial Posterior")

# We can also write a general function 
library(manipulate)
beta.plp<-function(a,b,k,n){
  curve(dbeta(x,a,b),main="Beta Binomial",xlab=expression(theta),ylab=expression(paste("p[",theta,"|x]")),col="red",ylim=c(0,10))
  curve(dbeta(x,k+1,n-k+1),col="blue",add=T) 
  curve(dbeta(x,a+k,b+n-k),col="green",add=T)
  legend(0,10,c("prior","likelihood","posterior"),lty=c(1,1,1),col=c("red","blue","green"),bty="n")
}

manipulate(beta.plp(a,b,k,n),a=slider(1,5,label="a"),b=slider(1,10,label="b"),k=slider(1,50,label="k"),n=slider(10,100,label="n"))

# Notice though that the posterior is not a proper PDF at this point
integrate(beta.kern,lower=0,upper=1,alpha=a+k,beta=b+n-k)

# Thus we need to normalize the posterior. R has a built in Beta distribution 
# that includes the normalizing constant.
?dbeta
curve(dbeta(x,a+k,b+n-k),0,1,xlab=expression(theta),ylab=expression(paste("p[",theta,"|x]")),
      main="Beta-Binomial Posterior")

# Now the posterior is a proper PDF
integrate(dbeta,lower=0,upper=1,shape1=a+k,shape2=b+n-k)

#================================================
# Grid based approach

# Another easy way of normalizing the posterior is the "grid based" method

# First create a dense grid of points
grid<-seq(0,1,by=.001) # the sequence function can be specified with step width
grid<-seq(0,1,len=1000) # or the number of points in the grid

post.kern<-beta.kern(grid,a+k,b+n-k) # feed the grid into the kernel function
plot(grid,post.kern,cex=.1)

# Normalize the posterior by summing over all values
norm <- sum(post.kern) #sum over all possibilities 
posterior <- post.kern/norm # divide through to normalize the posterior
plot(grid,posterior,cex=.1)

# Now the posterior is a proper PDF
sum(posterior) 

#================================================
# Posterior Inference

# The posterior contains all the information we need to make inferences

# For example we can ask what is probability that theta is greater than 0.2?
1-pbeta(.2,a+k,b+n-k)

# Or what is the probability of theta=0.1?

dbeta(.1,a+k,b+n-k)/100

# Or what values of theta are most likely, i.e. where is 95% of the posterior density?
# If we cut off the upper and lower 2.5% tails of this distribution, we get a 95% probability interval. 

# Using the quantile function gives us the theta in the 2.5 percentile and 97.5 percentile
qbeta(c(0.025,0.975),a+k,b+n-k)

curve(dbeta(x,a+k,b+n-k),0,.4,xlab=expression(theta),ylab=expression(paste("p[",theta,"|x]")),
      main="Beta-Binomial Posterior")
abline(v=qbeta(c(0.025,0.975),a+k,b+n-k)[1])
abline(v=qbeta(c(0.025,0.975),a+k,b+n-k)[2])

# Is this the best (shortest) 95% confidence interval?
library(TeachingDemos)
post.hpd <- hpd(qbeta,shape1=a+k,shape2=b+n-k)

curve(dbeta(x,a+k,b+n-k),0,.4,xlab=expression(theta),ylab=expression(paste("p[",theta,"|x]")),
      main="Beta-Binomial Posterior")
abline(v=post.hpd[1],lty=2,col="red")
abline(v=post.hpd[2],lty=2,col="red")

