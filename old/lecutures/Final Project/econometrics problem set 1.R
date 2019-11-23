#You flip a coin n times and see k heads. 
#Let the parameter θ be the unknown probability of flipping a heads. 
#Use this information to answer the following questions:

#(a)Using an ignorance prior for θ plot the posterior p[θ|{n = 99,k = 32}] on thoroughly labeled graph. 
#Use the dbeta function and the Beta-Binomial model.
n <- 99
k <- 32

# Oberve the Beta distribution for a=1, b=1 is uniform over (0,1)
curve(dbeta(x,1,1),0,1) 

# Programming the kernel of the Beta distribution
beta.kern <- function(theta,alpha,beta){
  (theta)^(alpha-1)*(1-theta)^(beta-1)
}
#programming the kernel of the binomial distribution 
binom.kern <- function(theta,n,k){
  (theta^k)*(1-theta)^(n-k)
}

a <- 1 # prior parameter alpha
b <- 1 # prior parameter beta

curve(dbeta(x,a+k,b+n-k),0,1,xlab=expression(theta),ylab=expression(paste("p[",theta,"|x]")),
      main="Beta-Binomial Posterior")

###(b) For n = 99 and k = 32 calculate the maximum posterior value of θ, 
###the highest posterior density (HPD) region, P r[θ ≤ 0.3], and P r[θ > 0.8].
optimize(dbeta,interval=c(0,1),a+k, b+n-k, maximum=T)


##hpd region =(0.237,0.418)
# Is this the best (shortest) 95% confidence interval?
library(TeachingDemos)

post.hpd <- hpd(qbeta,shape1=a+k,shape2=b+n-k)


# For example we can ask what is probability that theta is greater than 0.8?  P r[θ > 0.8].

1-pbeta(.8,a+k,b+n-k)

##theta less than .3
pbeta(.3,a+k,b+n-k)

## theta equal to .3 - don't need
dbeta(.3,a+k,b+n-k)/100

#(c)Use the quantile function to calculate the symmetric 95% probability interval. 
#Plot the posterior distribution with both the symmetric interval 
#and HPD region on a thoroughly labeled graph.


# Or what values of theta are most likely, i.e. where is 95% of the posterior density?
# If we cut off the upper and lower 2.5% tails of this distribution, we get a 95% probability interval. 
# Using the quantile function gives us the theta in the 2.5 percentile and 97.5 percentile
qbeta(c(0.025,0.975),a+k,b+n-k)
symmetric<-qbeta(c(0.025,0.975),a+k,b+n-k)
##initial curve
curve(dbeta(x,a+k,b+n-k),0,1,xlab=expression(theta),ylab=expression(paste("p[",theta,"|x]")),
      main="Beta-Binomial Posterior")
abline(v=qbeta(c(0.025,0.975),a+k,b+n-k)[1],col="green")
abline(v=qbeta(c(0.025,0.975),a+k,b+n-k)[2],col="green")


library(TeachingDemos)
post.hpd <- hpd(qbeta,shape1=a+k,shape2=b+n-k)
abline(v=post.hpd[1],lty=2,col="red")
abline(v=post.hpd[2],lty=2,col="red")

legend(.5,9,c("symmetric","HPD"),lty=c(1,2),col=c("green","red"),bty="n")

###(d)Using the informative Beta distribution prior with α = 1, β = 10 
####plot the posterior distribution for n = 10 and k = 3.
a <- 1
b <- 10
n <- 10
k <- 3

curve(dbeta(x,a+k,b+n-k),0,1,xlab=expression(theta),ylab=expression(paste("p[",theta,"|x]")),
      main="Beta-Binomial Posterior")

curve(dbeta(x,1,10),0,1) 
curve(dbeta(x,a,b),0,1) ##same
curve(dbeta(x,a+k,b+n-k),0,1,xlab=expression(theta),ylab=expression(paste("p[",theta,"|x]")),
      main="Beta-Binomial Posterior")

#e)Calculate the maximum posterior value for θ with the informative prior 
#and find the probability P r[θ > 0.4].

1-pbeta(0.4,a+k,b+n-k)
optimize(dbeta,interval=c(0,1),a+k, b+n-k, maximum=T)

