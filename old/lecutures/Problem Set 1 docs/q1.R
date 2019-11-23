#---------------------------------------------------------
# 100 subprime mortgages in 2005, all outstanding 5 years
# 5 of the 100 subprime mortgages defaulted
# estimate probability theta of default of subprime mortgages in
# 1st 5 years
# get idea of how much uncertainty there is given the obsvd. data
# assume subprime is a new mortgage type, so 
# bank assigns a uniform prior over theta
#---------------------------------------------------------

# Beta-Binomial Model: 
#
# The data D: {100 subrime mortgages with 5 observed defaults}
# n = 100 (number of bernoulli trials)
# k = 5 (number of defaults)
# theta = probability of default
# (1-theta) = probability of not defaulting
# Since assuming that this is a new mortgage type, stick with principle of indifference
# and assign equal probabilities to heads and tails
# thus beta distribution is uniform over [0,1] when alpha=beta=1
# The prior p[H]: Beta(a,b) - defined over support (0,1)
# The likelihood p[D|H]: Binomial distribution
#----------------------------------------------------------


#program the kernel of the beta distribution(likelihood) - under assumption uniform
beta.kern<-function(theta,a,b){(theta)^(a-1)*(1-theta)^(b-1)}

# [Programming the kernel of the Binomial distribution]
binom.kern<-function(theta,n,k){(theta^k)(1-theta)^(n-k)}

# need to add normalizing constant 
binom<-function(theta,n,k){choose(n,k)*(theta)^(k)*(1-theta)^(n-k)}

# Using the R functions to plot the posterior with uniform Beta prior
curve(dbeta(x,1,1)*dbinom(100,50,x),0,1, xlab=expression(theta),
      ylab=expression(paste("p[",theta,"|x]")),main="Beta-Binomial Posterior")

#taking uninformative uniform prior over [0,1] by setting a=b=1, can derive 
#posterior distribution for theta which we see is just the the binomial dist!
#binomial distribution be written in beta form as beta[1+k,n-k+1]

# finding the maximum likelihood

# define the log likelihood:
log.post<-function(p,n,k){log(choose(n,k)*(p^k)*(1-p)^(n-k))} 

# use the optimize function
optimize(log.post,interval=c(0,1),n=100,k=5,maximum=T)
# returned values are max likelihood value of theta and expected theta

###### slower alternate way, doing since can't figure out how to 
###### get ml isolated
# then create a set of values with which to evaluate that function
pvals<-seq(0,1,.0001)
out<-log.post(pvals,n=100,k=5)
# bind those values and find the slot in which the log likelihood 
# is maximized
v<-cbind(seq(0,1,.0001),log.post(pvals,n=100,k=5))
ml<-v[which.max(v[,2]),1]
######## do not do this again

# Plot likelihood with modal theta and symmetric confidence intervals
n<-100
k<-5
curve(binom(x,n,k),xlab=expression(theta),ylab=expression(paste("p[x|",theta,"]")),main= "Likelihood")

# Add MLE as a point#### reason I had to do slow way
points(ml,binom(ml,n,k),pch=20,col="red",cex=2)

###doing this way###
# The likelihood for n = 5, N = 100 is 
n<-50
N<-100

db<-curve(dbeta(x,n+1,N-n+1),xlim=c(0,1),xlab=expression(theta), ylab=expression(paste("p[",theta,"|x]")),main="Likelihood")
attributes(db)
abline(v=db$x[db$y==max(db$y)],col="red")
abline(v=(n+1)/(n+1+N-n+1),col="blue")
legend(.5,18,c("Maximum Likelihood","expected likelihood"),lty=c(1,1),col=c("red","blue"),bty="n")
}


# Use quantile function to get confidence interval
ci <- qbeta(c(0.025,0.975),n+1,N-n+1)

# Add vertical CI lines
abline(v=ci[1],lty=2)
abline(v=ci[2],lty=2)

#using quantile function qbeta cut off left and right tail
# already did so 

#####2
# Prior
curve(dbeta(x,100,100),xlab=expression(theta),
      ylab=expression(paste("p[",theta,"]")),main="Beta Prior")










