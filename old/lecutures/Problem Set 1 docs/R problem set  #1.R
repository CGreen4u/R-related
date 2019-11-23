#question 1
#parta a
library("rmeta")
library("manipulate")

# Programming the kernel of the Binomial distribution
binom.kern <- function(theta,n,k){(theta^k)*(1-theta)^(n-k)}

# Programming the kernel of the Beta distribution
beta.kern <- function(theta,a,b){(theta)^(a-1)*(1-theta)^(b-1)}

#programming the kernel of the binomial distribution (with normalizing constant)
binom<-function(theta,n,k){choose(n,k)*(theta)^(k)*(1-theta)^(n-k)}

#prior
curve(dbeta(x,5,100),xlab=expression(theta),ylab=expression(paste("p[",theta,"]")),main="Beta Prior")

#likelihood
curve(dbinom(5,100,x),xlab=expression(theta),
      ylab=expression(paste("p[x|",theta,"]")),main="Binomial Likelihood")

#max likelihood:
# define the log likelihood:

log.post<-function(p,n,k){log(choose(n,k)*(p^k)*(1-p)^(n-k))} 
# then create a set of values with which to evaluate that function
pvals<-seq(0,1,.0001)

# use the optimize function
optimize(log.post,interval=c(0,1),n=100,k=5,maximum=T)

# Use quantile function to get confidence interval
ci <- qbeta(c(0.025,0.975),5+1,(100-5)+1)

# Add vertical CI lines
abline(v=ci[1],lty=2)
abline(v=ci[2],lty=2)

#the likelihood for n=5, N=100 is
n<-5
N<-100

db<-curve(dbeta(x,n+1,N-n+1),xlim=c(0,1),xlab="theta",ylab="p[x|theta]",main="Likelihood")
attributes(db)
abline(v=db$x[db$y==max(db$y)],col="red")
abline(v=(n+1)/(n+1+N-n+1),col="blue")

#cut off upper and lower 2.5% tails of distribution,
#get a 95% prob int of (.0221,.1118) (db list of 2)

ci<-qbeta(c(0.025,0.975),n+1,N-n+1)
abline(v=ci[1],lty=3)
abline(v=ci[2],lty=3)

#d. shortest integral
library(TeachingDemos)
di<-hpd(qbeta,shape1=n+1,shape2=N-n+1)

abline(v=di[1])
abline(v=di[2])

