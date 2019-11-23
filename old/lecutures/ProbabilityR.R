# probability in R

# all distribution functions in R contain the following

# pdist - cdf
# qdist - quantile or inverse cdf
# ddist - pdf
# rdist - random generation

# what is p(X < 20) when X is normal with mean 50 and standard deviation 20
pnorm(20,50,20)
curve(pnorm(x,50,20),0,100,xlab="x",ylab="cdf",main="my cdf")
points(20,pnorm(20,50,20),col="red", pch=15,cex=1)

# What is the probability that a randomly chosen x > 19?

1-pnorm(20,50,20)
curve(1-pnorm(x,50,20),0,100)
points(20,1-pnorm(20,50,20))

# qdist is the R function that calculates the inverse cdf where
# p = F(x)
# x = F^-1(p)
# So given a number p between zero and one, qnorm looks up 
# the p-th quantile of the normal distribution.

# Suppose X is normally distributed with mean 100 and standard 
# deviation 15. What is the 95th percentile of the distribution?
qnorm(.95,100,15)
curve(qnorm(x,100,15))
points(.95,qnorm(.95,100,15))


# the pdf is caculated using ddist though 
curve(dnorm(x,50,20),0,100)
points(20,dnorm(20,50,20))

# however to calculate probabilities we need to integrate.

# Examine the likelihood of a binomial model

N<-10
n<-4

# The likelihood for n = 4, N = 10 is 
db<-curve(dbeta(x,n+1,N-n+1),xlim=c(0,1),xlab="theta",ylab="p[x|theta]",main="Likelihood")
attributes(db)
abline(v=db$x[db$y==max(db$y)],col="red")
abline(v=(n+1)/(n+1+N-n+1),col="blue")

# If we cut off the upper and lower 2.5% tails of this distribution,
# we get a 95% probability interval of (.0221, .1118). 

ci<-qbeta(c(0.025,0.975),n+1,N-n+1)
abline(v=ci[1])
abline(v=ci[2])

# investigate some data

y<-rexp(1000,.4)
hist(y,prob=T,breaks=100)
plot(density(y))
summary(y)
quantile(y,c(seq(0,1,.05)))

##################################################
# Some useful functions

# apply, sapply, lapply 

m <- matrix(cbind(rnorm(30, 0), 
                       rnorm(30, 2), 
                       rnorm(30, 5)), 
            nrow=30, ncol=3)
md<-as.data.frame(m)
colnames(md)<-c("x1","x2","x3")
# take the mean of each column
mx<-c()
for(i in 1:3){
  mx[i]<-mean(m[,i])
}
mx

colMeans(md)

# apply
# We tell apply to traverse row wise or column wise by the second argument.
apply(m, 2, mean)
apply(m,1,function(x) x+2)
apply(m, 2, is.vector)


# sapply
# sappls works on a list or vector of data
sapply(1:3, function(x) x^2)

# lapply is very similar, however it will return a list rather than a vector
lapply(1:3, function(x) x^2)
# which you can unlist
unlist(lapply(1:3, function(x) x^2))

# can use sapply to apply a function to only some columns or rows
sapply(21:25, function(x) mean(m[x,]))



# tapply is useful when we need to break up a vector into groups 
# defined by some classifying factor
ex <-
  data.frame(patient = 1:100,
             age = rnorm(100, mean = 60, sd = 12),
             treatment = gl(2, 50,
                            labels = c("Treatment", "Control")))

summary(ex)
tapply(ex$age, ex$treatment, mean)

ex[ex$treatment=="Control",2]


