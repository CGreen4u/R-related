##########################################################
# ECON 5529: Applied Bayesian Statistics for Economcis   #
# UMKC, Fall 2017                                        #
# Ellis Scharfenaker                                     #
# Lecture 5                                              #
##########################################################

# Simulating distributions
par(mar=c(2,2,2,1),mfrow=c(2,2)) # set plot parameters

curve(dgamma(x,2,9))
hist(rgamma(100,2,9),breaks=20,prob=T,add=T)

curve(dgamma(x,2,9))
hist(rgamma(500,2,9),50,prob=T,add=T)

curve(dgamma(x,2,9))
hist(rgamma(1000,2,9),100,prob=T,add=T)

curve(dgamma(x,2,9))
hist(rgamma(10000,2,9),100,prob=T,add=T)

par(mar=c(5,4,4,1),mfrow=c(1,1))


#---------------------------------------------------------
# Monte Carlo Simulation
n=10^5
norm.sims<-rnorm(n,0,1) # generate random normal variable
hist(norm.sims,breaks=100) # plot histogram
var.est<-sum(norm.sims[norm.sims>-2 & norm.sims<1]^2)/10^5 #estimate the variance between -2 and 1

norm.var<-function(x) {x^2*dnorm(x,0,1)}
integrate(norm.var,lower=-2,upper=1)


norm.var.sim<-function(n){
  norm.sims<-rnorm(n,0,1) 
  sum(norm.sims[norm.sims>-2 & norm.sims<1]^2)/n
}

plot(sapply(1:500,norm.var.sim),type="l")

plot(sapply(2:500,function(i) mean(sapply(1:i,norm.var.sim))),type="l")
abline(h=var.est,col=2)

###############################################################
# Markov Chains
# 3x3 Example 
###############################################################
P<-matrix(c(.2,.1,.5,.4,.3,.1,.4,.6,.4),nrow=3)
rowSums(P) # right stochastic matrix
P<-t(P) # left stochastic matrix
colSums(P) # Check stochastic condition satisfied 

# See how Markov chain evolves for each iteration of the difference equation
pi0<-c(.5,.4,.1)
pi1<-P%*%pi0 # %*% is notation for dot product
pi2<-P%*%pi1
pi3<-P%*%pi2

# Recursive iteration of the Chapman-Kolmogorov function 500 times 
init <- pi0 
for(i in 1:500) init<-P%*%init 
init

# Plot evolution of each state probability
init <- pi0 
mc.plot<-function(j,state){for(i in 1:j){init<-P%*%init}
return(init[state])}
mc1<-sapply(1:10,mc.plot,state=1)
mc2<-sapply(1:10,mc.plot,state=2)
mc3<-sapply(1:10,mc.plot,state=3)

plot(mc1,type="p",pch=1,ylim=c(0,1),col=1,xlab="iteration",ylab="Markov Probs.")
points(mc2,col=2,pch=2)
points(mc3,col=3,pch=3)

# Find ergodic state using Perron-Frobenius theorem
eg<-eigen(P) # need to transpose if using right stochastic matrix
ergod<-Re(eg$vectors[,1]/sum(eg$vectors[,1]))



# Write simulation function 
MC.sim <- function(n,P) {
  sim<-c()
  # n - number of steps, P-left stochastic matrix, x1=inititial value for MC
  m <- ncol(P)    
  sim[1] <- sample(1:m,1) # random start 
  for(i in 2:n){
    newstate <- sample(1:m,1,prob=P[,sim[i-1]])
    sim[i] <- newstate
  }
  sim
}

# simulate 3 state MC
set.seed(123)
sim3<-MC.sim(10,P) # increase number of iterations from 10 to 10^4
plot(sim3,type="p")
lines(sim3)
hist(sim3)

plot(ergod,type="h",lwd=10,col="lightblue",ylim=c(0,.45))
sim.probs<-sapply(1:3,function(i) length(sim3[sim3==i])/length(sim3))
lines(sim.probs,type="h",lwd=7,col="pink")

# Create 5 state left stochastic transition matrix
P=matrix(c(rep(.2,5),rep(.1,4),.6,.5,rep(.125,4),.5, .5,0,0,0,.25,.5,0,.25,0),
         nrow=5,byrow=TRUE)
P<-t(P)
colSums(P) # Check left stochastic condition satisfied 

# Recursive iteration of this function 50 times 
init <- c(1,0,0,0,0) 
for(i in 1:50) init <- P%*%init 
init

# Check ergodic state from Perron-Frobenius theorem
eg<-eigen(P)
ergod<-Re(eg$vectors[,1]/sum(eg$vectors[,1])) # normaliez real part of eigenvector corresponding to largest real eigenvalue

set.seed(100)
sim5<-MC.sim(100,P)
plot(sim5,type="p",pch=20) # simulate 100 steps of MC for 5 states
lines(sim5)

plot(ergod,type="h",lwd=10,col="lightblue",ylim=c(0,.45)) # ergodic state
sim.probs<-sapply(1:5,function(i) length(sim5[sim5==i])/length(sim5))
lines(sim.probs,type="h",lwd=7,col="pink")


###############################################################
# Simple Gibbs Sampler: Missing Mortgage
###############################################################
set.seed(123)
Nsim<-10000
x<-c()
theta<-c()
theta[1]<-.01
x[1]<-rbinom(1,1,theta[1])
for(i in 2:Nsim){
  theta[i]<-rbeta(1,14+x[i-1]+1,6-x[i-1]+1) 
  x[i]<-rbinom(1,1,theta[i])
}

# Visualize the Trace Plot
burn<-500
par(mfrow=c(1,1),mar=c(4,4,1,1))
plot(theta[1:burn],type="l",ylab=expression(theta),xlab="iteration",main="MCMC Traceplot")
mean.theta<-mean(theta[1:burn])
abline(h=mean.theta,col=2,lwd=2)

mean.theta.burn<-mean(theta[-c(1:burn)])
plot(theta[-c(1:burn)],type="l",ylab=expression(theta),xlab="iteration",main="MCMC Traceplot")
abline(h=mean.theta.burn,col=2,lwd=2)

# Full MCMC posterior distribution
hist(theta,prob=T,breaks=50)
library(TeachingDemos)
abline(v=emp.hpd(theta)[1],lwd=2)
abline(v=emp.hpd(theta)[2],lwd=2)
mean(theta)
var(theta)

# Convergence of Gibbs Sampler

plot(theta[1],theta[2],type="n",xlim=range(theta),ylim=range(theta),
     xlab=expression(theta[t]),ylab=expression(theta[t+1]))
for (i in 1:100){
  segments(theta[i],theta[i],theta[i+1],theta[i])
  segments(theta[i+1],theta[i],theta[i+1],theta[i+1])
}

# Write as a function of the iterations
gibbs.walk.plot<-function(ll){
  plot(theta[1],theta[2],type="n",xlim=range(theta),ylim=range(theta),
       xlab=expression(theta[t]),ylab=expression(theta[t+1]))
for (i in 1:ll){
  segments(theta[i],theta[i],theta[i+1],theta[i])
  segments(theta[i+1],theta[i],theta[i+1],theta[i+1])
}
}

library(manipulate)
manipulate(gibbs.walk.plot(ll),
           ll=slider(1,500,label="length"))


###############################################################
# Accept-Reject Algorithm

optimize(f=function(x){dbeta(x,2.7,6.3)},interval=c(0,1),maximum=T)$objective
curve(dbeta(x,2.7,6.3))
Nsim<-2500
a<-2.7;b<-6.3
M<-2.67
u<-runif(Nsim,max=M) #uniform over (0,M)
y<-runif(Nsim) #generation from g
x<-y[u<dbeta(y,a,b)] #accepted subsample
hist(x,prob=T)
plot(seq(1:2500)/2500,u,ylab="u.g[y]",xlab="y")
curve(dbeta(x,a,b),add=T,lwd=4,col="red")
cord.x<-c(0,seq(0,1,0.01),1)
cord.y<-c(0,dbeta(seq(0,1,0.01),a,b),0)
polygon(cord.x,cord.y,col=rgb(1,0,0,0.5))

hist(x,breaks=20,prob=T,add=T,col=4)

