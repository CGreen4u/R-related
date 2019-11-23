##########################################################
# ECON 5529: Applied Bayesian Statistics for Economcis   #
# UMKC, Fall 2017                                        #
# Ellis Scharfenaker                                     #
# Lecture 4                                              #
##########################################################

#----------------------------------------------------------
library("geoR")

x<-rnorm(1000,1.2,1)
N<-100000 # number of samples
n<-length(x)
s<-var(x)
xbar<-mean(x)

# Create 
mu<-rep(0,N)
sigma2<-rep(0,N)
mu[1]<-0             #initial value
sigma2[1]<-2    #initial value

# Set values of hyperparameters for diffuse prior
m <- 0
tau <- 1000

# Posterior simulation strategy

rinvchisq <- function (n, df, scale = 1/df) {
  if ((length(scale) != 1) & (length(scale) != n)) 
    stop("scale should have the same length as n")
  if (df <= 0) 
    stop("df must be greater than zero")
  if (any(scale <= 0)) 
    stop("scale must be greater than zero")
  (df * scale)/rchisq(n, df = df)
}

for (i in 2:N){
  sigma2[i]=rinvchisq(1,n-1,s^2) 
  post.mean <- (m/tau + (n*mean(x)/sigma2[i]))/(1/tau + n/sigma2[i])
  post.var <- 1/(1/tau + n/sigma2[i]) 
  mu[i]=rnorm(1,post.mean,sqrt(post.var))
}

hist(mu,prob=T,breaks=100,yaxt="n",ylab="",xlab=expression(mu),main="Marginal Posterior of mu")
abline(v=xbar,col="red")
hist(sqrt(sigma2),prob=T,breaks=100,yaxt="n",ylab="",xlab=expression(sigma^2),main="marginal posterior of sigma")
abline(v=s,col="red",lwd=3)

library(TeachingDemos)
# calculate the empirical HPD region for mu
hpd.mu=emp.hpd(mu,conf=.9)
abline(v=hpd.mu[1])
abline(v=hpd.mu[2])

# calculate the empirical HPD region for sigma^2

hpd.sig=emp.hpd(sqrt(sigma2))
abline(v=hpd.sig[1])
abline(v=hpd.sig[2])


#-----------------------------------------------
# The Multinomial-Dirichlet Model

#install.packages("LearnBayes")
library(LearnBayes)
a<-c(1,1,1); # Hyperparameters in Dirichlet prior
x<-c(659,588,129); # Data 
theta<-rdirichlet(10000,x+a) # Sample from posterior
head(theta)
theta.diff<-theta[,1]-theta[,2] # Calculate margin
hist(theta.diff,col="grey",prob=T,xlab="margin of Clinton over Trump",main="")

# Load state level poll data
setwd("~/Dropbox/UMKC/ECON5529/Lectures/Lecture4") # set working directory
election.2016<-read.csv("statepolls.csv") # read in data
attach(election.2016) # attach data


# Sample 
library(MCMCpack)
prob.clinton=function(j){
    p=rdirichlet(5000,
    500*c(Trump[j]+7,Clinton[j]+1,100-Trump[j]-Clinton[j])/100+1)
    mean(p[,2]>p[,1])
}

Clinton.win.probs<-sapply(1:51,prob.clinton)
data.frame(State,Clinton.win.probs)

win<-rbinom(51,1,Clinton.win.probs)

data.frame(State,pp=Clinton.win.probs,win,ev=win*EV)

sim.election=function(){
     winner=rbinom(51,1,Clinton.win.probs)
     sum(EV*winner)
}

sim.EV<-replicate(1000,sim.election())

hist(sim.EV,prob=T,col="blue",ylim=c(0,.03)) 
hist(sim.EV,prob=T,min(sim.EV):max(sim.EV),col="green",add=T)
hpd.mu=emp.hpd(sim.EV,conf=.95)
abline(v=hpd.mu[1],lwd=2,col="red")
abline(v=hpd.mu[2],lwd=2,col="red")
mean(sim.EV)
min(sim.EV)
# compate to 2008

library("LearnBayes")
data(election.2008)
attach(election.2008)

prob.obama=function(j){
  p=rdirichlet(5000,
               500*c(M.pct[j]+1,O.pct[j]+1,100-M.pct[j]-O.pct[j])/100+1)
  mean(p[,2]>p[,1])
}

Obama.win.probs=sapply(1:51,prob.obama)

sim.election=function(){
  winner=rbinom(51,1,Obama.win.probs)
  sum(EV*winner)
}
sim.EV=replicate(1000,sim.election())

hist(sim.EV,min(sim.EV):max(sim.EV),col="blue") 
abline(v=365,lwd=3) # Obama received 365 votes
text(375,30,"Actual \n Obama \n total")

hpd.mu=emp.hpd(sim.EV,conf=.95)
abline(v=hpd.mu[1])
abline(v=hpd.mu[2])

