##########################################################
# ECON 5529: Applied Bayesian Statistics for Economcis   #
# UMKC, Fall 2016                                        #
# Ellis Scharfenaker                                     #
# Lecture 4                                              #
##########################################################

library(rjags)

# Jags: Binomial Model

x <- rbinom(100,1,.3)
N <- length(x)  
data <- list( x=x,  N = N)

theta0 <- sum(x)/length(x)
initsList <- list(theta=theta0)

setwd("~/Dropbox/UMKC/ECON5529/Lectures/Lecture5MCMC")

jagsModel = jags.model(file="Binomial.jags" , data=data , inits=initsList ,
                        n.chains=3)

codaSamples = coda.samples(jagsModel, variable.names = c ("theta"), n.iter = 100)

summary(codaSamples)
plot(codaSamples)

library(ggmcmc)

mcmc.out <- ggs(codaSamples)
ggs_histogram(mcmc.out)
ggs_density(mcmc.out)
ggs_traceplot(mcmc.out)


# Jags: Regression Model

x<-rnorm(100,0,1)
y<-2*x+rnorm(100,0,2)
plot(log(data$x),log(data$c))
plot(log(D),log(G)
data<-list(x=x,y=y,N=length(y))

jagsModel<-jags.model(file="regression.jags",data=data, n.chains=3)
update(jagsModel,n.iter=50)
codaSamples<-coda.samples(jagsModel,variable.names=c("a","b"),thin=1,n.iter=1000)
plot(codaSamples)
summary(codaSamples)

mcmc.out <- ggs(codaSamples)
ggs_histogram(mcmc.out)
ggs_density(mcmc.out)
ggs_traceplot(mcmc.out)

# Compare to LM function
fit<-lm(y~x)
summary(fit)

# Faster Gibbs Sampling using "runjags"
# The function run.jags is a user friendly pagackage for running JAGS in R.
# It also allows for parallel cores to be used for more efficient sampling.

library("parallel")
detectCores()

library(runjags)
runJagsOut <- run.jags( method="parallel" ,
                        model="regression.jags" ,
                        monitor=c("a","b") ,
                        data=data,
                        n.chains=4,
                        burnin=500,
                        sample=1000,
                        thin=1)

codaSamples <- as.mcmc.list( runJagsOut )
summary(codaSamples)
plot(codaSamples)
