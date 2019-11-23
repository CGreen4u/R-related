#2
#DownloadtheExtendedPennWorldTablesfromblackboard

setwd("/Users/chloesegale/Desktop/econ 5529 - bayesian statistics/Problem set 2")
epwt<-read.csv("6-EPWTv4.csv",header=TRUE)

head(epwt)
unique(epwt$Year)
unique(epwt$Quality)
attach(epwt)

##(a)##Subset the data to include 
###(1) only observations between 1983 and 2007 
###(2) only observations of quality “A" or “B", 
###(3) only the variables “Country", “Id", “Year", "x", and "c", and 
###(4) drop all missing values using the compete.cases function.

##use lecture 6 as guide

# subset the data
str(epwt)
D<-subset(epwt, (Year >= 1983 & Year <= 2007) & (Quality=="A" | Quality=="B"), select=c(1,2,3,16,24))

colnames(D)<-c("Country","Id","Year","x","c")

# Missing values or NA's can be removed by useing complete.cases
EPWT.CC.SUB<-D[complete.cases(D),]

#(b) Plot a scatter plot of consumption per capital(c) against income per capita(x).
library(ggplot2)
ggplot(EPWT.CC.SUB,aes(c,x,color=Country))+geom_point(size=.5)+theme_bw()
##OR..
c<-EPWT.CC.SUB$c
x<-EPWT.CC.SUB$x
plot(c,x,pch=20,xlab="consumption per capita (c)",ylab="income per capita (x)")
#or
plot(EPWT.CC.SUB$c,EPWT.CC.SUB$x,pch=20,xlab="consumption per capita (c)",ylab="income per capita (x)")

##c) Write a model in Stan or JAGS to estimate the consumption function
###                     c =c0 +c1x

# using Poisson Regression lecture examples

library(rstan)

consumptionfunction.stan="
data {
int<lower=0> N;
vector[N] x;
vector[N] y;
}
parameters {
real beta0;
real beta1;
real<lower=0> sigma;
} 
model {
    y~normal(beta0+beta1*x, sigma);
}"
###HELLPPPPP
dat<-list(N=length(EPWT.CC.SUB$x),y=EPWT.CC.SUB$c,x=EPWT.CC.SUB$x)
stan.fit<-stan(model_code=consumptionfunction.stan,data=dat,iter=10000,chains=3,thin=2)
##d) Diagnose model with traceplots of the MCMC runs,
##simulated posterior densities of the parameters
###autocorrelation, partial correlation, Geweke statistic, and Rhat.
print(stan.fit)
traceplot(stan.fit)
plot(stan.fit)
summary(stan.fit)
#convert stan object into MCMC obejct
library(ggmcmc)
S<-ggs(stan.fit)


##  A traceplot is a plot of the iteration number against the 
##  value of the draw of the parameter at each iteration.
##    ■ In general we look for a plot of the Markov chains that shows a tight random 
##    scatter around a mean value that does not wander. 
##    The running mean plot should be relatively stable.
ggs_traceplot(S)
ggs_running(S)

#look at posterior densities
ggs_density(S)
ggs_histogram(S)
ggs_caterpillar(S)

#Another way to assess convergence is to assess the 
##autocorrelations between the draws of our Markov chain
ggs_autocorrelation(S)

#partial correlation
ggs_compare_partial(S)

## The Geweke diagnostic takes two nonoverlapping parts 
## (usually the first 0.1 and last 0.5 proportions) of the Markov chain 
## and compares the means of both parts, using a difference of means test to 
## see if the two parts of the chain are from the same distribution (null hypothesis)
ggs_geweke(S)

ggs_Rhat(S)

###e)Use the mean of the posterior for the marginal propensity to consume (c1) 
###to calculate the multiplier 1/1c1

###
print(stan.fit)
beta1=.76
1/(1-beta1)
