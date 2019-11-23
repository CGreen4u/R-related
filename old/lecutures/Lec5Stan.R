##########################################################
# ECON 5529: Applied Bayesian Statistics for Economcis   #
# UMKC, Fall 2016                                        #
# Ellis Scharfenaker                                     #
# Lecture 4                                              #
##########################################################
# Example using STAN
library(rstan)
options(mc.cores = parallel::detectCores())

# Specify Model String (// are comments)
modelString = "
data {
  int<lower=0> N ;
  int x[N] ; // x is a length-N vector of integers
}
parameters {
  real<lower=0,upper=1> theta ;
} 
model {
  theta  ~ beta(1,1) ;
  x  ~ bernoulli(theta) ;
}
" 

x<-rbinom(50,1,.1)
N<-length(x)
z<-sum(x)
dataList = list(x = x , N = N)
inits<-function(){list(theta=.5)}

# Translate model 
stanDso = stan_model(model_code=modelString)
# Sample from model
stanFit = sampling(object=stanDso , data=dataList ,
          chains=3 , iter=1000 , warmup=200 , thin=1,init=inits)

print(stanFit)
# or in one line of code
set.seed(444)

# call .stan file from working directory
stanFit = stan(file="binom.stan" , data=dataList ,
               chains=3 , iter=1000 , warmup=200 , thin=1)

# or write model_code in R
stanFit = stan(model_code=modelString , data=dataList ,
                chains=4 , iter=10000 , warmup=200 , thin=4)

# print stan output
print(stanFit)
print(stanFit, digits_summary=4)

# plot stan output
traceplot(stanFit)
plot(stanFit)

# extract stan output
samplesAll <- as.data.frame(extract(stanFit))
head(samplesAll)
hist(samplesAll$theta)

# return a list of arrays
la<-extract(stanFit,permuted=TRUE)
theta<-la$theta
str(la)

##  use S3 functions as.array (or as.matrix) on stanfit objects
m<-as.matrix(stanFit)
head(m)
hist(m[,1])

# use ggs function to extract stan output
library(coda)
library(ggmcmc)
S <- ggs(stanFit)
ggs_density(S)
ggs_histogram(S)
ggs_caterpillar(S)
ci(S)
?ci

ggs_traceplot(S)
ggs_running(S)
ggs_compare_partial(S)
ggs_autocorrelation(S)
ggs_geweke(S)
