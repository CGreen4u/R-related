##########################################################
# ECON 5529: Applied Bayesian Statistics for Economcis   #
# UMKC, Fall 2017                                        #
# Ellis Scharfenaker                                     #
# Lecture 6                                              #
##########################################################

#----------------------------------------------------------
# Probit regression 

# Create data
set.seed(55)
reps<-1000
beta0<-1
beta1<-.5
n<-1000
x<-runif(n,0,10)
y<-rbinom(n,1,pnorm(beta0-beta1*x))

plot(x,y,ylim=c(-2,2))
abline(lm(y~x),col="red")

# Frequentist probit
prob<-glm(y~x,family=binomial(link="probit"))
summary(prob)
beta0<-prob$coefficients[1]
beta1<-prob$coefficients[2]
plot(x,y,xlab="hhincome",ylab="in debt")
curve(pnorm(beta0+beta1*x),lwd=3,add=TRUE,col="blue")

newdata = data.frame(x=0.1)
predict(prob, newdata, type="response")


# Probit in Stan
library(rstan)

probit.model.string<-"
data {
  int<lower=0> N;
  vector[N] x;
  int<lower=0,upper=1> y[N];
}
parameters {
  real beta0;
  real beta1;
} 
model {
  for(n in 1:N)
  y[n] ~ bernoulli(Phi(beta0 + beta1 * x[n]));
}"

dat<-list(N=length(x),x=x,y=y)

probitSTAN<-stan(model_code=probit.model.string,data=dat,iter=1000,chains=4)
print(probitSTAN)

library(ggmcmc)
S <- ggs(probitSTAN)
ggs_density(S)
ggs_caterpillar(S)
ggs_traceplot(S)
ggs_autocorrelation(S)
ggs_geweke(S)


# extract samples (may have to detach ggmcmc package as it also has an extract function)
detach("package:ggmcmc", unload=TRUE)
detach("package:tidyr", unload=TRUE)

samplesAll<-as.data.frame(extract(probitSTAN))
hist(samplesAll$beta0,prob=T)
hist(samplesAll$beta1,prob=T)

plot(x,y,xlab="hhincome",ylab="in debt")
curve(pnorm(mean(samplesAll$beta0)+mean(samplesAll$beta1)*x),lwd=3,add=TRUE,col="red")

# Add HPD region from samples
library(TeachingDemos)

b0hpd<-emp.hpd(samplesAll$beta0)
b1hpd<-emp.hpd(samplesAll$beta1)

curve(pnorm(b0hpd[1]+b1hpd[1]*x),lwd=3,add=TRUE,col="blue")
curve(pnorm(b0hpd[2]+b1hpd[2]*x),lwd=3,add=TRUE,col="blue")


#----------------------------------------------------------
# Logit

# For Logit
logit.model.string<-"
data {
  int<lower=0> N;
  vector[N] x;
  int<lower=0,upper=1> y[N];
}
parameters {
  real beta0;
  real beta1;
} 
model {
  beta0~normal(0,100);
  beta1~normal(0,100);
for(n in 1:N)
  y ~ bernoulli_logit(beta0 + beta1*x);
}"


dat<-list(N=length(data$x),x=data$x,y=data$c)
logit.STAN<-stan(model_code=logit.model.string,data=dat,iter=500,chains=2)
print(logit.STAN,digits_summary=4)

par(mfrow=c(1,1))
plot(x,y,xlab="hhincome ",ylab="in debt ")

detach("package:ggmcmc", unload=TRUE)
detach("package:tidyr", unload=TRUE)

samplesAll<-as.data.frame(extract(olsSTAN))

# Plot logistic curve with HPD estimates

prob<-glm(y~x,family=binomial(link="logit"))
summary(prob)
beta0<-prob$coefficients[1]
beta1<-prob$coefficients[2]
plot(x,y,xlab="hhincome",ylab="in debt")

curve(pnorm(beta0+beta1*x),lwd=3,add=TRUE,col="blue")
curve(exp(beta0+beta1*x)/(1+exp(beta0+beta1*x)),lwd=3,add=TRUE,col="red ")

curve(exp(mean(samplesAll$beta0)+mean(samplesAll$beta1)*x)/(1+exp(mean(samplesAll$beta0)+mean(samplesAll$beta1)*x)),lwd=3,add=TRUE,col="red ")

b0hpd<-emp.hpd(samplesAll$beta0)
b1hpd<-emp.hpd(samplesAll$beta1)

curve(exp(b0hpd[1]+b1hpd[1]*x)/(1+exp(b0hpd[1]+b1hpd[1]*x)),lwd=3,add=TRUE,col="green ")
curve(exp(b0hpd[2]+b1hpd[2]*x)/(1+exp(b0hpd[2]+b1hpd[2]*x)),lwd=3,add=TRUE,col="green ")

# Compare to glm estimate
prob<-glm(y~x,family=binomial(link="logit"))
summary(prob)

