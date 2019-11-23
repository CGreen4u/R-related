##########################################################
# ECON 5529: Applied Bayesian Statistics for Economcis   #
# UMKC, Fall 2017                                        #
# Ellis Scharfenaker                                     #
# Lecture 6                                              #
##########################################################
# KV regression using STAN

setwd("~/Dropbox/UMKC/ECON5529/Lectures/Lecture6Regression")
D<-read.csv("KV.csv")
attach(D)
plot(X,L,log="xy",pch=20,xlab="National Income (2001Q1 Dollars)",ylab="Non-Farm Employment")
plot(log(X),log(L),pch=20,xlab="National Income (2001Q1 Dollars)",ylab="Non-Farm Employment")
fit<-lm(log(L)~log(X))
abline(fit,col="red",lwd=2)
summary(fit)

plot(log(D[213:262,1]),log(D[213:262,2]),pch=20,xlim=c(9,9.4),xlab="National Income (2001Q1 Dollars)",ylab="Non-Farm Employment")
abline(lm(log(D[1:213,2])~log(D[1:213,1])),col="red")

plot(fit$residuals,type="l")
abline(h=0)

library(rstan)

DD<-list(N=length(X),y=log(L),x=log(X))


stan.code="
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
  y ~ normal(beta0 + beta1*x, sigma);
}"


STANout <-stan(model_code=stan.code,data=DD,iter=10000,chains=3)

print(STANout)
# detach("package:ggmcmc", unload=TRUE)
traceplot(STANout,"beta1",inc_warmup = T)
library(ggmcmc)
S<-ggs(STANout)
ggs_traceplot(S)
ggs_density(S)
ggs_compare_partial(S)
ggs_running(S)
ggs_autocorrelation(S)
ggs_geweke(S)
