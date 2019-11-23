##########################################################
# ECON 5529: Applied Bayesian Statistics for Economcis   #
# UMKC, Fall 2017                                        #
# Ellis Scharfenaker                                     #
# Lecture 6                                              #
##########################################################
# Poisson Regression

library(ggplot2)
library(data.table)

setwd("~/Dropbox/UMKC/2016_Fall/ECON5529/Lectures/Lecture5")

D<-read.csv("StrikeWS.csv")
plot(D$Year,D$Strikes,pch=20)

plot(D$WS,D$Strikes,pch=20,xlab="Wage Share",ylab="Strikes")
grid()
abline(lm(Strikes~WS,data=D))
summary(lm(Strikes~WS,data=D))

pfit <- glm(Strikes ~ WS, family="poisson", data=D)

summary(pfit)
betas<-pfit$coefficients
curve(exp(betas[1]+betas[2]*x),40,60,add=T,col="red")

# The coefficient is the expected log strike count for a one-unit increase in the wage share 
require(ggplot2)

newdata = data.frame(x=.40)
predict(pfit, newdata, type="response")


D$shat <- predict(pfit, type="response")
## order 
D <- D[with(D, order(Strikes, WS)), ]

## create the plot
ggplot(D, aes(x = WS, y = shat)) +
  geom_point(aes(y = Strikes), alpha=.5, position=position_jitter(h=.2)) +
  geom_line(size = 1) +
  labs(x = "Wage share", y = "Expected number of strikes")

# In Stan
D<-read.csv("StrikeWS.csv")
dat <- list(x=D$WS,y=D$Strikes,N=nrow(D))

model="
data {
   int<lower=0> N;
   int<lower=0> y[N];
   real x[N];
}
parameters {
  real beta0;
  real beta1;
}
model {
  //beta0 ~ gamma(2, 2);
  //beta1 ~ gamma(2, 2);
  for (n in 1:N) 
  y[n]~poisson(exp(beta0+beta1*x[n]));
}"

curve(dgamma(x,2,2))
stan.out<-stan(model_code=model,data=dat,iter=10000,chains=3,thin=2)

print(stan.out)
traceplot(stan.out)
plot(stan.out)

library(ggmcmc)
S<-ggs(stan.out)
ggs_histogram(S)
ggs_traceplot(S)
ggs_density(S)
ggs_compare_partial(S)
ggs_running(S)
ggs_autocorrelation(S)
ggs_geweke(S)
