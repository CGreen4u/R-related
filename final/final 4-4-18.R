library(readr)
library(rstan)
library(arm)
library(gplots)
library(ggplot2)
library(ggmcmc)
library(data.table)
library(readxl)
library(MCMCpack)
data <- read_excel("~/Grad School CG/Semester 1/Bayesian Statistics/R/Final EPWT6.0.xlsx", 
                            sheet = "EPWT OECD")
View(data)
data<-data[complete.cases(data),]
data<-data.table(data)
attach(data)
summary(pi)
unique(pi)

MOD1<-lm(data$pistari ~ data$pi, data = data)
plot(MOD1)

MOD7<- MCMCregress(data$pistari ~ data$pi, data = data, burnin = 1000, mcmc = 5000, thin = 1, b0=c(0,0), B0=c(0,0), c0=0.001, d0=0.001)
par(mfrow=c(2,4),mar=c(2,2,2,2))
plot(MOD7)
HPDinterval(MOD7)
summary(MOD7)

plot(desity(MOD7$beta[,1] ))


MOD2<-lm(data$pistari ~ data$pi+ data$xcurppp + data$Fertility , data = data)
MOD8<- MCMCregress(data$pistari ~ data$pi+ data$xcurppp + data$Fertility , data = data, burnin = 1000, mcmc = 1000, thin = 1, b0=c(0,0), B0=c(0,0), c0=0.001, d0=0.001)
par(mfrow=c(2,4),mar=c(2,2,2,2))
plot(MOD7)
HPDinterval(MOD7)
summary(MOD7)


fit = stan(data$pistari ~ data$pi+ data$xcurppp + data$Fertility , data = data, iter=12000,
           warmup=2000, thin=10, chains=3)

###########################
XD<-list(N=length(data$pi),y=data$pi)

###############################
stan.code="
data {
int<lower=0> N;
vector[N] y;
}
parameters {
real beta0;
real beta1;
real <lower=0> sigma;
}
model {
for (n in 2:N)
y[n] ~ normal(beta0 + beta1*y[n-1], sigma);
}"

STANout <-stan(model_code=stan.code,data=XD,iter=15000,chains=3)

print(STANout, digits_summary = 3)

plot(STANout)

samplesAll <- as.data.frame(rstan::extract(STANout))

cdf.samp <- ecdf(samplesAll$beta1)

summary(samplesAll$beta1)

1-cdf.samp(1)

par(mfrow=c(1,1))

hist(samplesAll$beta1, breaks = 100, main = "Excluding Bottom 90%", xlab = "Beta1 Samples")

library(TeachingDemos)
hist(samplesAll$beta1, breaks = 100, main = "Posterior Distribution. for Beta1 of expression(pi)", xlab = "Beta1 Samples")
abline(v=emp.hpd(samplesAll$beta1)[1],lwd=2)
abline(v=emp.hpd(samplesAll$beta1)[2],lwd=2)
##############################################################################

XD2<-list(N=length(data$pistar),y=data$pistar)

STANout <-stan(model_code=stan.code,data=XD2,iter=15000,chains=3)

print(STANout, digits_summary = 3)
par(mfrow=c(1,1))
plot(STANout)

samplesAll <- as.data.frame(rstan::extract(STANout))

cdf.samp <- ecdf(samplesAll$beta1)

summary(samplesAll$beta1)

1-cdf.samp(1)

S <- ggs(STANout)

ggs_traceplot(S)
ggs_density(S)
ggs_compare_partial(S)
ggs_running(S)
ggs_autocorrelation(S)
ggs_geweke(S)
ggs_Rhat(S)

##############################################################################
XD3<-list(N=length(data$pistari),y=data$pistari)

STANout <-stan(model_code=stan.code,data=XD3,iter=10000,chains=3)

print(STANout, digits_summary = 3)

par(mfrow=c(1,1))
plot(STANout)

samplesAll <- as.data.frame(rstan::extract(STANout))

cdf.samp <- ecdf(samplesAll$beta1)

summary(samplesAll$beta1)

1-cdf.samp(1)

S <- ggs(STANout)

ggs_traceplot(S)
ggs_density(S)
ggs_compare_partial(S)
ggs_running(S)
ggs_autocorrelation(S)
ggs_geweke(S)
ggs_Rhat(S)
