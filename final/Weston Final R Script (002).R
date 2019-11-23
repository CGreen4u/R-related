
#         FINAL PROJECT           #

library(data.table)
library(rstan)
rstan_options(auto_write = TRUE)
library(dplyr)
library(ggmcmc)
library(coda)
library(bindr)
library(mcmcplots)
library(ggplot2)
library(arm)
library(rstan)
library(TeachingDemos)
library(reshape2)
library(plyr)
library(coda)
library(labstats)
library(BayesCombo)

data<-data.table(read.csv("Final Data.csv",header=TRUE))

CCD<-data[complete.cases(data),]

D<-CCD[(sic >= 2000 & sic <= 3900), c(1,2,3,4,5,6,7,8)]

head(D)

D[,surv:=length(fyear),by=gvkey]

SurvD<-D[ which(surv=="55")]

SurvD[,rbar:=mean(sale),by=fyear] 

SurvD[,growth:=log(sale/rbar)] 

head(SurvD)

DD<-list(N=length(SurvD$growth),y=SurvD$growth)

##################### Deciles
SizeD<-D[ which(surv=="55")]
D[, q.sales:=findInterval(sale, quantile(sale,seq(0,1,.01)), rightmost.closed=TRUE),by=fyear]
SizeD[, q.sales:=findInterval(sale, quantile(sale,seq(0,1,.01)), rightmost.closed=TRUE),by=fyear]

decile<-data.table(SizeD[ which(SizeD$q.sales>=10)])
decile<-data.table(SizeD[ which(SizeD$q.sales>=20)])
decile<-data.table(SizeD[ which(SizeD$q.sales>=30)])
decile<-data.table(SizeD[ which(SizeD$q.sales>=40)])
decile<-data.table(SizeD[ which(SizeD$q.sales>=50)])
decile<-data.table(SizeD[ which(SizeD$q.sales>=60)])
decile<-data.table(SizeD[ which(SizeD$q.sales>=70)])
decile<-data.table(SizeD[ which(SizeD$q.sales>=80)])
decile<-data.table(SizeD[ which(SizeD$q.sales>=90)])

head(SizeD)

decile[,rbar:=mean(sale),by=fyear]
decile[,growth:=log(sale/rbar)]

DD<-list(N=length(decile$growth),y=decile$growth)
#####################


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

STANout <-stan(model_code=stan.code,data=DD,iter=10000,chains=3)

print(STANout, digits_summary = 3)

plot(STANout)

samplesAll <- as.data.frame(rstan::extract(STANout))

cdf.samp <- ecdf(samplesAll$beta1)

summary(samplesAll$beta1)

1-cdf.samp(1)


hist(samplesAll$beta1, breaks = 100, main = "Excluding Bottom 90%", xlab = "Beta1 Samples")


hist(samplesAll$beta1, breaks = 100, main = "Posterior Dist. for Beta1 of the Manufacturing Industry with Growth Defined by Assets", xlab = "Beta1 Samples")
abline(v=emp.hpd(samplesAll$beta1)[1],lwd=2)
abline(v=emp.hpd(samplesAll$beta1)[2],lwd=2)

emp.hpd(samplesAll$beta1)



# page 479



S <- ggs(STANout)



ggs_traceplot(S)
ggs_density(S)
ggs_compare_partial(S)
ggs_running(S)
ggs_autocorrelation(S)
ggs_geweke(S)
ggs_Rhat(S)





#########################################  USING ASSETS  ###################################################

SurvD[,abar:=mean(at),by=fyear] 

SurvD[,growth:=log(at/abar)] 

head(SurvD)

####################

DD<-list(N=length(SurvD$growth),y=SurvD$growth)

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

STANout <-stan(model_code=stan.code,data=DD,iter=10000,chains=3)


print(STANout, digits_summary = 3)

plot(STANout)

S <- ggs(STANout)

ggs_histogram(S)
ggs_traceplot(S)
ggs_density(S)
ggs_compare_partial(S)
ggs_running(S)
ggs_autocorrelation(S)
ggs_geweke(S)
ggs_Rhat(S)


###############  Finance, Insurance, and Real Estate  #####################

D<-CCD[(sic >= 6000 & sic <= 6799), c(1,2,3,4,5,6,7,8)]

D[,surv:=length(fyear),by=gvkey]

head(D)

SurvD<-D[ which(surv=="55")]

SurvD[,rbar:=mean(sale),by=fyear] 

SurvD[,growth:=log(sale/rbar)] 

head(SurvD)


#####################


DD<-list(N=length(SurvD$growth),y=SurvD$growth)

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

STANout <-stan(model_code=stan.code,data=DD,iter=10000,chains=3)


print(STANout, digits_summary = 3)

plot(STANout)

S <- ggs(STANout)


ggs_histogram(S)

beta1_STANout<- rstan::extract(STANout, pars='beta1')

print(beta1_STANout)

ggs_traceplot(S)
ggs_density(S)
ggs_compare_partial(S)
ggs_running(S)
ggs_autocorrelation(S)
ggs_geweke(S)
ggs_Rhat(S)

hist(samplesAll$beta1, breaks = 100, main = "Posterior Distribution for Beta1 of The F.I.R.E. Industry", xlab = "Beta1 Samples")
abline(v=emp.hpd(samplesAll$beta1)[1],lwd=2)
abline(v=emp.hpd(samplesAll$beta1)[2],lwd=2)

emp.hpd(samplesAll$beta1)



