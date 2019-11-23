###
setwd("/Users/chloesegale/Desktop/econ 5529 - bayesian statistics/Problem set 2")
RR<-read.csv("0-RR-data.csv",header=TRUE)
head(RR)
attach(RR)
plot(debtgdp,dRGDP,pch=20,xlab="Debt/GDP ratio ",ylab="Growth of Real GDP")

plot(RR$debtgdp,RR$dRGDP,pch=20,xlab="Debt/GDP ratio",ylab="Growth of Real GDP")
grid()
abline(lm(dRGDP~debtgdp,data=RR))
library(ggplot2)
ggplot(RR,aes(RR$debtgdp,RR$dRGDP,color=Country))+geom_point(size=.5)+theme_bw()

##WritealinearmodelinStanorJAGStoestimatetheelasticityofrealGDPgrowth 
###to the debt/GDP ratio.
debtgdp<-RR$debtgdp
drgdp<-RR$dRGDP

library(rstan)

linear.model="
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
    y ~ normal(beta0+beta1*x, sigma);
}"

dat<-list(N=length(RR$debtgdp),y=RR$dRGDP,x=RR$debtgdp)
stan.fit<-stan(model_code=linear.model,data=dat,iter=10000,chains=3,thin=2)
print(stan.fit)

####Plottheposteriorsdensitiesforthemodelparameters
plot(stan.fit)
S <- as.data.frame(rstan::extract(stan.fit))
beta0<-S$beta0
beta1<-S$beta1
library(TeachingDemos)
hpd.beta0<-emp.hpd(beta0, conf=0.95)
hpd.beta1<-emp.hpd(beta1, conf=0.95)
### Beta0 @ 95% HPD interval
plot(density(beta0), main="Beta0 -95% HPD interval")
abline(v=hpd.beta0[1],col="red")
abline(v=hpd.beta0[2],col="red")
### Beta1 @ 95%
plot(density(beta1), main="Beta1 -95% HPD interval")
abline(v=hpd.beta1[1],col="red")
abline(v=hpd.beta1[2],col="red")

### report summary statistics
print(stan.fit)

m<-ggs(stan.fit)
### d) traceplots
ggs_traceplot(m)
ggs_density(m)
ggs_autocorrelation(m)
ggs_compare_partial(m)
ggs_geweke(m)
ggs_Rhat(m)

library(rstan)
#e) Writeaprogramthatsimulatesfittedregressionlinesfromtheposteriordistributions 
#of your model parameters and plot 100 simulated regression lines over the 
##scatterplot of data.
nlfit<-loess(drgdp~debtgdp,span=.75)
attributes(nlfit)

plot(drgdp~debtgdp,pch=20)
abline(lm(drgdp~debtgdp),col=2)

dom <- seq(from=0, to=3, length=length(K))
lpred<-predict(nlfit, dom, se = TRUE)
lines(dom,lpred$fit,col="darkred",lwd=3)

lines(dom,lpred$fit+1.96*lpred$se.fit,col="red")
lines(dom,lpred$fit-1.96*lpred$se.fit,col="red")

library(manipulate)
library(scales)
loessfit<-function(alph){
  lwf<-loess(drgdp~debtgdp,span=alph)
  plot(debtgdp,drgdp,pch=20,col=alpha("black",.5),xlab="debtgdp",ylab="drgdp")
  j <- order(debtgdp)
  lines(debtgdp[j],lwf$fitted[j],col="red",lwd=3)
}

loessfit(.5)

manipulate(loessfit(alpha),
           alpha = slider(.1,2,initial=.75,step=.1,label="Alpha")
)
