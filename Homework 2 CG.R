D<-read.csv("NBER.csv")
attach(NBER.csv)
library(ggplot2)

MC.sim <- function(n,P) {
  sim<-c()
  # n - number of steps, P-left stochastic matrix, x1=inititial value for MC
  m <- ncol(P)
  sim[1] <- sample(1:m,1) # random start
  for(i in 2:n){
    newstate <- sample(1:m,1,prob=P[,sim[i-1]])
    sim[i] <- newstate}
  sim}



###non-simulated
q<-matrix(c(D$Duration..peak.to.trough,D$Duration..trough.to.peak,D$Duration..peak.to.peak,D$Duration..trough.to.trough, nrow=2))

data.cc<-data[complete.cases(D)]
rowSums(q)
colSums(q)

plot(log(D$duration.peak.to.trough), Log(D$Duration..trough.to.peak))

#####MArkov Chains

#Numbers are calculated by collecting the sums of the columns then dividing by the total
#this gave me a collumn that could sum to 1 values range from 0-1
Ptotals<-matrix(c(1806,576,1278,1854),nrow=2)
P<-matrix(c(.5856,.2370,.4144,.7630),nrow=2)
rowSums(P) # right stochastic matrix
P<-t(P) # left stochastic matrix
colSums(P) # Check stochastic condition satisfied 

# simulate 2 state MC
set.seed(12)
sim2<-MC.sim(32,P) # iterations 
plot(sim2,type="P")
plot(sim2)
lines(sim2)
hist(sim2)
plot(P)
lines((P))
sim<-MC.sim(32,P)
plot(sim)
lines(sim)


#Markov chain evolves for each iteration of the difference equation## %*% is notation for dot product
pi0<-c(.2370,.7630)
pi1<-P%*%pi0 
pi2<-P%*%pi1
pi3<-P%*%pi2

# Recursive iteration of the Chapman-Kolmogorov function 500 times 
init <- pi0 
for(i in 1:3) init<-P%*%init 
init

# Plot evolution of each state probability
init <- pi0 
mc.plot<-function(j,state){for(i in 1:j){init<-P%*%init}
  return(init[state])}
mc1<-sapply(1:32,mc.plot,state=1)
mc2<-sapply(1:32,mc.plot,state=2)

plot(mc1,type="p",pch=1,ylim=c(0,1),col=1,xlab="iteration",ylab="Markov Probs.")
points(mc2,col=2,pch=2)
points(mc3,col=3,pch=3)

# Find ergodic state using Perron-Frobenius theorem
eg<-eigen(P) # need to transpose if using right stochastic matrix
ergod<-Re(eg$vectors[,1]/sum(eg$vectors[,1]))



data<-c(rbind(expansion,contraction))
dd<-data.frame("length"=data,"state"=rep(c(1,2),33))
bc<-rep(dd$state, dd$length)

########################################


# Simulating distributions
par(mar=c(.5856,.2370,.4144,.7630),mfrow=c(2,2)) # set plot parameters

curve(dgamma(x,2,9))
hist(rgamma(32,2,9),breaks=20,prob=T,add=T)

curve(dgamma(x,2,9))
hist(rgamma(500,2,9),50,prob=T,add=T)

curve(dgamma(x,2,9))
hist(rgamma(1000,2,9),100,prob=T,add=T)

curve(dgamma(x,2,9))
hist(rgamma(10000,2,9),100,prob=T,add=T)

###qUESTION 2###############################################################################################
library(ggplot2)
library(data.table)
setwd("~/Grad School CG/R")

data<-read.csv("EPWTv4.csv",header=TRUE)
head(data,10)
summary(data)
attach(data)


# Clearing data in set using data.table#
d<-data.table(data)
d<-d[Year >= 1983 & Year <= 2007 & (Quality=="A" | Quality=="B"), .(Country,Id,Year,Quality,X,c)]

# Drop no observations using complete.cases
data.cc<-data[complete.cases(data),]


----------------------------------------------------------------------------------------------
########b Plot a scatter of consumption per capital (c)against income per capita(x)####
#Plot Consumption against income
plot(c,x)
# Plot growth in labor productivity by country



####Write a model in Stan

install.packages("rstan")
library(rstan)
options(mc.cores = parallel::detectCores())

# Model String ##

DD<-list(N=length(data$x),y=log(data$c),x=log(data$x))


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


STANout<-stan(model_code = stan.code,data = DD,iter = 10000,chains = 3)


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

  
Summary(stan)

mean<-.98
1/1-mean



################ Question 3########################
library(rstan)
library(ggplot2)
library(data.table)
setwd("~/Grad School CG/R")


data<-read.csv("RR-processed.csv",header=TRUE)
head(data,10)
summary(data)
data.cc<-data[complete.cases(data)]


plot(data$debtgdp)
plot(data$dRGDP)

plot(data$debtgdp,data$dRGDP, pch=20,xlab="Debt/GDP Ratio",ylab="Growth of Real GDP")

x<-data$debtgdp
k<-data$dRGDP

#Do not log data
##plot(x,k,log="xy",pch=20,xlab="Debt/GDP Ratio",ylab="Growth of Real GDP")
##plot(log(x),log(k),pch=10,xlab="Debt/GDP Ratio",ylab="Growth of Real GDP")

#Code need Rtool#
fit<-lm(x~k,data)
#fit<-lm(log(x)~log(k),data)
lm(log(x),log(k))
lm(x,k)
abline(fit,col="red",lwd=2)
summary(fit)

data.cc<-data[complete.cases(data)]




# Model String ##

DD<-list(N=length(data.cc$debtgdp),y=log(data.cc$dRGDP),x=log(data.cc$debtgpd))


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


STANout2<-stan(model_code = stan.code,data = DD,iter = 10000,chains = 3)
STANout2<-stan(model_code = stan.code,data = DD,iter = 10000,chains = 3)

print(STANout2)

# DONT FORGET TO DETACHextract samples (may have to detach ggmcmc package as it also has an extract function)
detach("package:ggmcmc", unload=TRUE)
detach("package:tidyr", unload=TRUE)

samplesAll<-as.data.frame(extract(STANout2))
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

curve(exp(b0hpd[1]+b1hpd[1]*x)/(1+exp(b0hpd[1]+b1hpd[1]*x)),lwd=3,add=TRUE,col="green ")
curve(exp(b0hpd[2]+b1hpd[2]*x)/(1+exp(b0hpd[2]+b1hpd[2]*x)),lwd=3,add=TRUE,col="green ")

summary(STANout2)

library(coda)
library(ggmcmc)
S <- ggs(STANout2)
ggs_density(S)
ggs_histogram(S)
ggs_caterpillar(S)
ci(S)
ggs_traceplot(S)
ggs_running(S)
ggs_compare_partial(S)
ggs_autocorrelation(S)
ggs_geweke(S)

######################  ########   G

lines(loess.smooth(data$debtgdp,data$dRGDP,span=.5))


#  isoquant


L<-1/x # 1/x=L/X
K<-k/x #x=X/L k=K/L -> K=K/X

# Loess fit
nlfit<-loess(L~K,span=.75)
attributes(nlfit)

plot(L~K,pch=20)
abline(lm(L~K),col=2)

dom <- seq(from=0, to=3, length=length(K))
lpred<-predict(nlfit, dom, se = TRUE)
lines(dom,lpred$fit,col="darkred",lwd=3)

lines(dom,lpred$fit+1.96*lpred$se.fit,col="red")
lines(dom,lpred$fit-1.96*lpred$se.fit,col="red")

library(manipulate)
library(scales)
loessfit<-function(alph){
  lwf<-loess(L~K,span=alph)
  plot(K,L,pch=20,col=alpha("black",.5),xlab="K",ylab="L")
  j <- order(K)
  lines(K[j],lwf$fitted[j],col="red",lwd=3)
  lines(loess.smooth(K,L,span=.80))
}

loessfit(.5)

manipulate(loessfit(alpha),
           alpha = slider(.1,2,initial=.5,step=.1,label="Alpha")
)

