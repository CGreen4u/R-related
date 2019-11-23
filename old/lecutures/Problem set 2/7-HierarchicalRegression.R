##########################################################
# ECON 5529: Applied Bayesian Statistics for Economcis   #
# UMKC, Fall 2017                                        #
# Ellis Scharfenaker                                     #
# Lecture 7                                              #
##########################################################
# Hierarchical Regression
install.packages("arm")
library(ggplot2)
library(arm)
library(rstan)
library(TeachingDemos)
library(reshape2)
library(plyr)

setwd("/Users/chloesegale/Desktop/econ 5529 - bayesian statistics/Problem Set 2")
data<-read.csv("6-EPWTv4.csv",header=TRUE)
head(data)
unique(data$Country)
attach(data)
D<-data[ Country=="Korea Republicof" | Country=="Cambodia" | Country=="Thailand" |  Country=="Vietnam"| 
          Country=="Laos" | Country=="Japan" | Country=="Singapore" | Country=="Hong Kong", c(1,2,3,15,18)]

colnames(D)<-c("country","id","year","k","xfc")
D<-D[complete.cases(D),]
summary(D)
# Drop all levels for no observations
D$country <- droplevels(D$country)
attach(D)


# create list of countries
cntry<-as.character(unique(D$country))
# check sample size by country
cbind("country"=as.character(unique(D$country)),"n.obs"=as.vector(table(D$country)))

# Running the regression on each country
par(mfrow=c(2,4),mar=c(2,2,2,2))
for(j in cntry) {
  plot(log(D$k[D$country==j]),log(D$xfc[D$country==j]),
       ylab="x",xlab="k",main=j,pch=20)
  abline(lm(log(xfc)~log(k),data=D[D$country==j,]) ,col="blue") # individual reg.
}


par(mfrow=c(1,1))

# Complete Pooling
plot(log(D$k),log(D$xfc),
       ylab="x",xlab="k",main=j,pch=20)

abline(lm(log(xfc)~log(k),data=D) ,col="blue") # pooled reg.

#---------------------------------------------------------------
# Partial pooling (varying intercept)

# log(x_ij)=log(A_j)+a log(k_ij)

# We could regard each country as a separate observation 
# on the time series relation of x to k, and allow the absolute
# level of productivity to vary from country to country.

# Since country is a factor, adding it to the RHS of the regression
# and subtracting 1 adds a dummy to each country.

# That is, when including an input variable with J categories into 
# a classical regression, standard practice is to choose one of
# the categories as a baseline and include indicators for 
# the other J − 1 categories.


######## varying-intercept model, no predictors
function(lmer)
xmean<-lmer(xfc ~ (1|country) ,data=D)
coef(xmean) # mean labor productivity 
fixef(xmean) # pooled mean
ranef(xmean) # deviation from pooled mean

######## varying-intercept model, predictors

cd.pp<-lmer(log(xfc)~log(k) + (1|country ) ,data=D)
display(cd.pp)
coef(cd.pp) # varying intercepts 
fixef(cd.pp) # average regression
ranef(cd.pp) # deviation from average regression
summary(cd.pp)
se.fixef(cd.pp)
se.ranef(cd.pp)


# The no-pooling model includes country indicators

A.pp<-coef(cd.pp)$country[,1] # extract within group intercepts
a.pp<-coef(cd.pp)$country[1,2] #extract between group constant slope

names(A.pp)<-cntry # add country identifies to coefs

# Plot regressions for individual regressions, no pooling, complete pooling
CM<-D[D$country=="Cambodia",]
par(mfrow=c(1,1))
plot(log(CM$k),log(CM$xfc),pch=20)
CMfit<-lm(log(xfc)~log(k),data=CM)
abline(CMfit,col="blue") # no pooling fit
abline(A.pp["Cambodia"],a.pp,col="red") # partial pooling fit
cd.pool<-lm(log(xfc)~log(k),data=D) # pooled fit
abline(cd.pool,col=1)


# Compare partial pooling for all countries

par(mfrow=c(2,4),mar=c(2,2,2,2))
for(j in cntry) {
  plot(log(D$k[D$country==j]),log(D$xfc[D$country==j]),
       ylab="x",xlab="k",main=j,pch=20)
  abline(cd.pool,col=1) # complete pooling
  abline(lm(log(xfc)~log(k),data=D[D$country==j,]) ,col="blue") # individual reg.
  abline(A.pp[j], a.pp ,col="red") # partial pooling (varying intercept)
}
par(mfrow=c(1,1))

# In Stan

pp.stan.model="data {
  int<lower=1> N;
  int<lower=1> J; # number of countries
  int<lower=1,upper=J> country[N];
  vector[N] x;
  vector[N] k;
}
parameters {
  vector[J] A;
  real a;
  real<lower=0> sigma_A;
  real<lower=0> sigma_x;
  real mu_A;
}
model {
  vector[N] x_hat;
  for (i in 1:N)
  x_hat[i] = A[country[i]] + a*k[i];
  
  a ~ normal(0, 1);
  mu_A ~ normal(0, .001);
  sigma_A ~ uniform(0, 100);
  sigma_x ~ uniform(0, 100);
  
  A ~ normal(mu_A, sigma_A);
  x ~ normal(x_hat, sigma_x);
}"

# Need a unique numeric id variable
D$id<-as.numeric(factor(country))
J<-length(unique(D$id))
dat <- list(N = nrow(D), J=J,country=D$id, x = log(D$xfc), k=log(D$k))

vi.stan <- stan(model_code = pp.stan.model, data = dat, 
                    iter = 2000, chains = 3,warmup = 500)
print(vi.stan)
vi.samp <- data.frame(extract(vi.stan))

head(vi.samp)
colMeans(vi.samp[,1:9])

# extract intercept posteriors
A.samp<-vi.samp[,1:8]
colnames(A.samp)<-c(cntry)
head(A.samp)


A.dens<-melt(A.samp)
head(A.dens)
colnames(A.dens)<-c("country","prod")


#plot density of intercept terms
par(mfrow=c(1,1))
hist(A.dens$prod)
hist(A.dens$prod[A.dens$country=="Japan"],main="",xlab="")

ggplot(A.dens, aes(country,prod)) + theme_bw() +
  geom_boxplot(alpha=.2) + xlab("") + ylab("")  + ggtitle("A") +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5, size=10))+
  geom_hline(aes(yintercept=fixef(cd.pp)[1]))


#---------------------------------------------------------------
# Mixed effects (varying slope and intercept)

# log(x_ij)=log(A_j)+a_j log(k_ij)
 
#chloe
#g_t =  a + pg_t-1 + e_t
#g_i,t=ln(s_i,t/mean(sale_t))
#g_i,t = alpha_i +(p_i)*(g_i,t-1)+epsilon_i,t *****
#g_i,t= ln(s_i,t/mean(sale_t))

#log(g_i,t)=log(alpha_i)+(p_i) log(g_i,t-1) (i=1,......,n)
#end chloe



cd.vsi<-lmer(log(xfc)~log(k) + (1+log(k)|country ) ,data=D)
coef(cd.vsi)
fixef(cd.vsi) # average regression
ranef(cd.vsi) # deviation from average regression
display(cd.vsi)
# the unexplained within-country variation has an estimated standard deviation
# sigma_x=0.08, the estimated standard deviation of the country intercepts is
# sigma_A=2.12, the estimated standard deviation of the country slopes is
# sigma_a=0.19
se.fixef(cd.vsi)
se.ranef(cd.vsi)


A.vsi<-coef(cd.vsi)$country[,1] # extract within group intercepts
a.vsi<-coef(cd.vsi)$country[,2] # extract between group slope

names(A.vsi)<-cntry # add country identifies to coefs
names(a.vsi)<-cntry # add country identifies to coefs

# Plot regressions for individual regressions, no pooling, complete pooling
VT<-D[D$country=="Vietnam",]
par(mfrow=c(1,1))
plot(log(VT$k),log(VT$xfc),pch=20)
VTfit<-lm(log(xfc)~log(k),data=VT)
abline(VTfit,col="blue") # no pooling fit
abline(A.pp["Vietnam"],a.pp,col="red") # partial pooling varying intercept fit
abline(lm(log(xfc)~log(k),data=D),col=1) # pooled fit
abline(A.vsi["Vietnam"],a.vsi["Vietnam"],col="purple") # partial pooling varying slope and intercept fit


# Compare partial pooling varying slope and intercept for all countries

par(mfrow=c(2,4))
for(j in cntry) {
  plot(log(D$k[D$country==j]),log(D$xfc[D$country==j]),
       ylab="x",xlab="k",main=j,pch=20)
  abline(cd.pool,col=1) # complete pooling
  abline(lm(log(xfc)~log(k),data=D[D$country==j,]) ,col="blue") # individual reg.
  abline(A.pp[j], a.pp ,col="red") # partial pooling (varying intercept)
  abline(A.vsi[j],a.vsi[j],col="green") # partial pooling varying slope and intercept fit
}
par(mfrow=c(1,1))

# In Stan

# log(x_ij)=log(A_j)+a_j log(k_ij)

#log(g_it)=log(alpha_i)+(p_i) log(g_i,t-1) (i=1,......,n)

vis.model="data {
  int<lower=0> N; //N is an integer value that has a lower bound of zero
  int<lower=1> J; // number of countries
  int<lower=1,upper=J> country[N];
  vector[N] x; // x is a length, N vector of integers
  vector[N] k; // k is a length, N vector of integers
} 
parameters {
  vector[J] A; //A is a length, J vector of integers
  vector[J] a; //a is a length, J vector of integers
  real mu_A;
  real mu_a;
  real<lower=0,upper=100> sigma_A;
  real<lower=0,upper=100> sigma_a;
  real<lower=0> sigma_x;
}
transformed parameters {
  vector[N] x_hat; //xhat is a length, N vector of integer
  for (i in 1:N)
  x_hat[i] = A[country[i]] + a[country[i]] * k[i];
} 
model {
  mu_A ~ normal(0, 1);
  A ~ normal(mu_A, sigma_A);
  mu_a ~ normal(0, 1);
  a ~ normal(0.1*mu_a, sigma_a);
  x ~ normal(x_hat, sigma_x); //specified priors
}"

D$id<-as.numeric(factor(country))
J<-length(unique(D$id))
dat <- list(N = nrow(D), J=J,country=D$id, x = log(D$xfc), k=log(D$k))
dat <- list(N = nrow(data), J=J,country=data$id, x = data$growth, k=log(data$equation))
vis.stan <- stan(model_code = vis.model, data = dat, 
                iter = 2000, chains = 3,warmup = 500)

print(vis.stan)
plot(vis.stan)
traceplot(vis.stan)
vis.samp <- data.frame(extract(vis.stan))

colMeans(vis.samp[,1:21])

# extract intercept posteriors
A.samp<-vis.samp[,1:8]
colnames(A.samp)<-c(cntry)

a.samp<-vis.samp[,9:16]
colnames(a.samp)<-c(cntry)
head(a.samp)

A.dens<-melt(A.samp)
a.dens<-melt(a.samp)

colnames(A.dens)<-c("country","prod")
colnames(a.dens)<-c("country","mpc")


#plot density of intercept terms

ggplot(A.dens, aes(country,prod)) + theme_bw() +
  geom_boxplot(alpha=.2) + xlab("") + ylab("")  + ggtitle("A") +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5, size=10))+
  geom_hline(aes(yintercept=fixef(cd.vsi)[1]),col="red")

ggplot(a.dens, aes(country,mpc)) + theme_bw() +
  geom_boxplot(alpha=.2) + xlab("") + ylab("")  + ggtitle("a") +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5, size=10))+
  geom_hline(aes(yintercept=fixef(cd.vsi)[2]),col="red")

