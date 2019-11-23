

## GVKey company codes
## include:
# company name, 
# SIC - standard industry classification code, 
# FYEAR - fiscal data year
# SALE - sales/turnover (net)
# AT - total assets

# import the data into R as a data.table 
setwd("/Users/chloesegale/Desktop/econ 5529 - bayesian statistics/Final Project)


library(data.table)
data<-data.table(read.csv("400be901b8744372.csv",header=TRUE))

# subset the data to only include the manufacturing industry 
# (SIC 2000-3900) and complete cases. You should have no NAs 
# after this step. 
data<-data[sic >= 2000 & sic <= 3900,]
data<-data[complete.cases(data),]

# create a new variable representing the years survived by each firm. 
# for example data[,surv:=length(fyear),by=gvkey] (this is just ading a column taking a count of the years per id)
data<-data[,surv:=length(fyear),by=gvkey]
years.survived.var<-data$surv
# subset the data to only include firms that have lived the span of the data set
# span of data set is 54 years 2016- 1962 = 54 
data<-data[surv>=54,] 
head(data)

# create a unique numeric identifier for each firm
data$id<-as.numeric(factor(data$gvkey))
J<-length(unique(data$id))
### 184 firms in data


##mean of sales by year, independent of firm
data<-data[,rbar:=mean(sale),by=fyear]

##add result of div equation into table for visibility into equations
##write div equation (salei,t/mean(salet))
data<-data[,equation:=data$sale/data$rbar]
###keep above!!!!


## log of equation to get growth
data<-data[,growth:=log(equation)]
##log growth column is wanted column throughout all. 





### test model

#g_t =  a + pg_t-1 + e_t
#g_i,t=ln(s_i,t/mean(sale_t))
#g_i,t = alpha_i +(p_i)*(g_i,t-1)+epsilon_i,t *****
#g_i,t= ln(s_i,t/mean(sale_t))
##i by firm, t by time

#log(g_i,t)=log(alpha_i)+(p_i) log(g_i,t-1) (i=1,......,n)

library(ggplot2)
library(StanHeaders)
library(rstan)

#testing

      
model="
data 
{int<lower=0> N; //number of observations
  int<lower=1> J; //number of firms
  vector[N] g; //data for Hierarchical AR(1)
  //vector[N] k; //specify what k is later - going to need to be transformed for time series. 
  int<lower=1,upper=J> firm[N]; //number of sets of observations for N??
}
parameters {
vector[J] alpha; //alpha is a length, J vector of integers
vector[J] rho; //rho is a length, J vector of integers
real mu_alpha;
real mu_rho;
real sigma_alpha;
real sigma_rho;
real sigma_g;
}
transformed parameters {
  vector[N] g_hat; //ghat is a length, N vector of integer
 // for (i in 1:N)
for (i in 2:N)
  //g_hat[i] = alpha[firm[i]] + rho[firm[i]] * k[i];
g_hat[i] = alpha[firm[i]] + rho[firm[i]] * g_hat[i-1];
}

model {
mu_alpha ~ normal(0, 1);
alpha ~ normal(0.0001*mu_alpha, -sigma_alpha);
mu_rho ~ normal(0, 1);
rho ~ normal(0.0001*mu_rho, sigma_rho);
g ~ normal(g_hat, sigma_g); //specified priors
//for n in 2:N
//g[n]~normal(alpha+rho*k(n-1), sigma)
//g[n]~normal(g_hat[n],sigma[g])
}

dat <- list(N=nrow(data), J=J, firm=data$id, g=log(data$equation))



stan.out <- stan(model_code = model, data = dat, iter = 2000, chains = 3,warmup = 500)

install.packages("Rtools")



##############
#sucks but only one that works
model="
data {
int<lower=0> N; //total number of observations
int<lower=1> J; //number of firms
int<lower=1,upper=J> firm[N] // Sizes of observations across groups
vector[N] y;
}
parameters {
real alpha;
real beta;
real<lower=0> sigma;
}
transformed parameters {
vector[N] yhat;
for (i in 1:N)
yhat[i]=alpha[firm[i]] + beta[firm[i]]*y[i]; //x[i]  is growth of firm in prior time period

}
model {
for (t in 2:N)
y[t] ~ normal(alpha + beta * y[t-1], sigma);
}"

dat<- list(N=nrow(data),J=J,y=data$growth)
stan.out<-stan(model_code=model,data=dat,iter=10000,chains=3,thin=2)
## doesnt workf
model="
data {
int<lower=0> N; //number of observations
int<lower=1> J; //number of firms
vector[N] y; //data
}
parameters {
real alpha;
real beta;
real<lower=0> sigma;
}
transformed parameters {
vector[N] yhat;
for (i in 1:N)
yhat[i]=alpha[firm[i]] + beta[firm[i]]*x[i]; //x[i]  is growth of firm in prior time period


}
model {
for (t in 2:N)
y[t] ~ normal(alpha + beta * y[t-1], sigma);
}"

######none of below models work
model="
data {
int<lower=0> N;
int<lower=1> J;
int<lower=1,upper=J> firm[N];
int<lower=0,upper=1> x[N];
vector[N] y;
//vector[N] k;
}

parameters {
real<lower=0> sigma;
vector[J] alpha;
vector[J] beta;
//real alpha;
//real beta;
}

transformed parameters {
vector[N] yhat;
for (i in 1:N)
yhat[i]=alpha[firm[i]] + beta[firm[i]]*x[i]; //x[i]  is growth of firm in prior time period

//for (t in 2:N)
//x[t]=y[t-1]
}

//k[i]= y[n-1]
//alpha and beta only depend on i. 
//log(g_i,t)=log(alpha_i)+(p_i) log(g_i,t-1) (i=1,......,n)

model {
for (n in 2:N)
y[n] ~ normal(alpha + beta * yhat[n-1], sigma);
}"

dat<- list(N=nrow(data),J=J,firm=data$id,y=log(data$equation), k=log(data$equation))
stan.out<-stan(model_code=model,data=dat,iter=10000,chains=3,thin=2)
print(stan.out)
##OR

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
real<lower=0,upper=100> sigma_x;
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

dat <- list(N = nrow(data), J=J,country=data$id, x = data$growth, k=log(data$equation))
vis.stan <- stan(model_code = vis.model, data = dat, 
                 iter = 2000, chains = 3,warmup = 500)

print(stan.out)

#######ignore jibberish models above. 


print(stan.out)



##does it appear Gibrat's law holds for long-lived manufacturing firms in the US? 
## explain in terms of the posterior density of p, i.e. how 
## probable is gibrat's law?? resoure lecture 8 posterior pred checking
library(shinystan)
## do i need to set seed? comparison to 8 model checks r example
launch_shinystan(stan.out)
## growth changes with
# Extract MCMC samples
params1<-extract(stan.out)
alpha<-params1$alpha
beta<-params1$beta
sigma<- params1$sigma
nsims <-length(params1$sigma)


# produce the replications from posterior and inspect
N<-nrow(data)
y<-data$growth
yRep <- sapply(1:nsims, function(i) rnorm(N, alpha+beta*y[i-1], sigma)) 


# Check min, max, and mean
min_rep <- apply(yRep, 2, min)
max_rep <- apply(yRep,2,max)
mean_rep <- apply(yRep,2,mean)
sd_rep <- apply(yRep,2,sd)

# Plot posterior mins against actual min
hist(min_rep, main='posterior min & actual min',breaks = 50)
abline(v=min(data$`growth`),lwd=3) 
min(data$growth)
#centerd neat -1.5

# Plot posterior maxs against actual maxs
hist(max_rep, main='posterior max & actual max',breaks = 50)
abline(v=max(data$growth),lwd=3) 
max(data$growth)
# centered

# Plot posterior sds against actual sds
hist(sd_rep, main='posterior max & actual standard deviation',xlim=c(0.21,0.59), breaks = 50)
abline(v=sd(data$growth),lwd=3) 
sd(data$growth)
#not even close
sd(data$`growth`)
# Plot predicted data
hist(data$growth,breaks=50,prob=T,xlim=c(-2,1),col="red")

# Compare to predicted data
for(i in 2:N){
  lines(density(yRep[,i]),col="blue")
}
##looks good
##redefine growth as at (total assets)

##mean of total assets by year, independent of firm
data<-data[,rbar:=mean(at),by=fyear]
##write div equation (salei,t/mean(salet))
equation<-data$at/data$rbar
##add result of div equation into table for visibility into equations
data<-data[,equation:=equation, by=id]
## log of equation to get growth
growth<-log(equation)
data<-data[,growth:=growth, by=id]
##log growth column is wanted column throughout all. 

###final table###     
firmdata<-data.table("Company Code"=data$gvkey,"Fiscal Data Year"=data$fyear,
                     "Company Name"=data$conm, "Total Assets"= data$at,
                     "Sales/Turnover (net)"=data$sale, "Years survived"=data$surv,
                     "Growth of firm size"=data$growth,"numeric identifier"=data$id)
dat<- list(N=nrow(firmdata),J=J,y=firmdata$`Growth of firm size`)
stan.totalassets<-stan(model_code=model,data=dat,iter=10000,chains=3,thin=2)
print(stan.totalassets)
##looks good


##redefine growth as gvkey (company code)

##mean of total assets by year, independent of firm
data<-data[,rbar:=mean(gvkey),by=fyear]
##write div equation (salei,t/mean(salet))
equation<-data$gvkey/data$rbar
##add result of div equation into table for visibility into equations
data<-data[,equation:=equation, by=id]
## log of equation to get growth
growth<-log(equation)
data<-data[,growth:=growth, by=id]
##log growth column is wanted column throughout all. 

###final table###     
firmdata<-data.table("Company Code"=data$gvkey,"Fiscal Data Year"=data$fyear,
                     "Company Name"=data$conm, "Total Assets"= data$at,
                     "Sales/Turnover (net)"=data$sale, "Years survived"=data$surv,
                     "Growth of firm size"=data$growth,"numeric identifier"=data$id)
dat<- list(N=nrow(firmdata),J=J,y=firmdata$`Growth of firm size`)
stan.totalassets<-stan(model_code=model,data=dat,iter=10000,chains=3,thin=2)
print(stan.totalassets)

