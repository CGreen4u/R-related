
## GVKey company codes
## include:
# company name, 
# SIC - standard industry classification code, 
# FYEAR - fiscal data year
# SALE - sales/turnover (net)
# AT - total assets

# import the data into R as a data.table 
setwd("/Users/chloesegale/Desktop/econ 5529 - bayesian statistics/Final Project")


library(data.table)
data<-data.table(read.csv("400be901b8744372.csv",header=TRUE))

# subset the data to only include the manufacturing industry 
# (SIC 2000-3900) and complete cases. You should have no NAs 
# after this step. 
data<-data[sic >= 2000 & sic <= 3900,]
data<-data[complete.cases(data),]
complete
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



vis.model="data {
int<lower=1> N; //N is an integer value that has a lower bound of 1 (id)
int<lower=1> J; // number of Firms
int<lower=1,upper=J> firm[N]; /////
///int<lower=1,upper=J> vector[N]; ////dding so time span is by firm??
vector[N] g; // g is a length, N vector of integers // in ar model
//vector[N] k; // k is a length, N vector of integers //justadded
} 
parameters {
vector[J] a; //A is a length, J vector of integers represents t
vector[J] p; //a is a length, J vector of integers
real<lower=0> mu_a; 
real<lower=0> mu_p;
real<lower=0,upper=100> sigma_a;
real<lower=0,upper=100> sigma_p;
real<lower=0> sigma_g;
}
transformed parameters {
vector[N] g_hat; //xhat is a length, N vector of integer ////firm varying by 
for (i in 2:N)
g_hat[i] = a[firm[i]] + p[firm[i]] * g_hat[i-1]; ///coeff vary by every row of data??

} 
model {
mu_a ~ normal(0, 1);
a ~ normal(mu_a, sigma_a);
mu_p ~ normal(0, 1);
p ~ normal(0.1*mu_p, sigma_p);
g ~ normal(g_hat, sigma_g); //specified priors
//1
//model {
//for (n in 2:N)
//y[n] ~ normal(alpha + beta * y[n-1], sigma);
}"
##from other


nrow(data)
firm=data$id
dat <- list(N = nrow(data), J=J,firm=data$id, g = data$growth)
vis.stan <- stan(model_code = vis.model, data = dat, 
                 iter = 2000, chains = 3,warmup = 500)

