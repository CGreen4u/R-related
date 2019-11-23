


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
data<-data.table(read.csv("compustat.csv",header=TRUE))

# subset the data to only include the manufacturing industry 
# (SIC 2000-3900) and complete cases. You should have no NAs 
# after this step. 
data<-data[sic >= 2000 & sic <= 3900,]
data.cc<-data[complete.cases(data),]

# create a new variable representing the years survived by each firm. 
# for example data[,surv:=length(fyear),by=gvkey] (this is just ading a column taking a count of the years per id)
data<-data.cc[,surv:=length(fyear),by=gvkey]
years.survived.var<-data$surv
# subset the data to only include firms that have lived the span of the data set
# span of data set is 55 years 2015- 1962 = 53 (54 only has one firm..)
data<-data[surv>=53,] 
head(data)

# create a unique numeric identifier for each firm
data$id<-as.numeric(factor(data$gvkey))
J<-length(unique(data$id))

        ##mean of sales by year, independent of firm
        data<-data[,rbar:=mean(sale),by=fyear]
        ##write div equation (salei,t/mean(salet))
        equation<-data$sale/data$rbar
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
  
  
### test model
library(rstan)

model="
data {
int<lower=0> N;
int<lower=1> J;
vector[N] y;
}
parameters {
real alpha;
real beta;
real<lower=0> sigma;
}
model {
for (n in 2:N)
y[n] ~ normal(alpha + beta * y[n-1], sigma);
}"

dat<- list(N=nrow(firmdata),J=J,y=firmdata$`Growth of firm size`)
stan.out<-stan(model_code=model,data=dat,iter=10000,chains=3,thin=2)
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
N<-nrow(firmdata)
y<-firmdata$`Growth of firm size`
yRep <- sapply(1:nsims, function(i) rnorm(N, alpha+beta*y[i-1], sigma)) 


# Check min, max, and mean
min_rep <- apply(yRep, 2, min)
max_rep <- apply(yRep,2,max)
mean_rep <- apply(yRep,2,mean)
sd_rep <- apply(yRep,2,sd)

# Plot posterior mins against actual min
hist(min_rep, main='',breaks = 50)
abline(v=min(firmdata$`Growth of firm size`),lwd=3) 
min(firmdata$`Growth of firm size`)
#way off

# Plot posterior maxs against actual maxs
hist(max_rep, main='',breaks = 50)
abline(v=max(firmdata$`Growth of firm size`),lwd=3) 
max(firmdata$`Growth of firm size`)
# centered

# Plot posterior sds against actual sds
hist(sd_rep, main='', breaks = 50)
abline(v=sd(firmdata$`Growth of firm size`),lwd=3) 
#not even close
sd(firmdata$`Growth of firm size`)
# Plot predicted data
hist(firmdata$`Growth of firm size`,breaks=50,prob=T,col="red")

# Compare to predicted data
for(i in 2:N){
  lines(density(yRep[,i]))
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

