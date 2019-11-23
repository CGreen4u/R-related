##########################################################
# ECON 5529: Applied Bayesian Statistics for Economcis   #
# UMKC, Fall 2017                                        #
# Ellis Scharfenaker                                     #
# Lecture 7                                              #
##########################################################
# Hierarchical Models


schools<-data.frame("students"=rep(NA,12),"freq"=rep(NA,12))

schools$students<-c(47,148,119,810,211,196,148,215,207,97,256,360)
schools$dropouts<-c(0,18,8,46,8,13,9,31,14,8,29,24)
schools$freq<-schools$dropouts/schools$students
attach(schools)

y<-dropouts
n<-students

th<-seq(0,1,length=1000)

# Estimate dropout probability for each school individually
par(mfrow=c(4,3),mar=c(2,2,1.5,1.5))
for(i in 1:12){
  plot(th,dbeta(th,y[i]+1,n[i]-y[i]+1),type="l",
       col=1,lwd=2,xlim=c(0,.25),ylim=c(0,60),main=LETTERS[i])
  text(.2,20,paste("n =",n[i]))
  text(.2,25,paste("k =",y[i]))
  text(.2,30,paste("k/n =",round(y[i]/n[i],digits=2)))
}

# Estimate dropout probability for district

par(mfrow=c(1,1),mar=c(5,4,4,4))
plot(th,dbeta(th,sum(y)+1,sum(n-y)+1),type="l",
       xlab=expression(theta),
       ylab=expression(paste("p[",theta,"|x]")),
       main="population distribution",
       col=2,xlim=c(0,.25),ylim=c(0,100))

# compare pooled estimate to individual estimates
par(mfrow=c(4,3),mar=c(2,2,1.5,1.5))
for(i in 1:12){
    plot(th,dbeta(th,y[i]+1,n[i]-y[i]+1),type="l",
         col=1,lwd=2,xlim=c(0,.25),ylim=c(0,100),main=LETTERS[i])
    lines(th,dbeta(th,sum(y)+1,sum(n-y)+1),type="l",
    col=2,lwd=2)
  }

#------------------------------------------------------------
# Hierarchical Analysis 

# Log of Parent Distribution p[alpha,beta|x]=p[theta,alpha,beta|x]/p[theta|alpha,beta,x]
kernel.ab.y<-function(n,y,alpha,beta){
    100+lgamma(alpha+beta)+lgamma(alpha+y)+
    lgamma(beta+n-y)-lgamma(alpha)-
    lgamma(beta)-lgamma(alpha+beta+n)
}

# rescale the loglikelihood (+100) so that the posterior has more difference in values

# Log-Likelihood  
llike.ab.y<-function(n,y,alpha,beta){
  sum(kernel.ab.y(n,y,alpha,beta))
}

# Gelman's log prior on alpha and beta (see pp. 110)
lprior.ab<-function(alpha,beta) {-2.5*log(alpha+beta)}


# Grid range of alpha and beta values for evaluating likelihood
alpha<-seq(from=1,to=15,length=1000)
beta<-seq(from=1,to=160,length=1000)

# Log-posterior of parent distribution
lpost.ab.y<-matrix(0,nrow=length(alpha),ncol=length(beta))

for(l in 1:length(alpha)) for(m in 1:length(beta))
  lpost.ab.y[l,m]<-(llike.ab.y(n,y,alpha[l],beta[m])) + lprior.ab(alpha[l],beta[m])

par(mfrow=c(1,1),mar=c(4,4,2,2))
contour(alpha,beta,exp(lpost.ab.y),nlevels=6,xlab="a",ylab="b",font.lab=5,main=expression("Posterior of "*alpha*" and "*beta))
persp(alpha,beta,exp(lpost.ab.y),xlab="a",ylab="b",zlab="",theta=100,phi=20,shade=.3, border=NA, col="grey90",font.lab=5,main=expression("Posterior of "*alpha*" and "*beta))

# Find maximum posterior alpha and beta 
index<-matrix(1:(length(alpha)*length(beta)),nrow=length(alpha))
ind<-index[lpost.ab.y==max(lpost.ab.y)]

alphahat<-alpha[ind-length(alpha)*floor((ind-1)/length(alpha))]
alphahat

betahat<-beta[1+floor((ind-1)/length(alpha))]
betahat

points(alphahat,betahat,col=2,pch=20)

# Posterior theta's for each school
th<-seq(0,1,0.001)
par(mfrow=c(1,1))
plot(th,dbeta(th,alphahat+y[1],betahat+n[1]-y[1]),type="l",col="1",xlim=c(0,.2),ylim=c(0,60),xlab=expression(theta),ylab="density",main="Posterior for each school")
for(i in 2:12){lines(th,dbeta(th,alphahat+y[i],betahat+n[i]-y[i]),type="l",col=1)}

# Plot each school posterior theta individually
par(mfrow=c(3,4))
par(mar=c(2,2,1.5,1.5))
for(i in 1:12){
  plot(th,dbeta(th,alphahat+y[i],betahat+n[i]-y[i]),type="l",
       col=1,xlim=c(0,.2),ylim=c(0,60),main=LETTERS[i]) 
}

# Compare Hierarchical estimates to individual and pooled estimates
for(i in 1:12){
  plot(th,dbeta(th,alphahat+y[i],betahat+n[i]-y[i]),type="l",col=4,xlim=c(0,.2),ylim=c(0,100),main=LETTERS[i]) # hierarchical
  lines(th,dbeta(th,dropouts[i]+1,students[i]-dropouts[i]+1),type="l",col=1) # individual estimate
  lines(th,dbeta(th,sum(dropouts)+1,sum(students)-sum(dropouts)+1),type="l",col=2) # pooled
  legend(.07,110,c("Individual","Population","Hierarchical"),col=c(1,2,4),pch=c(20,20,20),bty="n")
}


# Find HPD regions for hierarchical posterior
library(TeachingDemos)

hpds<-data.frame("l"=rep(NA,12),"h"=rep(NA,12),row.names=LETTERS[1:12])
for(i in 1:12){hpds[i,]<-hpd(qbeta,shape1=alphahat+y[i],shape2=betahat+n[i]-y[i])}
hpds


#---------------------------------------------------
# In Stan
library(rstan)

hierarchical.model<-'
data {
  int<lower=0> J;     // number of schools
  int<lower=0> y[J];  // number of dropouts for j
  int<lower=0> n[J];  // number of students for j
}
parameters {
  real<lower=0,upper=1> theta[J]; // chance of success for j
  real<lower=0,upper=1> mu;  // prior mean chance of success
  real<lower=0.1> kappa;  // prior count
}
transformed parameters {
  real<lower=0> alpha;  // prior success count
  real<lower=0> beta;  // prior failure count
  alpha = mu * kappa;
  beta = (1 - mu) * kappa;
}
model {
  mu ~ uniform(0,1);
  kappa ~ pareto(1.5,.1);  // hyperprior
  theta ~ beta(alpha,beta);  // prior
  y ~ binomial(n,theta);  // likelihood
}
'

dat <- list(J = length(n), y = y, n=n)

Hier.STAN <- stan(model_code = hierarchical.model, data = dat, 
                iter = 10000, chains = 4, warmup = 500)

samplesAll <- as.data.frame(extract(Hier.STAN))

par(mfrow=c(3,4))
par(mar=c(2,2,1.5,1.5))
# Compare Hierarchical estimates to individual and pooled estimates
for(i in 1:12){
  plot(density(samplesAll[,i]),col=4,xlim=c(0,.2),ylim=c(0,100),main=LETTERS[i])
  lines(th,dbeta(th,dropouts[i]+1,students[i]-dropouts[i]+1),type="l",col=1) # individual estimate
  lines(th,dbeta(th,sum(dropouts)+1,sum(students)-sum(dropouts)+1),type="l",col=2) # pooled
  legend(.07,110,c("Individual","Population","Hierarchical"),col=c(1,2,4),pch=c(20,20,20),bty="n")
}

par(mfrow=c(1,1))
plot(density(samplesAll$alpha),main="distribution of alphas")
plot(density(samplesAll$beta),main="distribution of betas")

dena <- density(samplesAll$alpha, kernel=c("gaussian"))
amode<-dena$x[dena$y==max(dena$y)]

denb <- density(samplesAll$beta, kernel=c("gaussian"))
bmode<-denb$x[denb$y==max(denb$y)]

# population distribution 
par(mfrow=c(2,1),mar=c(2,2,3,3))
curve(dbeta(x,amode,bmode),main="population distribution")
plot(density(samplesAll[,1]),col=2,xlim=c(0,1),ylim=c(0,60),main="group distribution")
for(i in 2:12){
  lines(density(samplesAll[,i]),col=2,xlim=c(0,1))
}
