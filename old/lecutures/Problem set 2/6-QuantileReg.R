##########################################################
# ECON 5529: Applied Bayesian Statistics for Economcis   #
# UMKC, Fall 2017                                        #
# Ellis Scharfenaker                                     #
# Lecture 6                                              #
##########################################################
# Quantile Regression

library(quantreg)
library(MASS)
library(ggplot2)

#-------------------------------------------------------------
# Symmetric Data

Sigma <- matrix(c(10,3,3,5),2,2)
set.seed(123)
dd<-mvrnorm(n=1000, rep(0, 2), Sigma)
plot(dd)
y<-dd[,1]
x<-dd[,2]
fit1 <- rq(y~x, tau =seq(.05,.95,.15))

abline(rq(dd[,1]~dd[,2],tau=.5),col="blue")
abline(rq(dd[,1]~dd[,2],tau=.1),col="blue")
abline(rq(dd[,1]~dd[,2],tau=.9),col="blue")
abline(lm(dd[,1]~dd[,2],tau=.9),col="red")
plot(summary(fit1), parm="x")

#-------------------------------------------------------------
# Engle Curves

data(engel)
plot(foodexp ~ income, data = engel, main = "Engel data",pch=20,cex=.5)
abline(lm(foodexp ~ income,data=engel),col="red")
tau<-c(.05, .1, .25, .5, .75, .9, .95)
for(i in tau){
abline(rq(foodexp ~ income,tau=i,data=engel),col=1,lty=2)
}
abline(rq(foodexp ~ income,tau=.5,data=engel),col=4)
qfit<-rq(foodexp ~ income,tau=seq(.05,.95,.05),data=engel)
plot(summary(qfit), parm="income")



#-------------------------------------------------------------
# Bayesian Quantile Regression

library(bayesQR)

out<-bayesQR(foodexp ~ income, data = engel, quantile=tau, ndraw=10000)

out.sum <- summary(out, burnin=500)
plot(foodexp ~ income, data = engel, main = "Engel data",pch=20,cex=.5)
for (i in 1:length(out.sum)){
  abline(a=out.sum[[i]]$betadraw[1,1],b=out.sum[[i]]$betadraw[2,1],lty=i,col=i)
}

plot(out,var=2,plottype="quantile")
plot(out,var=2,plottype="trace")
plot(out,var=2,plottype="hist")
plot(out, var="income", credint=c(.05, .95), plottype="quantile")

lmfit<-lm(foodexp ~ income,data=engel)
abline(h=lmfit$coefficients[2],col=2)

#-------------------------------------------------------------
# Quantile Regression CEO Pay

# read in Execucomp

setwd("~/Documents/*NSSR/Financialization/Buybacks")
C<-read.csv("ceopay.csv",header=T)
summary(C)

CC<-data.table(conm=C$CONAME,gvkey=C$GVKEY,comp=C$TDC2,fyear=C$YEAR)
summary(CC)
CC[is.na(CC$comp)]<-0
CC[,pay:=sum(comp),by=list(gvkey,fyear)]
CC<-CC[CC[,list(row1 = .I[1]), by = list(gvkey,fyear)][,row1]] # change I[2] to I[1] for additional by groups
CC<-CC[CC$pay>0]
plot(CC$fyear[CC$gvkey=="176351"],CC$pay[CC$gvkey=="176351"])

# read in Compustat
D<-read.csv(file.choose()) 
D<-D[D$fyear>= 1992,]
head(D)

DD<-merge(D,CC,by=c("fyear","gvkey","conm"))

ggplot(DD, aes(x=as.factor(fyear),y=log(at))) +
geom_boxplot(alpha=.2) + xlab("") + theme_bw() +   ylab("log[pay]") + xlab("") +
geom_hline(yintercept=0,col="black",lwd=1) + stat_summary(fun.y="mean", geom="point", shape=23, size=1, fill="red",col="red") 

plot(log(DD$at),log(DD$pay))
abline(fit<-lm(log(DD$pay)~log(DD$at)),col="red")
summary(fit)

plot(DD$lq,log(DD$pay))
abline(fit<-lm(log(DD$pay)~log(DD$at)),col="red")
summary(fit)

qfit<-rq(log(DD$pay)~DD$lq,tau=c(.05,.1,.25,.5,.75,.95))
abline(rq(log(DD$pay)~DD$lq,tau=.1),col=3)
abline(rq(log(DD$pay)~DD$lq,tau=.5),col=3)
abline(rq(log(DD$pay)~DD$lq,tau=.95),col=3)

qfit
plot(qfit)
abline(rq(log(DD$pay)~log(DD$at),tau=.1),col=3)
abline(rq(log(DD$pay)~log(DD$at),tau=.5),col=3)
abline(rq(log(DD$pay)~log(DD$at),tau=.95),col=3)


 
