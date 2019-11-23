##########################################################
# ECON 5529: Applied Bayesian Statistics for Economcis   #
# UMKC, Fall 2017                                        #
# Ellis Scharfenaker                                     #
# Lecture 6                                              #
##########################################################
# Loess fits with the EPWT
#----------------------------------------------
library(ggplot2)
library(data.table)
setwd("~/Dropbox/UMKC/ECON5529/Lectures/Lecture6Regression")

data<-read.csv("EPWTv4.csv",header=TRUE)
head(data,10)
summary(data)

str(data)
# subset the data
D<-subset(data, (Year >= 1983 & Year <= 2007) & (Quality=="A" | Quality=="B"), select=c(1,2,3,4,10,15,18,19))
summary(D)
# or using data.table
d<-data.table(data)
d<-d[Year >= 1983 & Year <= 2007 & (Quality=="A" | Quality=="B"),.(Country,Id,Year,Pop.000s.,X,k,x.fc.,rho.fc.)]
# or
attach(data)
d<-data[(Year >= 1983 & Year <= 2007) & (Quality=="A" | Quality=="B"), c(1,2,3,4,10,15,18,19)]
summary(d)

# Missing values or NA's can be removed by useing complete.cases
data.cc<-data[complete.cases(data),]


# Plot growth in labor productivity by country
ggplot(d,aes(x=Year,y=X,color=Country)) + geom_line(size=.5) + theme_bw()

# Plot growth in  by country
ggplot(d,aes(x=Year,y=x.fc.,color=Country)) + geom_line(lwd=.5,lty=2) + scale_y_log10() +
  theme_bw() + ggtitle("GDP")

k<-d$k
x<-d$x
plot(k,x,pch=20)

# To estimate a Cobb-Douglas production function x=Ak^a take log(x)=log(A)+alog(k)

cd<-lm(log(x)~log(k))
A<-cd$coefficients[1]
a<-cd$coefficients[2]
lines(exp(A)*c(1:max(k))^a,col="red")


# How about a production isoquant?

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
}

loessfit(.5)

manipulate(loessfit(alpha),
           alpha = slider(.1,2,initial=.75,step=.1,label="Alpha")
)
