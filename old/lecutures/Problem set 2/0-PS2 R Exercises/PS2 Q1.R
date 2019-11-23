
###############################################################
P<-matrix(c(.5856,.2370,.4144,.7630),nrow=2)
rowSums(P) # right stochastic matrix
P<-t(P) # left stochastic matrix
colSums(P) # Check stochastic condition satisfied 

# Write simulation function 
MC.sim <- function(n,P) {
  sim<-c()
  # n - number of steps, P-left stochastic matrix, x1=inititial value for MC
  m <- ncol(P)    
  sim[1] <- sample(1:m,1) # random start 
  for(i in 2:n){
    newstate <- sample(1:m,1,prob=P[,sim[i-1]])
    sim[i] <- newstate
  }
  sim
}

# simulate 2 state MC
set.seed(123)
sim2<-MC.sim(100,P) 
plot(sim2,type="p")
lines(sim2)
hist(sim2)

setwd("/Users/chloesegale/Desktop/econ 5529 - bayesian statistics/Problem set 2/0-PS2 R Exercises")
import<-read.csv("NBER chronology.csv",header=TRUE)
attach(import)

NBER<-subset(import, (Peak.month.number >= 690 & Peak.month.number <= 2496), select=c(5,6))
contraction<-NBER$Duration..peak.to.trough
expansion<-NBER$Duration..trough.to.peak


####Note, once you create the variables “expansion" and 
####“contraction" you can transform them into states with:
###???????
#create vector of columns and call expansions and contraction

data<-c(rbind(contraction,expansion))
dd<-data.frame("length"=data,"state"=rep(c(1,2),33))
bc<-rep(dd$state, dd$length)
hist(bc)



