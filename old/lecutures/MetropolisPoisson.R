##########################################################
# ECON 5529: Applied Bayesian Statistics for Economcis   #
# UMKC, Fall 2017                                        #
# Ellis Scharfenaker                                     #
# Lecture 5                                              #
##########################################################

# Metropolis-Hastings Algorithm for Poisson Distribution

x<-c()  # create empty variable
x[1]<-10  # set initial value

# Metropolis Hastings

for(i in 2:10000){
  u1<-runif(1,0,1) # draw single value from [0,1]
  ifelse(u1>0.5, y<-x[i-1]+1, y<-x[i-1]-1) # Create random unit step: if u is greater than 0.5 +1 else -1
  r<-dpois(y,3)/dpois(x[i-1],3) # Test if new value is more probable then previous value
  u2<-runif(1,0,1)
  ifelse(u2 <= r, x[i]<-y,x[i]<-x[i-1])
}

# Plot traceplot
plot(x[1:200],type="l")

# Compare MH to density
par(mfrow=c(1,2))
plot(dpois(x=0:10, lambda=3), type="b")
hist(x,prob=T,breaks=20,add=F,col="grey")
par(mfrow=c(1,1))

# Calculate sample mean
mean(x)


