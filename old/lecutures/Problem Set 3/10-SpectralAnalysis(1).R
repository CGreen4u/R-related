##########################################################
# ECON 5529: Applied Bayesian Statistics for Economcis   #
# UMKC, Fall 2017                                        #
# Ellis Scharfenaker                                     #
# Lecture 10                                             #
##########################################################
# Time Series and Spectral Analysis
  
#---------------------------------------------------------------
# HP Filter

# Data from BEA on business cycles
setwd("~/Dropbox/UMKC/ECON5529/Lectures/Lecture10TimeSeries")
data<-read.csv("RGDP.csv",header = T)
gdp<-data$GDP
gdp<-gdp[1:(length(gdp)-2)] # 64 years of quarterly data

dts<-ts(gdp, frequency = 4, start = c(1948, 1))
plot(dts)
plot(log(dts))


# The library "mFilter" contains an HP filter function
library("mFilter")

plot(hpfilter(gdp,freq=1600))
# Or we can define the function ourselves
hp.filter <- function(x,lambda=1600){
  eye <- diag(length(x))
  trend <- solve(eye+lambda*crossprod(diff(eye,lag=1,d=2)),x)
  cycle <- x-trend
  return(list("cycle"=cycle,"trend"=trend))
}
hpgdp<-hp.filter(log(gdp),lambda=1600)
plot(log(dts),lwd=2)
lines(ts(hpgdp$trend,frequency = 4, start = c(1948, 1)),col="red",lwd=1)

plot(hpgdp$cycle,type="l")

# The residuals from the HP Filter are a measure of the 
# business cycle and other short-run fluctuations.

plot(ts(hpgdp$cycle,frequency = 4, start = c(1948, 1)),ylab="cycle")

# log-linear detrending 

lld<-lm(log(gdp)~c(1:length(gdp)))
plot(log(gdp),pch=20)
lines(hpgdp$trend,col="red",lwd=2)
abline(lld,col="blue",lwd=2)

# Compare the cycles

plot(ts(lld$residuals,frequency = 4, start = c(1948, 1)),type="l",col="blue",ylab="cycle") # Low-pass filter
lines(ts(hpgdp$cycle,frequency = 4, start = c(1948, 1)),type="l") # Band-pass filter
lines(ts(diff(log(gdp)),frequency = 4, start = c(1948, 1)),col="red") # High-pass filter

#---------------------------------------------------------
# Estimating the population spectrum

library(manipulate)
manipulate(curve(dc+amp*cos(f*x+ph),0,10,ylim=c(-5,5)),
           dc=slider(0,10,label="DC"),
           amp=slider(1,10,label="amplitude"),
           f=slider(1,10,label="frequency"),
           ph=slider(0,10,label="phase"))

# Create cyclical data
x<-seq(0,100,by=1)
y<-cos(.5*x)   
plot(x,y,type="l")
no<-length(y)
plot(x,fft(y),type="l")
FF = 1/sqrt(no)*abs(fft(y)) # Fourier transform
P = FF[1:(length(y)/2+1)] # Only need the first (n/2)+1 values of the FFT result.
f = (0:(length(y)/2))/length(y) # this creates harmonic frequencies from 0 to .5 in steps of 1/length(y).
plot(f, P, type="l") # This plots the periodogram;


# find max frequency
ind<-cbind(f,P)
maxfreq<-ind[ind[,2]==max(ind[,2])][1]
abline(v=maxfreq,col="red")

# or radial frequency \omegga
omegga<-2*pi*maxfreq

# In periods
per<-1/maxfreq
plot(x[1:13],y[1:13],type="l")

no/per # here we see there are three complete cycles over the sample

par(mfcol=c(1,2))
plot(x,y,type="l")
plot(1/f, P,type="l",lwd=1.5,ylab="amplitude",xlab="1/freq=period")
abline(v=1/maxfreq,col="red")
par(mfcol=c(1,1))

#-------------------------------------------------
# Add two signals

x<-seq(0,100,by=1)
y1<-cos(.5*x-.5)  
y2<-cos(x)
yy<-y1+y2
plot(x,yy,type="l")
no<-length(yy)
plot(x,fft(yy),type="l")
FF = 1/sqrt(no)*abs(fft(yy)) # Fourier transform
P = FF[1:(length(yy)/2+1)] # Only need the first (n/2)+1 values of the FFT result.
f = (0:(length(yy)/2))/length(yy) # this creates harmonic frequencies from 0 to .5 in steps of 1/length(y).
plot(f, P, type="l") # This plots the periodogram;



#-------------------------------------------------
# One signals plus noise

noise.spectrum.plot<-function(amp, freq, phase, noise){
  x<-seq(0,100,by=1)
  y<-amp*sin(phase+freq*x)+rnorm(length(x),0,noise)   
  #spec.period = 1/(.5*range(length(y))/2)/length(y)/2
  #spec.power = abs(fft(y))
  FF = abs(fft(y))*2 
  PH = Arg(fft(y))
  P = (1/length(y))*FF[1:(length(y)/2+1)] # Only need the first (n/2)+1 values of the FFT result.
  PP = (1/length(y))*PH[1:(length(y)/2+1)] 
  f = (0:(length(y)/2))/length(y) # this creates harmonic frequencies from 0 to .5
  par(mfcol=c(1,2))
  plot(x,y,type="l")
  plot(1/f,P,type="l",lwd=1.5,ylab="amplitude",xlab="1/freq=period")
  par(mfcol=c(1,1))
}
noise.spectrum.plot(1,1,0,.1)

manipulate(noise.spectrum.plot(1,f,0,sigma),
           f=slider(.02*pi,2,label="freq"),
           sigma=slider(0,2,step =.1,label="noise"))


#--------------------------------------------------
# Looking at the BEA data spectrum

par(mfrow=c(1,1))
plot(gdp,type="l")
plot(log(gdp),type="l")
freqGDP<-fft(log(gdp))
head(freqGDP)

# Notice that the first (DC) component is a real number. 
# The rest of the coefficients are complex numbers.
# We can see that they come in complex conjugate pairs

cbind("head"=head(freqGDP[-1]),"tail"=tail(freqGDP))


# We can recover the original data by inverting the Fourier transform

head(Re(fft(freqGDP,inverse=T)))/length(freqGDP)

head(log(gdp))

# The amplitude of the Fourier coefficients indicates the amount 
# of the variation in the original time domain series that originate 
# at each frequency

plot(abs(freqGDP),type="l",ylim=c(0,25),ylab="amplitude",xlab="frequency")

# Here we can see the symmetry of the transform. 
# It is easier to examine the amplitude by removing the DC component and 
# plotting only the lower frequency components. In this case T-1=271, 
# so the frequencies in quarters run from 1/271 cycles per quarter, 
# to 1/2 per quarter. Equivalently the periods run from 271 quarters to 
# 2 quarters.
per<-sapply(4:length(gdp),function(i) length(gdp)/(4*i))
plot(per,abs(freqGDP)[4:length(freqGDP)],type="l",ylim=c(0,30),xlim=c(1.5,20),xlab="period years",ylab="amplitude") #sqrt


# This type of spectrum, in which most of the variation corresponds to 
# very low frequencies, is characteristic of many trended macroeconomic
# time series. The Fourier decomposition puts a very high weight on very
# low frequencies (long periods) to match the trend, which is represented
# as the beginning of a very long cycle in the decomposition.

# Business cycle fluctuations show up (but not very well) in the "bump" in
# the amplitudes around 8-12 year period.

abline(v=8,lty=2,col="red")
abline(v=12,lty=2,col="red")


# In order to highlight the information at business cycle frequencies, 
# economists sometimes first-difference the logged data:

y<-diff(log(gdp))
plot(y,type="l")
FF = abs(fft(y)) #sqrt
P = (1/length(y))*FF[1:(length(y)/2+1)] # Only need the first (n/2)+1 values of the FFT result.
per<-sapply(2:length(gdp),function(i) length(gdp)/(4*i))
plot(per,P[2:length(gdp)],type="l",ylim=c(0,.002),xlab="period years",ylab="amplitude")
abline(v=8,lty=2,col="red")
abline(v=16,lty=2,col="red")


# The frequency domain analysis of differences of logged real GDP 
# does show a noticeable smeared-out peak at 8-16 years, 
# representing the business cycle frequency fluctuations. 
# The first-differencing makes the series much noisier, 
# particularly at high frequencies. If one has a strong 
# prior that there is no systematic business cycle fluctuation,
# the first-differenced data will minimize the data evidence 
# for business cycles and "whiten" the series toward completely 
# random fluctuations since it is a high-pass filter.


# Compare this spectrum to the low-pass filter using the log-linear
# detrending method.

lld<-lm(log(gdp)~c(1:length(gdp)))
y<-lld$residuals
plot(y,type="l")
FF = abs(fft(y)) 
P = (1/length(y))*FF[1:(length(y)/2+1)] # Only need the first (n/2)+1 values of the FFT result.
per<-sapply(2:length(gdp),function(i) length(gdp)/(4*i))
plot(per,P[2:length(gdp)],type="l",xlab="period years",ylab="amplitude")
abline(v=8,lty=2,col="red")
abline(v=16,lty=2,col="red")

# The frequency domain analysis of logged linear detrended real GDP 
# show a more noticeable  peak at 8-16 years. Since this is a low-pass
# filter the low frequency cycles remain.

# We can compare this to the band-pass filter using the HP-filter to 
# detrend the series.
  
y<-hpgdp$cycle
plot(y,type="l")
FF = abs(fft(y)) #sqrt
P = (1/length(y))*FF[1:(length(y)/2+1)] # Only need the first (n/2)+1 values of the FFT result.
per<-sapply(2:length(gdp),function(i) length(gdp)/(4*i))
plot(per,P[2:length(gdp)],type="l",xlab="period years",ylab="amplitude")
abline(v=4,lty=2,col="red")
abline(v=8,lty=2,col="red")

# The frequency domain analysis of the HP detrended real GDP 
# shows a more noticeable peak at the 7-10 year period. 
# Since this is a band-pass filter the low and high frequency 
# cycles no longer remain.
 
# note we can also use the base function spectrum
yspec<-spectrum(y,span=5,plot=F)
plot(1/yspec$freq,yspec$spec,type="l",ylab="spectrum",xlab="period")

#---------------------------------------------
# Bayesian Spectral Analyis - Zero Padding

y<-hpgdp$cycle
plot(y,type="l")
y <- y-mean(y) # Subtract mean (DC Component)
no <- length(y) # length of series
nz <- no^2 # square length
zpad <- rep(.00001, nz-no) # zero padding
d1<-c(y,zpad) # add zero pad
fftabs <- 1/sqrt(no)*abs(fft(d1)) # Fourier transform
psd2<- fftabs*fftabs # Calculate periodogram

P1side<- psd2[1:(nz/2+1)] # Only need the first (n/2)+1 values of the FFT result.
freq<- (1/nz)*(1:(nz/2+1)-1) # Create frequencies from 0 to .5
plot(freq,P1side,type="l")


# The posterior probabiliy for estimating the frequency of a single sinusoidal 
# signal is in the form of a Student’s t distribution. 

df <- (y%*%y)/no # mean squared average of data
p.stud<-(1-(2*P1side)/(no*df))^((2-no)/2) # posterior density
sum.p.stud<-sum(p.stud)
Norm.p.stud<-p.stud/(sum.p.stud*df)
plot(freq,Norm.p.stud,type="l")

ind<-cbind(freq,Norm.p.stud)
head(ind[order(-Norm.p.stud),],1)
maxfreq<-ind[ind[,2]==max(ind[,2])][1]

abline(v=maxfreq,col="red")

# since data is in quarters there are 10.12 full cycles
# compare this to the 11 estimated NBER cycles
length(y)/(1/maxfreq)

# The single harmoic model has the highest posterior probability
# for a 7 year cycle
(1/maxfreq)/4 



#---------------------------------------------
# If we generate a time series from independent normal 
# random shocks, the spectrum is flat.

y<-rnorm(1000)
plot(y,type="l")
no<-length(y)
FF = 1/sqrt(no)*abs(fft(y)) # Fourier transform
P = FF[1:(length(y)/2+1)] # Only need the first (n/2)+1 values of the FFT result.
f = (0:(length(y)/2))/length(y) # this creates harmonic frequencies from 0 to .5 in steps of 1/length(y).
plot(f, P, type="l") # This plots the periodogram;

# Multiple Time Series
#------------------------------------------------------
# Multiple Time Series

# Generate two time series
x1<-seq(0,10,by=.01)
y1<-5*cos(2*pi*x1) 

x2<-seq(0,10,by=.01)
y2<-1*cos(2*pi*x2+2) 

plot(x1,y1,type="l")
lines(x2,y2,col="red")

# Cross spectrum
CS = Conj(abs(fft(y1)))*abs(fft(y2)) 
no<-length(y1)
P = CS[1:(length(y1)/2+1)] # Only need the first (n/2)+1 values of the FFT result.
f = (0:(length(y1)/2))/length(y1) # this creates harmonic frequencies from 0 to .5 in steps of 1/length(y).
plot(f, P, type="l") # This plots the cross-spectrum;

# Coherence
gxy <- spectrum(cbind(y1,y2),spans=c(2,2),plot=F)
plot(gxy$freq,gxy$coh,type="l",ylab="Coherence",xlab="freqency")


# Gain
Gain=CS/(abs(fft(y1))*abs(fft(y1))) 
P = Gain[1:(length(y1)/2+1)] # Only need the first (n/2)+1 values of the FFT result.
plot(f, P, type="l") # This plots the cross-spectrum;


# Phase shift
Phase=Arg(Conj(fft(y1))*fft(y2)) 
P = Phase[1:(length(y1)/2+1)] # Only need the first (n/2)+1 values of the FFT result.
plot(f, P, type="l") # This plots the cross-spectrum;


# National Income Employment data
KV<-read.csv("KV.csv",header = T)

x<-KV$L
y<-KV$X
hpX<-hp.filter(log(x),lambda=1600)$cycle
hpL<-hp.filter(log(y),lambda=1600)$cycle

plot(hpL,type="l",col="red")
lines(hpX,type="l",col="blue")

gxl <- spectrum(hpL,spans=5)
plot(1/gxl$freq,gxl$spec,type="l")

# Cross spectrum
CS <- Conj(abs(fft(hpL)))*abs(fft(hpX)) 
P <- (1/length(hpL))*CS[1:(length(hpL)/2+1)] 
per<-sapply(2:length(hpX),function(i) length(hpX)/(4*i))
plot(per,P[2:length(hpX)],type="l",xlab="period years",ylab="Cross Spectrum")

# Coherence
gxy <- spectrum(cbind(hpL,hpX),spans=c(2,2),plot=F)
plot(1/gxy$freq,gxy$coh,type="l",ylab="Coherence",xlab="period",xlim=c(0,50))
plot(gxy$freq,gxy$coh,type="l",ylab="Coherence",xlab="freqency")

# Gain
Gain<-CS/(abs(fft(hpX))*abs(fft(hpX))) 
P <- Gain[1:(length(hpX)/2+1)] # Only need the first (n/2)+1 values of the FFT result.
per<-sapply(2:length(hpX),function(i) length(hpX)/(4*i))
plot(per, P[2:length(hpX)], type="l",ylab="Gain",xlab="period") # This plots the gain-spectrum;


# Phase shift
Phase <- Arg(Conj(fft(hpX))*fft(hpL)) 
P <- Phase[1:(length(y1)/2+1)] # Only need the first (n/2)+1 values of the FFT result.
plot(1/per, P[2:length(hpX)], type="l",ylab="Phase",xlab="period") # This plots the Phase shift;



