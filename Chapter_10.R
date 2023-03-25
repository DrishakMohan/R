# Chapter 10 R examples

# Exhibit 10.1
library(TSA)
data(co2)
# The stats package has another dataset of the same name 'co2', so
# make sure that you have loaded the TSA package before loading the co2 data.
#win.graph(width=4.875, height=3,pointsize=8)
plot(co2,ylab='CO2',main='Monthly Carbon Dioxide Levels at Alert, NWT, Canada')

# Exhibit 10.2
plot(window(co2,start=c(2000,1)),main='Carbon Dioxide Levels with Monthly Symbols', ylab='CO2')
# specify the y-label as "CO2", otherwise it will use "window(co2,start=c(2000,1))"
Month=c("J","F","M","A","M","J","J","A","S","O","N","D")
points(window(co2,start=c(2000,1)),pch=Month)

# First let's try a seasonal trend model, plus a time trend
# (that is, using the methods of Chapter 3):

# A seasonal means model, plus a time trend

data(co2)
month.=season(co2) 
# the period sign is included to make the printout from
# the commands two lines below clearer
model.s=lm(co2~time(co2) + month.-1) # -1 removes the intercept term 
summary(model.s)

# Checking the fit of the model with a residual plot, and a plot of the ACF of the residuals:

plot(rstandard(model.s),ylab='Standardized residuals',type='l')
abline(h=0)

# Some pattern in the residual plot is still evident.

acf(residuals(model.s),main='Sample ACF of Residuals from Seasonal Means Model for co2')

# Note the significant autocorrelations among the residuals.

#### A harmonic regression plus time trend model:

# first creates the first pair of harmonic functions and then fit the model
har.=harmonic(co2,m=1) 
# The m=1 creates one pair of harmonic functions (one cosine term and one sine term).
# If you want additional pairs, you could increase m.
model.h=lm(co2~har. + time(co2))
summary(model.h)
plot(rstandard(model.h),ylab='Standardized residuals',type='l')
abline(h=0)
# A pattern in the residual plot is very clear.

acf(residuals(model.h),main='Sample ACF of Residuals from Harmonic Model for co2')
# Still significant autocorrelations among the residuals.

# What if we add more cosine and sine terms?

har.=harmonic(co2,m=3) 
model.h3=lm(co2~har. + time(co2))
summary(model.h3)
plot(rstandard(model.h3),ylab='Standardized residuals',type='l')
abline(h=0)
acf(residuals(model.h3),main='Sample ACF of Residuals from Harmonic Model for co2')

# Somewhat better, but far from perfect.

# Exhibit 10.3

# First divide the plotting area into a 1 by 2 matrix of plots, i.e. one row
# of two figures. 
#win.graph(width=4.875, height=3,pointsize=8)
par(mfrow=c(1,2))
# The ARMAacf function computes the theoretical acf of an ARMA model.
# Note that the seasonal MA part (1+0.5B)(1+0.8B**12)=(1+0.5B+0.8B**12+0.4B**13).
plot(y=ARMAacf(ma=c(0.5,rep(0,10),0.8,0.4),lag.max=13)[-1],x=1:13,type='h',
xlab='Lag k',ylab=expression(rho[k]),axes=F,ylim=c(0,0.6))
points(y=ARMAacf(ma=c(0.5,rep(0,10),0.8,0.4),lag.max=13)[-1],x=1:13,pch=20)
abline(h=0)
axis(1,at=1:13, labels=c(1,NA,3,NA,5,NA,7,NA,9,NA,11,NA,13))
axis(2)
text(x=7,y=.5,labels=expression(list(theta==-0.5,Theta==-0.8)))

plot(y=ARMAacf(ma=c(-0.5,rep(0,10),0.8,-0.4),lag.max=13)[-1],x=1:13,type='h',
xlab='Lag k',ylab=expression(rho[k]),axes=F)
points(y=ARMAacf(ma=c(-0.5,rep(0,10),0.8,-0.4),lag.max=13)[-1],x=1:13,pch=20)
abline(h=0)
axis(1,at=1:13, labels=c(1,NA,3,NA,5,NA,7,NA,9,NA,11,NA,13))
axis(2)
text(x=7,y=.35,labels=expression(list(theta==0.5,Theta==-0.8)))


# Exhibit 10.4
plot(y=ARMAacf(ar=c(rep(0,11), 0.75), ma=0.4,lag.max=61)[-1],x=1:61,type='h',
xlab='Lag k',ylab=expression(rho[k]),axes=F,ylim=c(-0.4,.8),xlim=c(0,61))
points(y=ARMAacf(ar=c(rep(0,11), 0.75), ma=0.4,lag.max=61)[c(1,11,12,13,
23,24,25,35,36,37,47,48,49,59,60,61)+1],
x=c(1,11,12,13,23,24,25,35,36,37,47,48,49,59,60,61),pch=20)
abline(h=0)
axis(1,at=c(0,1,12,24,36,48,60,61),labels=c(NA,1,12,24,36,48,60,NA))
axis(2, at=c(-0.4,0.0,0.4,0.8))
text(x=40,y=.8,labels=expression(list(Phi==0.75,theta==-0.4)))

plot(y=ARMAacf(ar=c(rep(0,11), 0.75), ma=-0.4,lag.max=61)[-1],x=1:61,type='h',
xlab='Lag k',ylab=expression(rho[k]),axes=F,ylim=c(-0.4,.8))
points(y=ARMAacf(ar=c(rep(0,11), 0.75), ma=-0.4,lag.max=61)[c(1,11,12,13,
23,24,25,35,36,37,47,48,49,59,60,61)+1],
x=c(1,11,12,13,23,24,25,35,36,37,47,48,49,59,60,61),pch=20)
abline(h=0)
axis(1,at=c(0,1,12,24,36,48,60,61),labels=c(NA,1,12,24,36,48,60,NA))
axis(2, at=c(-0.4,0.0,0.4,0.8))
text(x=40,y=.8,labels=expression(list(Phi==0.75,theta==0.4)))

#### An example of a simulated seasonal AR series, with Phi=0.9, from Shumway and Stoffer:

library(astsa)

set.seed(666)
phi=c(rep(0,11),0.9)
sAR=arima.sim(list(order=c(12,0,0),ar=phi),n=37)
sAR=ts(sAR,freq=12)
layout(matrix(c(1,2,1,3),nc=2))
par(mar=c(3,3,2,1),mgp=c(1.6,0.6,0))
plot(sAR, axes=F, main='seasonal AR(P=1)', xlab='year',type='c')
Months=c('J','F','M','A','M','J','J','A','S','O','N','D')
points(sAR, pch=Months, cex=1.25,font=4,col=1:4)
axis(1,1:4);abline(v=1:4,lty=2,col=gray(0.6))
axis(2); box()

# Plotting the TRUE ACF and PACF for this model:
ACF=ARMAacf(ar=phi,ma=0,100)
PACF=ARMAacf(ar=phi,ma=0,100,pacf=T)
plot(ACF,type='h',xlab='lag',ylim=c(-.1,1))
abline(h=0)
plot(PACF,type='h',xlab='lag',ylim=c(-.1,1))
abline(h=0)

# Compare to the SAMPLE ACF and PACF for this simulated series:

x11() # opens new plotting window

par(mfrow=c(2,1))
acf(sAR,lag=36)
pacf(sAR,lag=36)
par(mfrow=c(1,1))

# An example of a real seasonal data set 
# (differenced logged monthly births in U.S. over time):

par(mfrow=c(2,1))
plot(birth)
plot(diff(log(birth)))

acf(diff(log(birth)),61)
pacf(diff(log(birth)),61)
par(mfrow=c(1,1))

## Analysis of the co2 data:

library(TSA)
data(co2)

# Exhibit 10.5
par(mfrow=c(1,1))
acf(as.vector(co2),lag.max=36,
main=expression(Sample~~ACF~~of~~CO[2]~~Levels))

# Exhibit 10.6
plot(diff(co2),main=expression(Time~~Series~~Plot~~of~~the~~First~~Differences~~of~~
CO[2]~~Levels), ylab=expression(First~~Difference~~of~~CO[2])) 

# Exhibit 10.7
acf(as.vector(diff(co2)),lag.max=36,
main=expression(Sample~~ACF~~of~~the~~First~~Differences~~of~~
CO[2]~~Levels))

# Exhibit 10.8
plot(diff(diff(co2),lag=12),main=expression(Time~~Series~~Plot~~of~~the~~First~~and~~
Seasonal~~Differences~~of~~CO[2]~~Levels), 
ylab=expression(First~~and~~Seasonal~~Difference~~of~~C~O[2])) 

# Exhibit 10.9
acf(as.vector(diff(diff(co2),lag=12)),lag.max=36,ci.type='ma',
main=expression(Sample~~ACF~~of~~the~~First~~and~~Seasonal~~Differences~~of~~
CO[2]~~Levels))

# Exhibit 10.10
# Do remember that in the book the MA parameterization uses the minus convention but R uses
# the positive convention, lest you find the estimates from R to be different from the reported values
# in the book!
m1.co2=arima(co2,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12))
m1.co2

# Exhibit 10.11
# first thirteen residuals are omitted from the plot.
plot(window(rstandard(m1.co2),start=c(1995,2)),ylab='Standardized Residuals', type='o',
main=expression(Residuals~~from~~the~~ARIMA(list(0,1,1))%*%(list(0,1,1))[12]~~Model))
abline(h=0)

# Exhibit 10.12
acf(as.vector(window(rstandard(m1.co2),start=c(1995,2))),lag.max=36,
main=expression(ACF~~of~~Residuals~~from~~the~~ARIMA(list(0,1,1))%*%(list(0,1,1))[12]~~Model))

# The figures in the above two exhibits can also be obtained as the first two sub-plots from the
# following command. The plotting convention is slightly different for the first sub-plot.
#win.graph(width=4.875, height=5,pointsize=8)
# by default, the first 13 residuals are ommited; please use ?tsdiag to learn
# more about the function.
tsdiag(m1.co2, gof.lag=36)

# Exhibit 10.13
hist(window(rstandard(m1.co2),start=c(1995,2)),xlab='Standardized Residuals',
main=expression(Residuals~~from~~the~~ARIMA(list(0,1,1))%*%(list(0,1,1))[12]~~Model))

# Exhibit 10.14
#win.graph(width=4, height=4,pointsize=8)
qqnorm(window(rstandard(m1.co2),start=c(1995,2)),main=expression(Normal~~Q-Q~~Plot))
title(main=expression(Residuals~~from~~the~~ARIMA(list(0,1,1))%*%(list(0,1,1))[12]~~Model),
line=3)
qqline(window(rstandard(m1.co2),start=c(1995,2)))

# Shapiro-Wilk test on the residuals:

shapiro.test(rstandard(m1.co2))

# Overfitting:

# Exhibit 10.15
m2.co2=arima(co2,order=c(0,1,2),seasonal=list(order=c(0,1,1),period=12))
m2.co2


#### Analysis of the airline data:

library(astsa)

data(AirPassengers)
y = AirPassengers
plot(y,type='o')  
# nonstationary, with increasing variance over time
# Take log transformation, then differences:

ly=log(y); dly=diff(ly); ddly=diff(dly, lag=12)

plot.ts(cbind(y,ly,dly,ddly), yax.flip=T, main="")

# a plot to show seasonality:
par(mfrow=c(2,1))
monthplot(dly); monthplot(ddly)
par(mfrow=c(1,1))

# Sample ACF and PACF of the first-differenced and seasonally differenced series:

acf2(ddly, 50)

# ACF is cutting off after 1 seasonal lag, and PACF is tailing off at 1s, 2s, 3s,...?
# A little ambiguous, but we will try the SARIMA model with Q=1:

# At the lower lags 1,2,3,... both the ACF and PACF are tailing off.
# We will first try an ARMA(p=1,q=1) for the nonseasonal piece:

sarima(ly, 1,1,1, 0,1,1, 12)
# specifies p=1,d=1,q=1, P=0,D=1,Q=1, s=12.

# The diagnostics look good, but the nonseasonal AR coefficient is nonsignificant.
# Let's try a couple of simpler models:

sarima(ly, 0,1,1, 0,1,1, 12)
# specifies p=0,d=1,q=1, P=0,D=1,Q=1, s=12.

sarima(ly, 1,1,0, 0,1,1, 12)
# specifies p=1,d=1,q=0, P=0,D=1,Q=1, s=12.

# The information criteria prefer the 
# ARIMA(0,1,1)x(0,1,1)_12 model (the "airline model").
# The diagnostics look good for this model as well, so this will be our final model.

### Forecasting with Seasonal Models:

# Forecasting with the co2 series:

library(TSA); data(co2)

# Exhibit 10.16
#win.graph(width=4.875, height=3,pointsize=8)
plot(m1.co2,n1=c(2003,1),n.ahead=24,col='red',xlab='Year',type='o',
ylab=expression(CO[2]~~Levels),
main=expression(Forecasts~~and~~Forecast~~Limits~~'for'~~the~~CO[2]~~Model))
# Note that for is a reserved word in R so it has to be enclosed in quotation marks.

# Exhibit 10.17
plot(m1.co2,n1=c(2004,1),n.ahead=48,col='red',xlab='Year',type='o',
ylab=expression(CO[2]~~Levels),
main=expression(Long~~Term~~Forecasts~~'for'~~the~~CO[2]~~Model))

# Showing the forecasts and standard errors:

predict(m1.co2, n.ahead=48)

# Forecasting with the airline passengers data:
# (this forecasts 24 time units (months, here) into the future):

sarima.for(ly, 24, 0,1,1,  0,1,1, 12)

# An example of a series that is differenced, but NOT seasonally differenced:

# Forecasting with the U.S. births data:

lbirth = log(birth)

plot.ts(cbind(birth,lbirth,diff(lbirth)), yax.flip=T, main="")

# Recall we considered a seasonal AR(P=1) or AR(P=2) model 
# for the differenced logged birth data:

acf2(diff(lbirth),36) 

# Probably P=2 or even P=3 is best, but note there are 
# substantial autocorrelations at the lower lags.
# may need a multiplicative model with nonzero p and/or q.

m.lb1=arima(lbirth,order=c(0,1,0),seasonal=list(order=c(1,0,0),period=12))
m.lb1

m.lb2=arima(lbirth,order=c(0,1,0),seasonal=list(order=c(2,0,0),period=12))
m.lb2

m.lb3=arima(lbirth,order=c(0,1,0),seasonal=list(order=c(3,0,0),period=12))
m.lb3

# The seasonal AR(P=3) model seems preferred.

# Trying a multiplicative model (AIC is even better, but more parameters to estimate):

m.lb3.mult=arima(lbirth,order=c(2,1,2),seasonal=list(order=c(3,0,0),period=12))
m.lb3.mult  

# Checking diagnostics for a couple of candidate models:

sarima(lbirth, 0,1,0,  3,0,0, 12)  # some problems with residuals' ACF

sarima(lbirth, 2,1,2,  3,0,0, 12)  # looks better

# Forecasting 24 months into the future:
sarima.for(lbirth, 24, 2,1,2,  3,0,0, 12)
