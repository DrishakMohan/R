# ######################################################################
# ########## R examples from Cryer and Chan - Chapter 9 ################
# ######################################################################

library(TSA)

## Forecasting with simple trend models:

# Linear trend for the logged earnings data
library(astsa)
modeljj=lm(log(jj)~time(jj))
summary(modeljj)

# predicting for Qtr 3, 1982:
future.time=1982.5
y.hat.pred = as.numeric(coef(modeljj) %*% c(1,future.time) ); print(y.hat.pred) # predicted logged earnings for Qtr 3, 1982
exp(y.hat.pred) # predicted earnings for Qtr 3, 1982

# Harmonic regression for Dubuque temperature data:
data(tempdub)
har.=harmonic(tempdub,m=1) 
model4=lm(tempdub~har.)
summary(model4)

# predicting for June 1976:
future.time = 1976.41667
y.hat.pred = as.numeric(coef(model4) %*% c(1,cos(2*pi*future.time),sin(2*pi*future.time)) ); 
print(y.hat.pred)

### Forecasting with ARIMA-type models

# Exhibit 9.1
data(color)
m1.color=arima(color,order=c(1,0,0))
m1.color 

# The MLE's:

phi.hat = coef(m1.color)["ar1"]
mu.hat = coef(m1.color)["intercept"]

# The most recent observed data value:

Y.t = color[length(color)]

# Forecasting "l" time units ahead:
# can change to l=2, l=3, l=20, etc.:
l=1; 
Y.t.l.hat = mu.hat + (phi.hat^l)*(Y.t - mu.hat); 
print(Y.t.l.hat)

# Prediction Intervals in the AR(1) model:

# Prediction limits "l" time units ahead:
# can change to l=2, l=3, l=20, etc.:

l=1; 
Y.t.l.hat = mu.hat + (phi.hat^l)*(Y.t - mu.hat); 

sigma.sq.e.hat = m1.color$sigma

alpha=0.05

P.L=Y.t.l.hat - qnorm(1-alpha/2)*sqrt( (sigma.sq.e.hat)*( (1-phi.hat^(2*l)) / (1-phi.hat^2) ) )
P.U=Y.t.l.hat + qnorm(1-alpha/2)*sqrt( (sigma.sq.e.hat)*( (1-phi.hat^(2*l)) / (1-phi.hat^2) ) )

# Printing the prediction interval:
print(paste(100*(1-alpha), "% PI for observation", l, "time units into future is:", signif(P.L,4), signif(P.U, 4) ))


# Exhibit 9.2

data(tempdub)

# append 2 years of missing values to the tempdub data as we want to forecast
# the temperature for two years. (this step may not be necessary...)

tempdub1=ts(c(tempdub,rep(NA,24)),start=start(tempdub),freq=frequency(tempdub)) 

# creates the first pair of harmonic functions and then fit the model
har.=harmonic(tempdub,1)
m5.tempdub=arima(tempdub,order=c(0,0,0),xreg=har.)
m5.tempdub
# The result is same as that from the fit using lm function.
har.=harmonic(tempdub,1)
model4=lm(tempdub~har.)
summary(model4)


# create the harmonic functions over the period of forecast.
newhar.=harmonic(ts(rep(1,24), start=c(1976,1),freq=12),1)
# Compute and plot the forecasts.
#win.graph(width=4.875, height=3,pointsize=8)
plot(m5.tempdub,n.ahead=24,n1=c(1972,1),newxreg=newhar.,
 type='b',ylab='Temperature',xlab='Year')

# Exhibit 9.3 

# Predicting future values of the color property data based on an AR(1) model:
# This goes 12 time units into the future:

data(color)
m1.color=arima(color,order=c(1,0,0))
plot(m1.color,n.ahead=12,type='b', xlab='Time', ylab='Color Property')
# add the horizontal line at the estimated mean ("intercept") 
abline(h=coef(m1.color)[names(coef(m1.color))=='intercept'])

# Showing the forecasted values and the standard errors of the forecasts:

predict(m1.color, n.ahead=12)

## Can see 95% prediction intervals for the forecast at any particular lead time:

# For one time unit ahead:

color.pred <- predict(m1.color, n.ahead=12)

c(color.pred$'pred'[1]-1.96*color.pred$se[1], color.pred$'pred'[1]+1.96*color.pred$se[1] )

# For five time units ahead:

c(color.pred$'pred'[5]-1.96*color.pred$se[5], color.pred$'pred'[5]+1.96*color.pred$se[5] )



# Exhibit 9.4

# Predicting future values of the (square-root transformed) hare data based on an AR(3) model:
# This goes 25 time units (years, here) into the future:

data(hare)
m1.hare=arima(sqrt(hare),order=c(3,0,0))
plot(m1.hare, n.ahead=25,type='b',xlab='Year',ylab='Sqrt(hare)')
abline(h=coef(m1.hare)[names(coef(m1.hare))=='intercept'])

# Showing the forecasted values and the standard errors of the forecasts:

predict(m1.hare, n.ahead=25)

# This goes 100 time units (years, here) into the future:

data(hare)
m1.hare=arima(sqrt(hare),order=c(3,0,0))
plot(m1.hare, n.ahead=100,type='b',xlab='Year',ylab='Sqrt(hare)')
abline(h=coef(m1.hare)[names(coef(m1.hare))=='intercept'])









# ######################################################################
# ##########    R examples from Shumway and Stoffer          ###########
# ######################################################################

library(astsa)

# The 'sarima' and 'sarima.for' functions can be used for
# estimating ARIMA-type models and forecasting with them:

# An AR(2) model for the recruitment data:

acf2(rec)  # Clearly an AR(2) model is reasonable.

sarima(rec,2,0,0)

# This goes 24 time units (months, here) into the future:
sarima.for(rec,24,2,0,0)
# Optionally add a horizonal line at the estimated mean:
# abline(h=61.8585, lty=3, col='green')

# Note that 'sarima' prints the forecasts to the console, as well as providing a plot
# of the forecasts, plus or minus 2 standard errors (pointwise 95% prediction limits)

#############################
##
## Forecasting using a model for DIFFERENCED data:  Check whether the constant term is needed!

library(TSA)

library(astsa)

# We have seen in chapter 7 that the differenced logged GNP series follows either an MA(2) or an AR(1) model.

# Fitting an ARIMA(0,1,2) model (i.e., an IMA(1,2) model) to the logged GNP series with the 'arima' function, 
# it assumes the differenced series has mean zero and cannot include a mean or constant term in the model:

arima(log(gnp), order=c(0,1,2))

# The 'sarima' function in the 'astsa' package CAN assume a nonzero constant term for the differenced series:
sarima(log(gnp), 0,1,2)

#...or you can tell 'sarima' not to include the constant:
sarima(log(gnp), 0,1,2, no.constant=T)

# In this case the constant term appears significantly different from zero.
# Note that including the constant term changes the forecasts substantially!

sarima.for(log(gnp), 40, 0,1,2)
x11()
sarima.for(log(gnp), 40, 0,1,2, no.constant=T)

## LESSON:  When forecasting with a NONSTATIONARY series, use 'sarima' rather than 'arima' to fit the model, 
## and check whether the constant term is needed.
##
#############################

# Another real data example:
# An IMA(1,1) model for the logged oil price time series:

data(oil.price)
sarima(log(oil.price),0,1,1)

# In this logged oil price example, we could optionally remove the constant term by using no.constant=T
# since the constant is not significantly different from zero.
# Removing the constant would change the forecasts SLIGHTLY here.

# I'll go ahead and show the forecasts based on the model WITH the constant term:

sarima.for(log(oil.price),35,0,1,1)


## A DETAILED EXAMPLE OF FORECASTING WITH A NONSTATIONARY TIME SERIES:

library (TSA)
library (astsa)

# Two approaches to forecasting the chicken price series, a nonstationary process:

data(chicken)
plot(chicken,type='o') # clearly nonstationary

## APPROACH 1: Fitting a linear trend model and detrending:

lin.mod.chick <- lm(chicken~time(chicken))
summary(lin.mod.chick)
detrend.chick <- resid(lin.mod.chick)  # getting the detrended series

# Does the detrended series look like white noise?
plot(detrend.chick,type='o') # Plot of detrended series
acf2(detrend.chick)

# Detrended data don't look like white noise; maybe AR(2) if it's stationary at all.

# Recall the times when the chicken price series was measured:

time(chicken)

# Let's predict the value of the residual series for October 2016 (3 time units into the future):

# Fit an AR(2) model to the detrended series:

# Does the detrended series have mean zero?  Let's try it both ways
arima(detrend.chick, order=c(2,0,0))  #includes the process mean mu to be estimated
arima(detrend.chick, order=c(2,0,0), include.mean=F)  #assumes mu=0

# Looks like we can assume the mean of the detrended series is zero.
detrend.chick.ar2<- arima(detrend.chick, order=c(2,0,0), include.mean=F)

detrend.chick.pred <- predict(detrend.chick.ar2, n.ahead=12) # Forecasting the detrended series for the next 12 months


# Adding the predicted trend model value in October 2016 to get a forecast for the chicken price in October 2016:
future.time=2016.75
y.hat.pred = as.numeric(coef(lin.mod.chick) %*% c(1,future.time) )

# Point prediction of the chicken series for October 2016:
y.hat.pred + detrend.chick.pred$'pred'[3]
# 95% prediction interval for the forecast of the chicken series for October 2016:
y.hat.pred + c(detrend.chick.pred$'pred'[3]-1.96*detrend.chick.pred$se[3], detrend.chick.pred$'pred'[3]+1.96*detrend.chick.pred$se[3])


## APPROACH 2 (Simpler approach):  Work with an ARIMA model for the differenced data.

plot(diff(chicken), type ='o') # The series of first differences looks stationary
acf2(diff(chicken))

# Some evidence that the differenced series follows an AR(2) model, but there may be some subtle seasonality.

# Let's fit an ARIMA(2,1,0) model to the original chicken series and forecast the chicken price for October 2016:

sarima(chicken, 2,1,0) 
# Using the 'sarima' function since d>0 and I may want to include a constant term in the model.

sarima.for(chicken, 12, 2,1,0)

diff.chick.pred <- sarima.for(chicken, 12, 2,1,0)

diff.chick.pred$pred[3]
# 95% prediction interval for the forecast of the chicken series for October 2016:
c(diff.chick.pred$pred[3]-1.96*diff.chick.pred$se[3], diff.chick.pred$pred[3]+1.96*diff.chick.pred$se[3])


