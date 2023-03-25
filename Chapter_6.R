# ######################################################################
# ########## R examples from Cryer and Chan - Chapter 6 ################
# ######################################################################

library(TSA)

# Exhibit 6.5
data(ma1.1.s)
#win.graph(width=4.875, height=3,pointsize=8)
acf(ma1.1.s,xaxp=c(0,20,10)) # if the argument xaxp is omitted, the
# tick marks will be generated according to the default convention. 
# Run the command ?par to learn more the xaxp argment.  
acf(ma1.1.s) # see the new tickmarks

# Note the acf values (mostly) cut off after lag 1 (do you see the exceptions to this?)

# So how was xaxp determined? First run the command without xaxp to find out
# how many lags are on the x-axis. Suppose there are 20 lags. Then we specify
# xaxp=c(0,20,10), i.e. the two extreme tickmarks are 0 and 20, and we want
# 10 tickmarks in between. How to set the xaxp argument if we want 1 tickmark for
# each lag?

pacf(ma1.1.s)

# Note the PACF values decay toward zero (roughly) rather than cut off.

# Exhibit 6.6
acf(ma1.1.s,ci.type='ma',xaxp=c(0,20,10))

# Exhibit 6.7
data(ma1.2.s)
acf(ma1.2.s,xaxp=c(0,20,10))

# Exhibit 6.8
data(ma2.s)
acf(ma2.s,xaxp=c(0,20,10))

# Exhibit 6.9
acf(ma2.s,ci.type='ma',xaxp=c(0,20,10))

# Exhibit 6.10
data(ar1.s)
acf(ar1.s,xaxp=c(0,20,10))


# Exhibit 6.11
pacf(ar1.s,xaxp=c(0,20,10))

# Exhibit 6.12
data(ar2.s)
acf(ar2.s,xaxp=c(0,20,10))

# Exhibit 6.13
pacf(ar2.s,xaxp=c(0,20,10))

# Exhibit 6.14
data(arma11.s)
plot(arma11.s, type='b',ylab=expression(y[t]))

# Exhibit 6.15
acf(arma11.s,xaxp=c(0,20,10))

# Exhibit 6.16
pacf(arma11.s,xaxp=c(0,20,10))

# Exhibit 6.17
eacf(arma11.s)

# If desirable,  a title can be added to the EACF table by the
# following command.

cat('Exhibit 6.17\n');eacf(arma11.s)

# Exhibit 6.18
data(oil.price)
acf(as.vector(oil.price),main='Sample ACF of the Oil Price Time Series',xaxp=c(0,24,12))
# The as.vector function strips the ts attaribute of oil.price in order to 
# prevent the plotting convention for seasonal time series.
# The main argument supplies the main heading of the figure.
# Try the following command to appreciate the effects of applying the as.vector
# function.

acf(oil.price,main='Sample ACF of the Oil Price Time Series')
# The tickmark 1 on the x-axis now refers to a lag of
# 1 period with each period containing 12 months, so it is lag 12!

# Exhibit 6.19
acf(diff(as.vector(log(oil.price))),main='Sample ACF of the Difference of the Oil Price Time Series',xaxp=c(0,24,12))







# ######################################################################
# ###### R examples from Shumway and Stoffer - Sample ACF and PACF #####
# ######################################################################



library(astsa)


set.seed(8675309)    
ar2 = arima.sim(list(order=c(2,0,0), ar=c(1.5,-.75)), n = 144)
plot(ar2, axes=FALSE, xlab="Time")
axis(2); axis(1, at=seq(0,144,by=12)); box()  # work the plot machine
abline(v=seq(0,144,by=12), lty=2)

# The 'acf2' function in the 'astsa' package plots both the ACF and PACF at once:
acf2(ar2)

ACF = ARMAacf(ar=c(1.5,-.75), ma=0, 50)
plot(ACF, type="h", xlab="lag")
abline(h=0)

## NOTE: The 'ARMAacf' function computes the THEORETICAL ACF (or PACF, if you use pacf=TRUE) for an ARMA-type model.
## The inputs for the 'ARMAacf' function are the true AR and/or MA coefficients of 
## the process, and the number of lags to plot.
## This is different from the 'acf' and 'pacf' functions, 
## which compute a SAMPLE ACF (or PACF) of an observed time series.

ar2.acf = ARMAacf(ar=c(1.5,-.75), ma=0, 24)[-1]
ar2.pacf = ARMAacf(ar=c(1.5,-.75), ma=0, 24, pacf=TRUE)
par(mfrow=c(1,2))
plot(ar2.acf, type="h", xlab="lag")
abline(h=0)
plot(ar2.pacf, type="h", xlab="lag")
abline(h=0)
par(mfrow=c(1,1))


# Exhibit 6.20
data(rwalk)
acf(diff(rwalk,difference=2),ci.type='ma',xaxp=c(0,18,9))

# Exhibit 6.21
acf(diff(rwalk),ci.type='ma',xaxp=c(0,18,9))

#### Augmented Dickey-Fuller Testing:

# Carry out the Dickey-Fuller unit root tests
# Find out the AR order for the differenced series:

ar(diff(rwalk))

# order 8 is indicated by AIC


library(fUnitRoots)  # may need to install package 'fUnitRoots'

# Testing for difference nonstationarity:

# The type='c' assumes just a constant (intercept), not a linear trend:

adfTest(rwalk,lags=8,type='c')  # The AIC indicates the correct order is k=8

# In comparison, setting the true order k to be 0:

adfTest(rwalk,lags=0,type='c')  
# The truth for this example is that k=0:
# Since Y_t is a random walk, the first differences are white noise, i.e., AR(0).

# Testing for trend nonstationarity:

# Repeat the test with the alternative have an intercept term and a linear trend

adfTest(rwalk,lags=8,type='ct')  # Based on the order 8 that the AIC indicates

adfTest(rwalk,lags=0,type='ct')  # Based on the order 0 that is the truth, but in reality would be unknown


# Another function to test for trend nonstationarity:

library(tseries) # may need to install package 'tseries'

adf.test(rwalk,k=8,alternative="stationary")  # Based on the order 8 that the AIC indicates

adf.test(rwalk,k=0,alternative="stationary")  # Based on the order 0 that is the truth, but in reality would be unknown

# Augmented Dickey-Fuller testing on some real data sets:

data(larain); plot(larain,type='o')

ar(diff(larain))

adfTest(larain,lags=4,type='c')

# reject H0:  conclude the LA rain series is stationary.

data(oil.price); plot(log(oil.price),ylab="Logged Oil prices",xlab="Year",type="o")

ar(diff(log(oil.price)))

adfTest(log(oil.price),lags=2,type='c')

# Fail to reject H0:  Conclude the logged oil price data is difference nonstationary.

library(astsa); data(jj); plot(log(jj), type='o')

ar(diff(log(jj)))

adfTest(log(jj),lags=5,type='ct')

# Fail to reject H0:  Conclude the logged Johnson & Johnson data is trend nonstationary.

# For the last example, note that:
# library(tseries); adf.test(log(jj),k=5,alternative="stationary") 
# does the same thing.


#############################

# Best subset ARMA model selection using BIC:

# Exhibit 6.22 
set.seed(92397)
test=arima.sim(model=list(ar=c(rep(0,11),.8),ma=c(rep(0,11),0.7)),n=120)
res=armasubsets(y=test,nar=14,nma=14,y.name='test',ar.method='ols')
plot(res)

# A title may be added to the plot, by adding the main="..." option
# in the above plot function. However, the title may crash with other
# labels. A better approach is to add a title by the 
# following command.

title(main="Exhibit 6.22", line=6)

# The option line=6 means put the title 6 lines from the edge of the
# plot region. You can experiment with this option to fine tune the
# placement of the label.

# Los Angeles Rain example
# Exhibit 6.23
data(larain)
#win.graph(width=3, height=3,pointsize=8)
qqnorm(log(larain))
qqline(log(larain))

# Exhibit 6.24
#win.graph(width=4.875, height=3,pointsize=8)
acf(log(larain),xaxp=c(0,20,10)) # note the main heading now includes the data name.

# Exhibit 6.25
data(color)
acf(color,ci.type='ma')

# Exhibit 6.26
pacf(color)

# Exhibit 6.27  
#win.graph(width=4, height=4,pointsize=8)
data(hare)
bxh=BoxCox.ar(hare)
bxh$mle # the mle of the power parameter
bxh$ci # corresponding 95% C.I.

# Exhibit 6.28
acf(hare^.5)

# Exhibit 6.29
pacf(hare^.5)

# Augmented Dickey-Fuller test for stationarity in the logged oil price data was done previously.
# the conclusion was that the logged oil price data is difference nonstationary.

# Exhibit 6.30
eacf(diff(log(oil.price)))

# Exhibit 6.31
res=armasubsets(y=diff(log(oil.price)),nar=7,nma=7,
y.name='test', ar.method='ols')
plot(res)

# Exhibit 6.32
acf(as.vector(diff(log(oil.price))),xaxp=c(0,22,11))

# Exhibit 6.33
pacf(as.vector(diff(log(oil.price))),xaxp=c(0,22,11))


# Shumway-Stoffer examples with real data:
# Analysis of recruitment data, indicates AR(2) model?

acf2(rec, 48)     # will produce values and a graphic 

# Analysis of SOI data:

acf2(soi, 48)     # doesn't match a stationary ARMA model...

acf2(diff(soi), 48) # first differences are not looking good either...

acf2(diff(soi, differences=2), 48) # Series of second differences looks like MA(1).

# Analysis of logged earnings data:

acf2(log(jj),40)

# The time series is not stationary, so we could try analyzing the differences:

acf2(diff(log(jj)),40)

# Analysis of airmiles data:

library(TSA)

data(airmiles)
acf2(airmiles)

eacf(airmiles)

res=armasubsets(y=airmiles,nar=14,nma=14,
y.name='test', ar.method='ols')
plot(res)

# In the ARMA(14,14) model, which subset of terms has nonzero coefficients in the model considered "best" by BIC?





# Estimation of recruitment data, assuming AR(2) model:
#(regr = ar.ols(rec, order=2, demean=F, intercept=TRUE))  # regression
#regr$asy.se.coef  # standard errors     
