# ######################################################################
# ########## R examples from Cryer and Shan - Chapter 1 ################
# ######################################################################

# loading the 'TSA' package.
# You may need to install the 'TSA' package first.
# If so, type :  install.packages('TSA')


library(TSA)

# annual LA rainfall time series:
#win.graph(width=4.875, height=2.5,pointsize=8)
data(larain); plot(larain,ylab='Inches',xlab='Year',type='o')

# Are there any notable trends over time?

# scatter plot of each observation against previous observation:
#win.graph(width=3, height=3,pointsize=8)
plot(y=larain,x=zlag(larain),ylab='Inches',xlab='Previous Year Inches')

# Does the previous year's rainfall give us much information about this year's rainfall?
# How would we answer this question based on this plot?

# color value time series:
#win.graph(width=4.875, height=2.5,pointsize=8)
data(color); plot(color,ylab='Color Property',xlab='Batch',type='o')

# Are there any trends or patterns of interest in this time series?

# scatter plot of each observation against previous observation:
#win.graph(width=3, height=3,pointsize=8)
plot(y=color,x=zlag(color),ylab='Color Property',
     xlab='Previous Batch Color Property')

# Does the previous color value tell us anything about the current color value?

# Canadian hare abundance time series:
#win.graph(width=4.875, height=2.5,pointsize=8)
data(hare); plot(hare,ylab='Abundance',xlab='Year',type='o')

# What is most apparent in this time series?

# scatter plot of each observation against previous observation:
#win.graph(width=3, height=3,pointsize=8)
plot(y=hare,x=zlag(hare),ylab='Abundance',xlab='Previous Year Abundance')

# Does the previous year's abundance give us much information about this year's abundance?

# Monthly Dubuque temperature time series:
#win.graph(width=4.875, height=2.5,pointsize=8)
data(tempdub); plot(tempdub,ylab='Temperature',type='o')

# What is the clearly evident pattern in this time series?

# Monthly oil filter sales time series:
data(oilfilters)
plot(oilfilters,type='o',ylab='Sales')

# Explicitly labeled plotting symbols can be useful when plotting monthly data:

# Monthly oil filter sales time series (with plotting symbols):
plot(oilfilters,type='l',ylab='Sales')
Month=c("J","A","S","O","N","D","J","F","M","A","M","J")
points(oilfilters,pch=Month)

# Alternative commands:
plot(oilfilters,type='l',ylab='Sales')
points(y=oilfilters,x=time(oilfilters),pch=as.vector(season(oilfilters)))





# ######################################################################
# ########## R examples from Shumway and Stoffer - Chapter 1 ###########
# ######################################################################

# loading the 'astsa' package.
# You may need to install the 'astsa' package first.
# If so, type :  install.packages('astsa')
# and choose a USA mirror site.

library(astsa)

plot(jj, type="o", ylab="Quarterly Earnings per Share")

# We see an increasing nonlinear trend, with variance increasing over time.

# What would a log-transformed time series look like?

plot(gtemp, type="o", ylab="Global Temperature Deviations")

# We see the clear basic trend, but is there any other pattern in the series?

# SOI and Recruitment data
par(mfrow = c(2,1))  # set up the graphics in 2-by-1 plotting window
plot(soi, ylab="", main="Southern Oscillation Index")
plot(rec, ylab="", main="Recruitment") 

# Several FMRI time series
par(mfrow=c(2,1), mar=c(3,2,1,0)+.5, mgp=c(1.6,.6,0))  
ts.plot(fmri1[,2:5], col=1:4, ylab="BOLD", xlab="", main="Cortex")
ts.plot(fmri1[,6:9], col=1:4, ylab="BOLD", xlab="", main="Thalamus & Cerebellum")
mtext("Time (1 pt = 2 sec)", side=1, line=2) 


# Some other examples of time series (from built-in R data sets):


par(mfrow = c(1,1)) # reset to 1-by-1 plotting window

data(milk)
plot(milk,ylab="Amount of milk produced",xlab="Year",type="o")

data(CREF)
plot(CREF,ylab="CREF stock values",type="o")

data(star)
plot(star,ylab="Star brightness",type="o")

data(oil.price)
plot(oil.price,ylab="Oil prices",xlab="Year",type="o")

data(airmiles)
plot(airmiles,ylab="Airline miles",xlab="Year",type="o")


data(airmiles)
plot(airmiles,ylab="Airline miles",xlab="Year",type='l')
points(y=airmiles,x=time(airmiles),pch=as.vector(season(airmiles)),cex=1)

# ######################################################################
# ########## R examples from Cryer and Chan - Chapter 2 ################
# ######################################################################

library(TSA)

# Exhibit 2.1
#win.graph(width=4.875, height=2.5,pointsize=8)
# rwalk contains a simulated random walk
data(rwalk)
plot(rwalk,type='o',ylab='Random Walk')

# R code for simulating a random walk with, say 60, iid standard normal errors
n=60
set.seed(12345) # initialize the random number so that the simulation can be reproducible.
sim.random.walk=ts(cumsum(rnorm(n)),freq=1,start=1)
plot(sim.random.walk,type='o',ylab='Another Random Walk')

# A lot of random walk realizations, plotted on the same plot:

n=60
sim.random.walk.lots<-matrix(0,nrow=25,ncol=60)
for (i in 1:25)
{
  sim.random.walk.lots[i,]=ts(cumsum(rnorm(n)),freq=1,start=1)
}
plot(sim.random.walk.lots[1,],type='l',ylim=c(-15,15), ylab='Simulated Random Walk Processes')
for (i in 2:25)
{
  lines(sim.random.walk.lots[i,])
}

# # Examples with moving averages:

# A simulated moving average process like the example in class:

n=60
wn.random <- rnorm(n)
sim.ma <- ts((wn.random+zlag(wn.random))/2)
plot(sim.ma)

# A lot of moving average realizations, plotted on the same plot:

n=60
sim.ma.lots<-matrix(0,nrow=25,ncol=60)
for (i in 1:25)
{
  wn.random <- rnorm(n)
  sim.ma.lots[i,]=ts((wn.random+zlag(wn.random))/2)
}
plot(sim.ma.lots[1,],type='l',ylim=c(-3,3), ylab='Simulated Moving Average Processes')
for (i in 2:25)
{
  lines(sim.ma.lots[i,])
}

# ######################################################################
# ########## R examples from Cryer and Chan - Chapter 3 ################
# ######################################################################

library(TSA)

# Exhibit 3.1
# time(rwalk) yields a time series of the time epoches when the random walk was sampled.
data(rwalk)
model1=lm(rwalk~time(rwalk))
summary(model1)

# Exhibit 3.2
#win.graph(width=4.875, height=2.5,pointsize=8)
# rwalk contains a simulated random walk
plot(rwalk,type='o',ylab='y')
abline(model1) # add the fitted least squares line

# Quadratic fit:
# time(rwalk) yields a time series of the time epoches when the random walk was sampled.
data(rwalk)
timesq = time(rwalk)^2
model1q=lm(rwalk~time(rwalk)+timesq)
summary(model1q)
plot(rwalk,type='o',ylab='y')
lines(as.vector(time(rwalk)), fitted(model1q)) 
# adds the fitted least squares quadratic curve (note 'abline' only works on lines)

# Linear trend for the logged earnings data
library(astsa)
modeljj=lm(log(jj)~time(jj))
summary(modeljj)
plot(log(jj),type='o',ylab='y')
abline(modeljj) # add the fitted least squares line

# Exhibit 3.3
# season(tempdub) creates a vector of the month index of the data as a factor 
data(tempdub)
month.=season(tempdub) 
# the period sign is included to make the printout from
# the commands two line below clearer; ditto below.
model2=lm(tempdub~month.-1) # -1 removes the intercept term 
summary(model2)

# Exhibit 3.4
model3=lm(tempdub~month.) # intercept is automatically included so one month (Jan) is dropped
summary(model3)

# Exhibit 3.5
# first creates the first pair of harmonic functions and then fit the model
har.=harmonic(tempdub,m=1) 
# The m=1 creates one pair of harmonic functions (one cosine term and one sine term).
# If you want additional pairs, you could increase m.
model4=lm(tempdub~har.)
summary(model4)

# Exhibit 3.6
#win.graph(width=4.875, height=2.5,pointsize=8)
plot(ts(fitted(model4),freq=12,start=c(1964,1)),ylab='Temperature',type='l',
     ylim=range(c(fitted(model4),tempdub))) # the ylim option ensures that the 
# y axis has a range that fits the raw data and the fitted values
points(tempdub)

# Exhibit 3.7
data(rwalk)
model1=lm(rwalk~time(rwalk))
summary(model1)

# Calculating the AIC and BIC of competing candidate models:

AIC(model1)
AIC(model1q)

BIC(model1)
BIC(model1q)

# Exhibit 3.8
plot(y=rstudent(model3),x=as.vector(time(tempdub)),xlab='Time',
     ylab='Standardized Residuals',type='o')

# Exhibit 3.9
plot(y=rstudent(model3),x=as.vector(time(tempdub)),xlab='Time',
     ylab='Standardized Residuals',type='l')
points(y=rstudent(model3),x=as.vector(time(tempdub)),
       pch=as.vector(season(tempdub)))

# Exhibit 3.10
plot(y=rstudent(model3),x=as.vector(fitted(model3)),xlab='Fitted Trend Values',
     ylab='Standardized Residuals',type="n")
points(y=rstudent(model3),x=as.vector(fitted(model3)),
       pch=as.vector(season(tempdub)))

# Exhibit 3.11
hist(rstudent(model3),xlab='Standardized Residuals',main='')

# Exhibit 3.12
#win.graph(width=3, height=3,pointsize=8)
qqnorm(rstudent(model3),main='')

# Shapiro-Wilk test:
shapiro.test(rstudent(model3))

# Runs test:

runs(rstudent(model3))

# Exhibit 3.13
#win.graph(width=4.875, height=3,pointsize=8)
acf(rstudent(model3),main='')

# Exhibit 3.14
plot(y=rstudent(model1),x=as.vector(time(rwalk)),ylab='Standardized Residuals',
     xlab='Time',type='o')

# Exhibit 3.15
#win.graph(width=4.875, height=3,pointsize=8)
plot(y=rstudent(model1),x=fitted(model1),ylab='Standardized Residuals',
     xlab='Fitted Trend Values',type='p')

# Exhibit 3.16
acf(rstudent(model1),main='')

# Runs test on model1 residuals:

runs(rstudent(model1))



# ######################################################################
# ########## R examples from Shumway and Stoffer - Chapter 2 ###########
# ######################################################################

# loading the 'astsa' package.
# You may need to install the 'astsa' package first.
# If so, type :  install.packages('astsa')
# and choose a USA mirror site.
# To get the latest version of 'astsa':
#            1- install the 'devtools' package
#            2- devtools::install_github("nickpoison/astsa")


library(astsa)




# regression of chicken price on time
# and plotting data with fitted regression line

summary(fit <- lm(chicken~time(chicken)))
ts.plot(chicken, ylab="cents per pound")
abline(fit)

# Some residual analysis:

# Plot of residuals across time:
plot(y=rstudent(fit),x=as.vector(time(chicken)),xlab='Time', ylab='Standardized Residuals',type='o')
# ACF of residuals:
acf(rstudent(fit),main='')
# Runs test on residuals:
runs(rstudent(fit))

par(mfrow=c(3,1)) # plot the data
ts.plot(cmort, main="Cardiovascular Mortality", ylab="")
ts.plot(tempr, main="Temperature", ylab="")
ts.plot(part, main="Particulates", ylab="")
dev.new() # open a new graphic device
ts.plot(cmort,tempr,part, col=1:3) # all on same plot (only useful if the units of measurement are analogous)
legend('topright', legend=c('Mortality', 'Temperature', 'Pollution'),
       lty=1, col=1:3)
dev.new()
pairs(cbind(Mortality=cmort, Temperature=tempr, Particulates=part))
temp = tempr-mean(tempr) # center temperature
temp2 = temp^2
trend = time(cmort) # time
fit = lm(cmort~ trend + temp + temp2 + part, na.action=NULL)
summary(fit) # regression results
summary(aov(fit)) # ANOVA table (compare to next line)
summary(aov(lm(cmort~cbind(trend, temp, temp2, part)))) # Table 2.1
num = length(cmort) # sample size
AIC(fit) # AIC
BIC(fit) # BIC

# Comparing models based on AIC:
fit.linear = lm(cmort~ trend + temp + part, na.action=NULL)
AIC(fit.linear)

fit.simpler = lm(cmort~ trend + temp, na.action=NULL)
AIC(fit.simpler)




# Regression with lagged variables

fish = ts.intersect(rec, soiL6=lag(soi,-6))
summary(fit1 <- lm(rec~soiL6,data=fish,na.action=NULL))

library(dynlm)  # may need to install 'dynlm' package first?
summary(fit2 <- dynlm(rec~L(soi,6)))

# Chicken data, detrending and differencing:

fit = lm(chicken~time(chicken), na.action=NULL)
par(mfrow=c(2,1))
ts.plot(resid(fit), main="detrended")
ts.plot(diff(chicken), main="first difference")
par(mfrow=c(3,1))
acf(chicken, 48, main="Chicken ACF")
acf(resid(fit), 48, main="Detrended, ACF")
acf(diff(chicken), 48, main="First Differences, ACF")

# We see a one-year cycle in the differenced data!

# Global temperatures data:

par(mfrow=c(2,1))
ts.plot(diff(gtemp), type='o')
mean(diff(gtemp))
acf(diff(gtemp), 48) 

# A log transformation:

par(mfrow=c(2,1))
ts.plot(varve, main="varve", ylab="")
ts.plot(log(varve), main="log(varve)", ylab="" )

# Does the log transformation improve the nonstationarity evident in the variance over time?

# Lagged Scatterplot matrices


lag1.plot(soi, 12) # Figure 2.8
lag2.plot(soi, rec, 8) # Figure 2.9

# A regression of recruitment on lag-6 SOI:

attach(data.frame(fish)) # so we can use the names of the variables in fish
plot(soiL6, rec)
lines(lowess(soiL6, rec), col=4, lwd=2)  # lowess will model a nonlinear trend (see below)
points(soiL6, fitted(fit1), pch='+', col=2)
ts.plot(resid(fit1)) # 
acf(resid(fit1)) # ... the residuals are obviously not white noise

# Moving average:

wgts = c(0.5, rep(1,11), 0.5)/12
soif = filter(soi, sides=2, filter=wgts)
ts.plot(soi)
lines(soif, lwd=2, col=4)

# Kernel smoother:

ts.plot(soi)
lines(ksmooth(time(soi), soi, "normal", bandwidth = 1), lwd=2, col=4)
lines(ksmooth(time(soi), soi, "normal", bandwidth = 12), lwd=2, col=3)

# We see the effect of changing the bandwidth.

# Lowess smoothing:

ts.plot(soi)
lines(lowess(soi, f=.05), lwd=2, col=4) #El Nino cycle
lines(lowess(soi), lwd=2, col=4)  # an estimate of the overall trend (this uses the default span of f=2/3)

# Smoothing One Series as a Function of Another:

plot(tempr, cmort, xlab="temperature", ylab="mortality")
lines(lowess(tempr,cmort))

#Classical Structural Modeling

x=window(hor, start=2002) # Hawaii hotel data set in astsa package
plot(decompose(x))
plot(stl(x, s.window='per'))
plot(stl(x, s.window=15))



# ######################################################################
# ########## R examples from Cryer and Chan - Chapter 4 ################
# ######################################################################


library(TSA)

# Exhibit 4.2
#win.graph(width=4.875, height=3,pointsize=8)
data(ma1.2.s)   # This is a simulated MA(1) series with coefficient equal to -0.9
plot(ma1.2.s,ylab=expression(Y[t]),type='o')

# Another MA(1) series with MA coefficient equal to -0.9 and 
# of length n=100 can be simulated by the following command
set.seed(12345) # initialize the seed of the random number generator so that
# the simulations can be reproduced.
y=arima.sim(model=list(ma=-c(-0.9)),n=100)
# Note that R uses the plus convention in the model formula so the 
# additional minus sign.  

# Exhibit 4.3
#win.graph(width=3, height=3,pointsize=8)
plot(y=ma1.2.s,x=zlag(ma1.2.s),ylab=expression(Y[t]),xlab=expression(Y[t-1]),type='p')

# Exhibit 4.4
plot(y=ma1.2.s,x=zlag(ma1.2.s,2),ylab=expression(Y[t]),xlab=expression(Y[t-2]),type='p')


# Exhibit 4.5
#win.graph(width=4.875, height=3,pointsize=8)
data(ma1.1.s)  # This is a simulated MA(1) series with coefficient equal to 0.9
plot(ma1.1.s,ylab=expression(Y[t]),type='o')

# Another MA(1) series with ma coefficient equal to 0.9 and 
# of length n=100 can be simulated by the following command
y=arima.sim(model=list(MA=-c(0.9)),n=100)
# Note that R uses the plus convention in the MA model formula so the 
# additional minus sign.  

# Plots to show lag-1 and lag-2 associations for this simulated MA(1) series:

# Exhibit 4.6
#win.graph(width=3, height=3,pointsize=8)
plot(y=ma1.1.s,x=zlag(ma1.1.s),ylab=expression(Y[t]),xlab=expression(Y[t-1]),type='p')

# Exhibit 4.7
plot(y=ma1.1.s,x=zlag(ma1.1.s,2),ylab=expression(Y[t]),xlab=expression(Y[t-2]),type='p')

# Exhibit 4.8
#win.graph(width=4.875, height=3,pointsize=8)
data(ma2.s)   # This is a simulated MA(2) series with coefficients equal to 1 and -0.6
plot(ma2.s,ylab=expression(Y[t]),type='o')

# Another MA(2) series with MA coefficients equal to 1 and -0.6 and 
# of length n=100 can be simulated by the following command
y=arima.sim(model=list(ma=-c(1, -0.6)),n=100)
# Note that R uses the plus convention in the MA model formula so the 
# additional minus sign.  

# Plots to show lag-1, lag-2, and lag-3 associations for this simulated MA(2) series:

# Exhibit 4.9
#win.graph(width=3, height=3,pointsize=8)
plot(y=ma2.s,x=zlag(ma2.s),ylab=expression(Y[t]),xlab=expression(Y[t-1]),type='p')

# Exhibit 4.10
plot(y=ma2.s,x=zlag(ma2.s,2),ylab=expression(Y[t]),xlab=expression(Y[t-2]),type='p')

# Exhibit 4.11
plot(y=ma2.s,x=zlag(ma2.s,3),ylab=expression(Y[t]),xlab=expression(Y[t-3]),type='p')

# Exhibit 4.13
#win.graph(width=4.875, height=3,pointsize=8)
data(ar1.s)  # A simulated AR(1) series with AR coefficient 0.9
plot(ar1.s,ylab=expression(Y[t]),type='o')

# An AR(1) series with AR coefficient equal to 0.9 and 
# of length n=100 can be simulated by the following command
y=arima.sim(model=list(ar=c(0.9)),n=100)
# Note that the R convention for the AR model formula is same as the book, so  
# NO additional minus sign.  

# Plots to show lag-1, lag-2, and lag-3 associations for this simulated AR(1) series:

# Exhibit 4.14
#win.graph(width=3, height=3,pointsize=8)
plot(y=ar1.s,x=zlag(ar1.s),ylab=expression(Y[t]),xlab=expression(Y[t-1]),type='p')

# Exhibit 4.15
plot(y=ar1.s,x=zlag(ar1.s,2),ylab=expression(Y[t]),xlab=expression(Y[t-2]),type='p')

# Exhibit 4.16
plot(y=ar1.s,x=zlag(ar1.s,3),ylab=expression(Y[t]),xlab=expression(Y[t-3]),type='p')

# Exhibit 4.19
#win.graph(width=4.875, height=3,pointsize=8)
data(ar2.s)  # A simulated AR(2) series with AR coefficients 1.5 and -0.75
plot(ar2.s,ylab=expression(Y[t]),type='o')

# The ACF plot of the simulated AR(2) series:
acf(ar2.s) 

# Plots to show lag-1, lag-2, and lag-3 associations for this simulated AR(2) series:

plot(y=ar2.s,x=zlag(ar2.s),ylab=expression(Y[t]),xlab=expression(Y[t-1]),type='p')
plot(y=ar2.s,x=zlag(ar2.s,2),ylab=expression(Y[t]),xlab=expression(Y[t-2]),type='p')
plot(y=ar2.s,x=zlag(ar2.s,3),ylab=expression(Y[t]),xlab=expression(Y[t-3]),type='p')


# An AR(2) series with AR coefficients equal to 1.5 and -0.75 and 
# of length n=100 can be simulated by the following command
y=arima.sim(model=list(ar=c(1.5,-0.75)),n=100)
# Note that the R convention for the AR model formula is same as the book, so  
# NO additional minus sign. 





# ######################################################################
# ########## R examples from Cryer and Chan - Chapter 5 ################
# ######################################################################
library(TSA)

# Exhibit 5.1
#win.graph(width=4.875, height=3,pointsize=8)
data(oil.price)
plot(oil.price, ylab='Price per Barrel',type='l')

# Exhibit 5.3
data(explode.s)
plot(explode.s,ylab=expression(y[t]),type='o')

# Exhibit 5.4
plot(diff(log(oil.price)),ylab='Change in Log(Price)',type='l')

# Exhibit 5.5
data(ima22.s)
plot(ima22.s,ylab="IMA(2,2) Simulation",type='o')

# Note the variance of Y_t seems to get larger as time goes on.
# And note the apparent strong positive autocorrelation, especially for small lags.

# Exhibit 5.6
plot(diff(ima22.s),ylab='First Difference',type='o')

# Still looks nonstationary ...

# Exhibit 5.7
plot(diff(ima22.s,difference=2),ylab='Differenced Twice',type='o')

# Now, THAT looks stationary!

# Note that plot(diff(ima22.s,2),ylab='Differenced Twice',type='o') will
# draw a wrong figure because the second argument is the lag not the times of
# differencing. That is, diff(ima22.s,2) is the series of ima22.s(t)-ima22.s(t-2).

# The ACF plot of the twice-differenced simulated IMA(2,2) series:
acf(diff(ima22.s,difference=2) )

# We see the lag-1 and lag-2 autocorrelations are significantly nonzero ...
# This is EXACTLY what we would expect to see if differencing twice has produced a resulting MA(2) series.
# This is evidence that the original series 'ima.22.s' is in fact IMA(2,2).

# Exhibit 5.8
data(electricity)
plot(electricity)

# Clearly, a nonconstant mean function,
# and the variance gets larger as time increases.

# Exhibit 5.9
plot(log(electricity),ylab='Log(electricity)') 
# without specifying the y-label
# the default y-label is "electricity" rather than "log(electricity)"!

# The variance now appears constant over time.

# Exhibit 5.10
plot(diff(log(electricity)),ylab='Difference of Log(electricity)')

# Differencing the logged data has achieved stationarity.

# Detrending the logged data (assuming a linear time trend here) can also work,
# though not quite as well as differencing in this case:

trend = time(electricity)

fit.elec <- lm(log(electricity) ~ trend)
plot(residuals(fit.elec),ylab='Detrended Log(electricity) data',type='l')



# Exhibit 5.11
#win.graph(width=3, height=3,pointsize=8)
BoxCox.ar(electricity) 
# In case the function fails, check whether all data are positive. If
# not, shift all data by a fixed constant. If the function still fails,
# try setting method='ols', as an alternative
# to the default method of 'mle'.

# The Box-Cox plot shows that a natural log transformation (lambda=0) is quite reasonable here.# ######################################################################
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
# ######################################################################
# ########## R examples from Cryer and Chan - Chapter 7 ################
# ######################################################################

library(TSA)

# Below is a function that computes the method of moments estimator of
# the MA(1) coefficient of an MA(1) model.
estimate.ma1.mom=function(x){r=acf(x,plot=F)$acf[1]; if (abs(r)<0.5) 
  return((-1+sqrt(1-4*r^2))/(2*r)) else return(NA)}

# Exhibit 7.1 code

# Method of Moments Estimation:

data(ma1.2.s)  
# The true theta is -0.9 for this simulated data set
estimate.ma1.mom(ma1.2.s)

data(ma1.1.s)  
# The true theta is 0.9 for this simulated data set
estimate.ma1.mom(ma1.1.s)


set.seed(1234)
ma1.3.s=arima.sim(list(ma=c(.9)),n=60)  
# The true theta is -0.9 for this simulated data set

estimate.ma1.mom(ma1.3.s) 
# Since r1 = 0.544 for these simulated data, no MOM estimate of theta exists here.


ma1.4.s=arima.sim(list(ma=c(-0.5)),n=60)   
# The true theta is 0.5 for this simulated data set
estimate.ma1.mom(ma1.4.s)

data(ar1.s)
ar(ar1.s,order.max=1,aic=F,method='yw')
# the true phi is 0.9 for this simulated data set

data(ar1.2.s)
ar(ar1.2.s,order.max=1,aic=F,method='yw')
# the true phi is 0.4 for this simulated data set

data(ar2.s)
ar(ar2.s,order.max=2,aic=F,method='yw')
# the true phi_1 is 1.5 and the true phi_2 is -0.75 for this simulated data set


# MOM estimation of the square-root transformed hare data, based on an AR(2) model:

data(hare)

mean(sqrt(hare))

# Estimate of mu is 5.82.

ar(sqrt(hare),order.max=2,aic=F,method='yw')

# estimate of phi_1 is 1.1178, estimate of phi_2 is -0.519.

acf(sqrt(hare),plot=F)

# We see r1=0.736, r2=0.304.

var(sqrt(hare))

# Sample variance is 5.88.

# So the estimate of the noise variance is (recall formula for AR(2) model):

(1 - 1.1178 * 0.736 - (-0.519) * 0.304) * (5.88)


# MOM estimation of the differences of the log-transformed oil price data, based on an MA(1) model:

data(oil.price)

estimate.ma1.mom(diff(log(oil.price)))

# estimate of theta is -0.222.

# sample mean of differences of the logged oil prices is 0.004:

mean(diff(log(oil.price)))

# sample variance of differences of the logged oil prices is 0.0072:

var(diff(log(oil.price)))

# So the estimate of the noise variance is (recall formula for MA(1) model):

0.0072/(1+(-0.222)^2)

# Note:  Using MOM to estimate models containing MA terms is NOT recommended,
# so the example just above is for illustration purposes only!

#########################################################################

### Examples of LS and ML estimation of some simulated time series:

# Least-squares estimation of the simulated MA(1) series from earlier:

arima(ma1.4.s,order=c(0,0,1),method='CSS',include.mean=F)



# Exhibit 7.4

# A simulated AR(1) series (The true parameter value here is phi = 0.9):
data(ar1.s)
ar(ar1.s,order.max=1,aic=F,method='yw') # method of moments
ar(ar1.s,order.max=1,aic=F,method='ols') # conditional sum of squares
ar(ar1.s,order.max=1,aic=F,method='mle') # maximum likelihood
# The AIC option is set to be False otherwise the function will choose
# the AR order by minimizing AIC, so that zero order might be chosen.

# Another simulated AR(1) series (The true parameter value here is phi = 0.4):
data(ar1.2.s)
ar(ar1.2.s,order.max=1,aic=F,method='yw') # method of moments
ar(ar1.2.s,order.max=1,aic=F,method='ols') # conditional sum of squares
ar(ar1.2.s,order.max=1,aic=F,method='mle') # maximum likelihood

# Exhibit 7.5

# A simulated AR(2) series (the true parameter values here are phi_1 = 1.5 and phi_2 = -0.75):
data(ar2.s)
ar(ar2.s,order.max=2,aic=F,method='yw') # method of moments
ar(ar2.s,order.max=2,aic=F,method='ols') # conditional sum of squares
ar(ar2.s,order.max=2,aic=F,method='mle') # maximum likelihood

# How do these methods compare with a smaller sample size?

ar(ar2.s[1:20],order.max=2,aic=F,method='yw') # method of moments
ar(ar2.s[1:20],order.max=2,aic=F,method='ols') # conditional sum of squares
ar(ar2.s[1:20],order.max=2,aic=F,method='mle') # maximum likelihood

# For this example, MOM appears less accurate with the smaller sample size...

# Exhibit 7.6

# A simulated ARMA(1,1) series (the true parameter values here are phi=0.6, theta = -0.3):
data(arma11.s)
arima(arma11.s, order=c(1,0,1),method='CSS') # conditional sum of squares
# order=c(1,0,1) specifies p=1, d=0, q=1, so an ARMA(1,1) model.

arima(arma11.s, order=c(1,0,1),method='ML') # maximum likelihood

# 
# Recall that R uses the plus convention whereas our book uses the minus 
# convention in the specification of the MA part, i.e. R specifies an
# ARMA(1,1) model as z_t=theta_0+phi*z_{t-1}+e_t+theta_1*e_{t-1} 
# versus our convention
# z_t=theta_0+phi*z_{t-1}+e_t-theta_1*e_{t-1} 
#
# To match the result on page 165 of the book, reverse the sign of the estimates of theta.

# Parameter Estimation with some real time series:

# Exhibit 7.7

# Estimation of Color property data, based on an AR(1) model.
data(color)
ar(color,order.max=1,aic=F,method='yw') # method of moments
ar(color,order.max=1,aic=F,method='ols') # conditional sum of squares
ar(color,order.max=1,aic=F,method='mle') # maximum likelihood

# The ML estimate of phi is 0.57.  
# The estimates using the other methods are fairly similar.

# An important distinction between the 'ar' function and the more sophisticated 'arima' function:
# The 'arima' function displays standard errors for the estimates.
# The 'arima' function also shows the estimated mean of the time series process (unless include.mean=F is specified).

# In the 'color' time series, the process mean is clearly not zero (see plot):

plot(color, type='o')

# Using the 'arima' function (below, with the 'ML' method), allows us to see the estimate of mu, the mean of the process:

arima(color,order=c(1,0,0),method='ML')

# Note the estimate of theta is (virtually) the same as we saw with the 'ar' function, but 'arima' provides more output.

# Exhibit 7.8

# estimation of square-root transformed hare data, based on an $AR(3)$ model:

data(hare)
arima(sqrt(hare),order=c(3,0,0))

# Note that an ARIMA model with p=3, d=0, q=0 is just an AR(3) model...

# Exhibit 7.9

data(oil.price)
arima(log(oil.price),order=c(0,1,1),method='CSS') # conditional sum of squares
arima(log(oil.price),order=c(0,1,1),method='ML') # maximum likelihood

# Here, order=c(0,1,1) specifies an ARIMA model with 
# p=0, d=1, q=1, that is, an MA(1) model on the FIRST DIFFERENCES
# of the logged oil prices.
















# ######################################################################
# ########## R examples from Shumway and Stoffer - Estimation ###########
# ######################################################################



library(astsa)

# Yule-Walker (MOM) estimation of AR(2) model for recruitment data:

rec.yw = ar.yw(rec, order=2)
rec.yw$x.mean  # = 62.26278 (mean estimate)
rec.yw$ar      # = 1.3315874, -.4445447  (parameter estimates)
sqrt(diag(rec.yw$asy.var.coef))  # = .04222637, .04222637  (standard errors)
rec.yw$var.pred  # = 94.79912 (error variance estimate)

# Compare the MOM estimates above with the ML estimates here for the recruitment data:
arima(rec,order=c(2,0,0),method='ML')

# glacial varve series

data(varve)
plot(varve,type='o')
acf2(varve)
# Apparent non-stationarity in the varve series...

plot(diff(log(varve)),type='o')
acf2(diff(log(varve)))

# an MA(1) model seems appropriate
# for the differences of the logged thickness values

arima(log(varve),order=c(0,1,1),method='CSS') # conditional sum of squares
arima(log(varve),order=c(0,1,1),method='ML') # maximum likelihood

# The U.S. GNP data:

plot(gnp)
acf2(gnp,50)
gnpgr = diff(log(gnp))
plot(gnpgr)
acf2(gnpgr)

arima(gnpgr,order=c(1,0,0),method='ML') # fits an AR(1) model using ML
arima(gnpgr,order=c(0,0,2),method='ML') # fits an MA(2) model using ML

# Some large sample CIs:

alpha=0.05

# Directly using outputted standard error from R:
round(c(0.3467 - qnorm(1-alpha/2)*0.0627, 0.3467 + qnorm(1-alpha/2)*0.0627),4)



arima(diff(log(oil.price)),order=c(0,0,1),method='ML',include.mean=F) # fits MA(1) model using ML

alpha=0.10
# Directly using outputted standard error from R:
round(c(0.2956 - qnorm(1-alpha/2)*0.0693, 0.2956 + qnorm(1-alpha/2)*0.0693),4)

# Exhibit 7.10

library(TSA); data(hare);

res=arima(sqrt(hare),order=c(3,0,0),include.mean=T)
set.seed(12345)
coefm.cond.norm=arima.boot(res,cond.boot=T,is.normal=T,B=1000,init=sqrt(hare))
signif(apply(coefm.cond.norm,2,function(x){quantile(x,c(.025,.975),na.rm=T)}),3)
# Method I

coefm.cond.replace=arima.boot(res,cond.boot=T,is.normal=F,B=1000,init=sqrt(hare))
signif(apply(coefm.cond.replace,2,function(x){quantile(x,c(.025,.975),na.rm=T)}),3)
# Method II

coefm.norm=arima.boot(res,cond.boot=F,is.normal=T,ntrans=100,B=1000,init=sqrt(hare))
signif(apply(coefm.norm,2,function(x){quantile(x,c(.025,.975),na.rm=T)}),3)
# Method III

coefm.replace=arima.boot(res,cond.boot=F,is.normal=F,ntrans=100,B=1000,init=sqrt(hare))
signif(apply(coefm.replace,2,function(x){quantile(x,c(.025,.975),na.rm=T)}),3)
# Method IV

# Some bootstrap series may be explosive which will be discarded. To see
# the number of usable bootstrap series, run the command

dim(coefm.replace)
# the output should be 
# [1] 952   5
# i.e. we have only 952 usable (i.e. finite) bootstrap time series even though
# we simulate 1000 series.

# The theoretical confidence intervals were computed by the output in Exhibit 7.8.


# Compute the quasi-period of the bootstrap series based on the method of
# stationary bootstrap with the errors drawn from the residuals with replacement.

period.replace=apply(coefm.replace,1,function(x){
  roots=polyroot(c(1,-x[1:3]))
  # find the complex root with smalles magnitude
  min1=1.e+9
  rootc=NA
  for (root in roots) {
    if( abs(Im(root))<1e-10) next
    if (Mod(root)< min1) {min1=Mod(root); rootc=root}
  }
  if(is.na(rootc)) period=NA else period=2*pi/abs(Arg(rootc))
  period
})

sum(is.na(period.replace)) # number of bootstap series that do not admit a  well-defined quasi-period.

quantile(period.replace, c(.025,.975),na.rm=T)

# Exhibit 7.11
#win.graph(width=3.9,height=3.8,pointsize=8)
hist(period.replace,prob=T,main="",xlab="quasi-period",axes=F,xlim=c(5,16))
axis(2)
axis(1,c(4,6,8,10,12,14,16),c(4,6,8,10,12,14,NA))


# Exhibit 7.12

#win.graph(width=3,height=3,pointsize=8)
qqnorm(period.replace,main="") #Normal Q-Q Plot for the Bootstrap Quasi-period Estimates")
qqline(period.replace)




# ######################################################################
# ########## R examples from Cryer and Chan - Chapter 8 ################
# ######################################################################

library(TSA)

# Exhibit 8.1

#win.graph(width=4.875, height=3,pointsize=8)
data(color)
m1.color=arima(color,order=c(1,0,0))
m1.color
plot(rstandard(m1.color),ylab='Standardized residuals',type='b')
abline(h=0)

# Exhibit 8.2

data(hare)
m1.hare=arima(sqrt(hare),order=c(3,0,0))
m1.hare # the AR(2) coefficient is not significant; it is second in the
# list of coefficients.
m2.hare=arima(sqrt(hare),order=c(3,0,0),fixed=c(NA,0,NA,NA)) # fixed the AR(2)
# coefficient to be 0 via the fixed argument.
m2.hare
# Note that the intercept term is actually the mean in the centered form
# of the ARMA model, i.e. if y(t)=sqrt(hare)-intercept, then the model is
# y(t)=0.919*y(t-1)-0.5313*y(t-3)+e(t) 
# So the "true" intercept equals 5.6889*(1-0.919+0.5313)=3.483, as stated in
# the book!
plot(rstandard(m2.hare),ylab='Standardized residuals',type='b')
abline(h=0)

# Seems to be some non-constant variance over time ...

# How about for the unrestricted AR(3) model?

plot(rstandard(m1.hare),ylab='Standardized residuals',type='b')
abline(h=0)

# Similar pattern ...

# Exhibit 8.3
data(oil.price)
m1.oil=arima(log(oil.price),order=c(0,1,1))
plot(rstandard(m1.oil),ylab='Standardized residuals',type='l')
abline(h=0)

# Note a couple of possible outliers early in the series...

# Exhibit 8.4
#win.graph(width=3, height=3,pointsize=8)
qqnorm(residuals(m1.color))
qqline(residuals(m1.color))

shapiro.test(residuals(m1.color))

# Exhibit 8.5
qqnorm(residuals(m1.hare))
qqline(residuals(m1.hare))

shapiro.test(residuals(m1.hare))

# Exhibit 8.6
qqnorm(residuals(m1.oil))
qqline(residuals(m1.oil))

shapiro.test(residuals(m1.oil))

# Exhibit 8.9
#win.graph(width=4.875, height=3,pointsize=8)
acf(residuals(m1.color),main='Sample ACF of Residuals from AR(1) Model for Color')

# Exhibit 8.10
acf(residuals(arima(sqrt(hare),order=c(2,0,0))),main='Sample ACF of Residuals from AR(2) Model for Hare')

# Exhibit 8.11
acf(residuals(m1.color),plot=F)$acf
signif(acf(residuals(m1.color),plot=F)$acf[1:6],2)# to display the first 6 acf
# to 2 significant digits.

# Three different diagnostic plots about the residuals from our time series model:

# Exhibit 8.12 
#win.graph(width=4.875, height=4.5)
tsdiag(m1.color,gof=15,omit.initial=F) 
# the tsdiag function is modified from that in the
# stats package of R.

# Output from the Ljung-Box test:

# On the AR(1) model for the color data:

Box.test(residuals(m1.color), lag = 6, type = "Ljung-Box", fitdf = 1)

# fitdf should be set to p+q.  Here, for this AR(1) model, p=1 and q=0 (no MA terms).

# Runs tests on the AR(3) model for the square-root hare data:

runs(residuals(m1.hare))

# Exhibit 8.13
m1.color 

# Exhibit 8.14
m2.color=arima(color,order=c(2,0,0))
m2.color

# Exhibit 8.15
m3.color=arima(color,order=c(1,0,1))
m3.color

# Exhibit 8.16
m4.color=arima(color,order=c(2,0,1))
m4.color



# ######################################################################
# ##########    R examples from Shumway and Stoffer           ##########
# ######################################################################

library(astsa)
data(varve);data(gnp)

# glacial varve series
# an MA(1) model seems appropriate
# for the differences of the logged thickness values
# That implies an ARIMA(0,1,1) model for the logged thickness values

m1.logvarve <- arima(log(varve),order=c(0,1,1),method='ML') # maximum likelihood
print(m1.logvarve)
plot(rstandard(m1.logvarve),type='b')
abline(h=0)

qqnorm(residuals(m1.logvarve))
qqline(residuals(m1.logvarve))

shapiro.test(residuals(m1.logvarve))

acf(residuals(m1.logvarve),main='Sample ACF of Residuals from IMA(1,1) Model')

runs(residuals(m1.logvarve))

# Should we include an AR term?

m2.logvarve <- arima(log(varve),order=c(1,1,1),method='ML') # maximum likelihood
print(m2.logvarve)
plot(rstandard(m2.logvarve),type='b')
abline(h=0)

qqnorm(residuals(m2.logvarve))
qqline(residuals(m2.logvarve))

shapiro.test(residuals(m2.logvarve))

acf(residuals(m2.logvarve),main='Sample ACF of Residuals from ARIMA(1,1,1) Model')

runs(residuals(m2.logvarve))

# Is the AR coefficient in the estimated ARIMA(1,1,1) model significantly different from zero?

# What do the AIC's tell us?



# The U.S. GNP data:

#plot(gnp)
#acf2(gnp,50)
gnpgr = diff(log(gnp))
plot(gnpgr)
acf2(gnpgr)

ar1.gnpgr <- arima(gnpgr,order=c(1,0,0),method='ML') # fits an AR(1) model using ML
print(ar1.gnpgr)
ma2.gnpgr <- arima(gnpgr,order=c(0,0,2),method='ML') # fits an MA(2) model using ML
print(ma2.gnpgr)


# Some residual analysis:

plot(rstandard(ar1.gnpgr),type='b'); abline(h=0)

plot(rstandard(ma2.gnpgr),type='b'); abline(h=0)


acf(residuals(ar1.gnpgr),main='Sample ACF of Residuals from AR(1) Model')

acf(residuals(ma2.gnpgr),main='Sample ACF of Residuals from MA(2) Model')

# Some overfit models:

arima(gnpgr,order=c(2,0,0),method='ML') # fits an AR(2) model using ML
arima(gnpgr,order=c(0,0,3),method='ML') # fits an MA(3) model using ML
arima(gnpgr,order=c(1,0,2),method='ML') # fits an ARMA(1,2) model using ML# ######################################################################
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
