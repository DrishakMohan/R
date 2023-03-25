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
arima(gnpgr,order=c(1,0,2),method='ML') # fits an ARMA(1,2) model using ML