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



