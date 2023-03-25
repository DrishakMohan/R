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




