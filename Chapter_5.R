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

# The Box-Cox plot shows that a natural log transformation (lambda=0) is quite reasonable here.