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
