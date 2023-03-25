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

