### R for Forecasting
lynx
time(lynx) # a hidden layer - time stamps to the vector 
length(lynx)
plot(lynx)

#ts() Function to generate time series data - can attach a time stamp to a vector
?ts() # frequently used options are '"start", "end" and "frequency"
set.seed(194)
data<-rnorm(200)
#Create a time series object starting from 1818
myts <- ts(data, start = (1818))
myts
time(myts)

### Excercise
#1. get a random variable (x) of 350 numbers - e.g.rnorm(),runif(). Set a seed so that your results 
#are reproducible
set.seed(57)
dataX = rnorm(350)
#2. Explor the use of ts(); add time component to x to creat a monthly dataset (y) starting in November 1914
Tdatax = ts(dataX, start = c(1914,11), deltat = 1/12)
#3. get a simple plot for the time series
plot(Tdatax)

### simple forecast methods
plot(myts)
install.packages(forecast)
library(forecast)
?library
?meanf
myts
meanm <- meanf(myts, h=20)
meanm
naivem <- naive(myts, h=20)
naivem
driftm <- rwf(myts, h=20, drift = T)
?plot
plot(meanm, plot.conf = F, main = "")
#plot.conf = F allows to get more lines on the plot
lines(naivem$mean, col=123, lwd = 2)
#lines adds more lines to the plot; $mean extracts the forecast value
#col set the color of the lines; lwd set the line width
lines(driftm$mean, col=22, lwd = 2)
?legend
legend("topleft",lty=1,col=c(4,123,22),
       legend=c("Mean method","Naive method","Drift Method"))

###### accuracy and model comparison

set.seed(194)
myts <- ts(rnorm(200), start = (1818))
#Get the first 170 samples
mytstrain <- window(myts, start = 1818, end = 1987)
mytstrain
plot(mytstrain)
#forecast for the next 30 years using the 170 samples
meanm <- meanf(mytstrain, h=30)
naivem <- naive(mytstrain, h=30)
driftm <- rwf(mytstrain, h=30, drift = T)
#The actual value for the 30 years
mytstest <- window(myts, start = 1988)
mytstest
time(mytstest)
#compare the the actual value and the forecasted value
accuracy(meanm, mytstest)
accuracy(naivem, mytstest)
accuracy(driftm, mytstest)

###### Residuals - the difference between forecast values and the 
#actual historic data over time
#Rule of thumb: you want all the patterns in the model, 
#only randomness stay in the residuals

set.seed(194)
myts <- ts(rnorm(200), start = (1818))
plot(myts)

meanm <- meanf(myts, h=20)
naivem <- naive(myts, h=20)
driftm <- rwf(myts, h=20, drift = T)

var(meanm$residuals)
mean(meanm$residuals)

mean(naivem$residuals)

naivwithoutNA <- naivem$residuals
naivwithoutNA <- naivwithoutNA[2:200]
var(naivwithoutNA)
mean(naivwithoutNA)

driftwithoutNA <- driftm$residuals
driftwithoutNA <- driftwithoutNA[2:200]
var(driftwithoutNA)
mean(driftwithoutNA)
hist(driftm$residuals)


## Exercise 
#Given, following data
set.seed(54)
myts <- ts(c(rnorm(50, 34, 10), 
             rnorm(67, 7, 1), 
             runif(23, 3, 14)))

#5. Plot the data, explain the statistical charaters of the data
#File name: myts plot

plot(myts)
#Based on my visual inspection, this plot presents statistical out of control due to fluctuation. 
#It may exceed UCL, and LCL

#6. Set up three forecasting models with 10 steps into future
mytstrain <- window(myts, start = 1, end = 112) 
#This command will forecast 1st to 112th 
meanm = meanf(mytstrain, h=10)

#h=10, 10 steps into future

naivem <- naive(mytstrain, h=10)

driftm <- rwf(mytstrain, h=10, drift = T)

#7. Get a plot with the three forecasts of the model, add a legend

#File name: 3 forecast model

plot(meanm, plot.conf= F ,main="")
lines(naivem$mean, col=123, lwd = 2)
lines(driftm$mean, col=22, lwd = 2)
legend("topleft",lty=1,col=c(4,123,22),
       legend=c("Mean method","Naive method","Drift Method"))
#Legend command will add a legend on graph

#8. Which method looks more promising
#Based on my observation, naive method looks more natural and promising.

#9. Get the error measures and compare them; do the results match the
#visual impression? if not, why?
mytstest = window(myts, start=113)
mytstest
accuracy(meanm, mytstest)
accuracy(naivem, mytstest)
accuracy(driftm, mytstest)
#Yes, the results match the visual impression. Naive method has the most accurate forecast comparing the actual data. Drift is also good. Mean method need lots of improvement because its MASE is much larger than standard.
