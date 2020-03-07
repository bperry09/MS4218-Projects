#Tutorial Work continued (lectures 4-6):
#Tutorial 4 done in notebook; all paper work, no code
#Will do remaining problems from it for final practice

#Tutorial 5:
#Question 5:
install.packages("TSA")
library(TSA)
data(SP)
#part a:
plot(SP)

#part b:
dSP <- diff(SP)
plot(dSP)
#still fans out with non constant variance so now utilize log transform
dlSP <- diff(log(SP))
plot(dlSP)
#recall that log has to occurr first so diff log the original dataset
#now the data seems to be random with no particular trend

#part c:
acf(dlSP)
#only one signifcant lag, so this seems to be an appropriate transformation

#Question 6:
data(larain)
#part a:
plot(larain)
#looks relatively stationary, no clear trend or change in variation

#part b:
qqnorm(larain); qqline(larain)
shapiro.test(larain)
#the plot does not show normality, the ends skew off the line
#additionally, the p-value is extremely low for the shapiro test

#part c:
BCla <- BoxCox.ar(larain)
BCla$mle
BCla$ci
#lamda approx between 0 and 0.5
#this means we could utilize a log transformation for the data
#the given MLE = 0.2 power transform

#part d:
lla <- log(larain)
plot(lla)
powerla <- larain^0.25
plot(powerla)

#part e:
qqnorm(powerla); qqline(powerla)
shapiro.test(powerla)
#All of the plots and p-value suggest normality in the transformed data set

#Question 7:
data(JJ)
#part a:
plot(JJ)
#Both increasing linear trend for the dataset as well as
#Increasing variance in the data as time increases

#part b:
BCjj <- BoxCox.ar(JJ)
BCjj$mle
BCjj$ci
#The confidence interval is from 0.1 to 0.3
#The recommended MLE value is 0.2
#No great transformation fits in the lines, so we will utilize the given one

#part c:
jjpower <- JJ^0.2
plot(jjpower)
#The data has random variance now, but there is still trend in the data over time
#This trend can be removed with differencing, then it will be stationary

#part d:
djj <- diff(jjpower)
plot(djj)
#The data looks better now in terms of trend and variance, but there is still
#variance and it is not ideal. Perfect stationarity will not be achieved.

#Tutorial 6:
#Questions 1-2 were done in class; see notes on those in email
#Question 3:
data(tempdub)
#part a:
par(mfrow=c(2,1))
plot(tempdub)
acf(tempdub)
#The series does NOT look stationary, there is clear seasonal trend
#Additionally the acf has no decay over time and follows a cyclic pattern

#part b:
adf.test(tempdub)
#The p-value is less than 0.01, which tells us to reject the null
#This alternative hypothesis tells us it is believed to be stationary...

#part c:
dt <- diff(tempdub, lag=12)
#seasonal differencing on the data
plot(dt)
acf(dt)
#based on the acf, only the lag 12 value is significant
pacf(dt)
#the lag 12 is also significant for the pacf graph
#thus we would think that _____ ????????
#REVIEW HERE WHAT MODEL IS APPROPRIATE - SEE SOLNS WHEN POSTED

#part d:
dev.new(width=8, height=8)
as <- armasubsets(dt, nar=12, nma=12)
plot(as)
#REVIEW WHICH MODELS THIS SUGGESTS AND HOW TO READ THE OUTPUT ???

#Question 4:
data(robot)
#part a:
plot(robot)
#slight trend? no clear pattern in variance
dev.new(width=8, height=6)
acf(robot)
#The acf is semi-decaying, but not rapidly, and many of the values are significant
#NOT stationary

#part b:
pacf(robot)
#WHAT WOULD BE SUGGESTED HERE BASED ON THESE GRAPHS - acf and pacf!

#part c:
eacf(robot)
#This plot suggests an ARMA(1,1) based on the triangle pattern

#part d:
asr <- armasubsets(robot, nar=, nma=)
#NEED TO SET VALUES FOR THOSE PIECES TO ANALYZE

#part e:
dr <- diff(robot)
plot(dr)
#no real pattern in the differenced data
acf(dr)
#lag one is the most significant, there are two others, but hardly cross the CI
pacf(dr)
#pacf seems to decay after the first lag and then values stay within CI
#This may suggest an MA(1) ???
eacf(dr)
#As suggested by the other graphs, the triangle pattern in the eacf
#seems to suggest as well that an ARMA(0,1) or MA(1) is a suitable choice
asdr <- armasubsets(dr, nar=8, nma=8)
plot(asdr)
#IS THIS RIGHT AND DOES IT AGREE WITH THE ABOVE?
