#MS4218 Project Script:
#Data Importation:
install.packages("TSA")
library(TSA)
install.packages("rdatamarket")
library(rdatamarket)

measles <- dmseries("http://data.is/1DqES68")
measts <- as.ts(measles, freq=12, start=c(1928,1))
measts

#Remove last 10% of Observations
length(measts)
#Remove 53 observations, from January 1968 onward
newm <- window(measts, end=c(1968,1))
newm
#Will be using newm data for the entire analysis
plot(newm)

#Assessing Stationarity:
par(mfrow=c(2,1))
plot(newm)
acf(newm)
#The plot shows slight changes in variance over time in the data, but not too much
#Additionally, there are many significant lags in the acf
#The acf also seems to have a semi-sinusoidal pattern
#Exhibits lack of stationarity
#Does not necessarily show a change in terms of mean though

#Transformations to Assist with Stationarity:
dev.new(width=7, height=7)
BCm <- BoxCox.ar(newm, lambda=seq(-2,2,0.1))
BCm$mle
BCm$ci
#Based on output the boxcox suggests a power transformation of exactly 0.1
#There are no options in the bands, thus the power transformation by 0.1 is the only route
transm <- (newm^0.1)
plot(transm)
points(y=transm,x=time(transm), pch=as.vector(season(transm)), col=4, cex=0.8)
#The data displays less overall issues with slightly changing variance, and there does
#not seem to be a trend exhibited in the data other than possible seasonality
#Reviewed the monthly labels to see if seasonality should be investigated

#Checks for Normality:
#Original Data -- 
qqnorm(newm); qqline(newm)
shapiro.test(newm)
#VERY clearly skewed, both of the tails pull off on the upper end of the graph
#Data is right-skewed

qqnorm(transm); qqline(transm)
shapiro.test(transm)
#The ends are still skewed off of the lines for the qqplot; heavy tailed
#The shapiro test produces a pvalue = 3.757e-10, extremely small
#Reject the null, take the alternative that the data is NOT normally distributed
#Thus the transformation did not do a great job of normalizing the data
#We acknowledge this moving forward...

#Look at the above outputs as well as the residuals
#May need to reorder the above two sections of code
adf.test(transm)
#This test will tell if we need to difference or not
#Regular differencing does not seem to be required, but we will for seasonality
acf(transm)
#Sinusoidal pattern with little decay over time
pacf(transm)
#Lag 1,2 and lag 12,13 are significant! This suggests seasonality
eacf(transm)
#eacf suggests an ARMA(2,0) BUT there are no full lines of x's
#thus seasonality transformations may not be necessary

#Seasonal Differencing:
#We investigate this just to see how it affects the data
dev.new(width=10, height=7)
dtm <- diff(transm, lag=12)
plot(dtm)
#Seasonal differencing
#Check if the data seems stationary now
#There is not much changes in variance now, and the overall data seems to have a random pattern

#Model Identification:
par(mfrow=c(2,1))
acf(dtm)
#Sinusoidal with semi-degradation, may suggest AR orders
pacf(dtm)
#Main significant lags are 1 and 2, maybe lag 13, others may be random (which may suggest SARIMA model)
eacf(dtm)
#eacf suggests seasonality although already differenced seasonally
#This does not seem to assist with our analysis of the data, so we will not use this

msub <- armasubsets(transm, nar=12, nma=12)
plot(msub)
#Use notes and tutorial notes on how it decays, which values are signifcant
#and more to analyze which models may be the best fit or not

#Model Fitting/Comparison:
#USE residual checks when fitting models (see Lec 3 R code)
dev.new(width=8, height=4)
ti <- as.vector(time(transm))
#THE FOLLOWING IS FROM HIS TUTORIAL 3 CODE, CHANGE AND REMOVE AS NEEDED:
#SWAP IT OUT FOR ARMA models from above and more generalized versions --
M1 <- 
M2 <- 

summary(M1)
summary(M2)
#INSERT COMMENTS HERE

lines(x=ti, y=predict(M1), col=2)
lines(x=ti, y=predict(M2), col=3)

resid1 <- residuals(M1)
resid2 <- residuals(M2)
plot(x=ti, y=resid1, type="o")
# Residuals from model 1 do not look random - there is a quadratic trend.
plot(x=ti, y=resid2 type="o")
# Although there is no trend in the residuals for model 2, this still does not
# look random. Neighbouring values hang together.

runs(resid1)
runs(resid2)
# Very small p-values, i.e., reject the null hypothesis of independence as
# expected since neighbouring values are related.

acf(resid1)
acf(resid2)
# Here we can see significant correlations - again, this is in line with the
# plots of the residuals and the runs test.
#SEE QUESTIONS 2 and 3 FROM TUT 3 FOR GUIDANCE
#DONT FORGET TO ALSO FIT MORE GENERAL MODELS THAN JUST THE SUGGESTED ONE
#COMPARE OUTPUTS FOR BEST SOLUTION

tsdiag(____)
#insert models into above code 
#three graphs are plotted
#resid timeplot, resid acf, and then p-values from the ljung box test
#SEE LECTURE 7/8 CODE IN EMAIL

#Model Checking:
#IS some of this above? See good way to organize from his lecture notes
#Fix when editing in RMarkdown!!! MAKE it professional, see his reqs on analysis

#Forecasting:
#WILL LEARN IN CLASS ON MONDAY APRIL 3
