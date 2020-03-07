#MS4218 Project Script:
#Data Importation:
install.packages("TSA")
library(TSA)
install.packages("rdatamarket")
library(rdatamarket)

robberies <- dmseries("http://data.is/1muuVxi")
robberies
rob <- as.ts(robberies, freq=12, start=c(1966,1))
rob

#Data Set Checks:
acf(rob)
dm <- diff(rob)
plot(dm)
acf(dm)

#Remove last 10% of Observations
length(rob)
#Remove 11 observations, all data from after November 1974
crime <- window(rob, end=c(1974,11))
crime
#Will be using crime data for the entire analysis

#Assessing Stationarity:
par(mfrow=c(2,1))
plot(crime)
acf(crime)
#The plot shows a change in variance over time in the data
#Additionally there is an overall trend to the data over time
#Change in mean and variance imply differencing and possibly log transformation
#There are many significant lags in the acf, seems to show slow decay over time
#Exhibits lack of stationarity

#Transformations to Assist with Stationarity:
dev.new(width=7, height=7)
BCm <- BoxCox.ar(crime, lambda=seq(-2,2,0.1))
BCm$mle
BCm$ci
#Based on output the boxcox suggests a power transformation between 0 and 0.3
#The MLE suggests 0.2, but...
#Since the log transformation is in this interval, we will utilize that 
#since it is a known transformation
transm <- log(crime)
plot(transm)
points(y=transm,x=time(transm), pch=as.vector(season(transm)), col=4, cex=0.8)
#The data displays much less overall issues with changing variance, as expected
#Reviewed the monthly labels to see if seasonality should be investigated
#The next main thing to work on is the overall trend, this can be handled by differencing

#Differencing:
#Diff-log transformations are common (see his examples)
#To remove the overall increasing trend in the data
dev.new(width=10, height=7)
dtm <- diff(transm)
plot(dtm)
#Check if the data seems stationary now
#There is not much changes in variance now, and the overall data seems to have a random pattern
#Looks much better from the original dataset, and so we proceed with checks for normality

#Checks for Normality:
#Original Data -- 
qqnorm(crime); qqline(crime)
shapiro.test(crime)
#VERY clearly skewed, both of the tails pull off the ends of the graph
#Data is not normal, and this is agreed by the p-value = 0.00001357
#Now we investigate the transformed and differenced data (diff log data)

qqnorm(dtm); qqline(dtm)
shapiro.test(dtm)
#Diff log data that should remove non-stationarity issues
#The ends are still slightly skewed off of the lines for the qqplot
#But perfect normality is hard to achieve with real data
#The shapiro test produces a pvalue = 0.1647
#Thus the transformation did a relatively good job of normalizing the data
#We take this and then compare it to the Dicky-Fuller

adf.test(dtm)
#The Dicky-Fuller test output a p-value = 0.01 suggests rejecting the null
#Thus we accept stationarity, so we have our d for an ARIMA model, next we need
#to determine the p and q values
#We will continue with the data as is transformed 
#Thus we believe from the graphs and the above values that stationarity is achieved

#Model Identification:
acf(dtm)
#Lag one displays as the only remaining significant line in the acf
pacf(dtm)
#Lag 1,2 are significant in the pacf
#As discussed in lecture, it is not easy to determine a model from just these two
eacf(dtm)
#The extended acf is used for determining the orders of p and q in an ARMA process
#eacf suggests an ARMA(0,1) or an ARMA(1,1), both will be investigated for fit
#which is actually an ARIMA(0,1,1) and ARIMA (1,1,1) on the logged data called as transm
#Additionally there are no lines of full x's thus seasonality is not suggested either

#Investigating ARMA subsets:
#Any values within 2 units of the "best choice" are worth consideration
par(mfrow=c(3,3))
plot(armasubsets(dtm,nma=4,nar=4))
#Tells us MA(1) with some AR subsets
plot(armasubsets(dtm,nma=6,nar=6))
#Again gives us ARMA(0,1) and ARMA(2,1)
#Both of which would be investigated based on output from the eacf
plot(armasubsets(dtm,nma=8,nar=8))
#At this point it suggests ARMA(8,1) and some other variations
#This does not seem to agree that well with the previous findings
plot(armasubsets(dtm,nma=10,nar=10))
plot(armasubsets(dtm,nma=12,nar=12))
#Thus we investigate the -2.4 and 1.9 suggestions for the diff, log data
#The first tells us ARMA(8,1) with significant Yt-8 terms
#The second tells us ARMA(12,1) with significant Yt-12, Yt-8 terms 

#Test ARIMA(0,1,1), ARIMA(1,1,1) and whatever models can be generalized from the eacf output
#ARIMA(2,1,1), ARIMA(1,1,2), ARIMA(0,1,2)
#Five models to test based on taking all of the above outputs into consideration

#Model Fitting/Comparison:
dev.new(width=8, height=4)
crimemod1 <- arima(transm, order=c(0,1,1))
crimemod2 <- arima(transm, order=c(1,1,1))
crimemod3 <- arima(transm, order=c(2,1,1))
crimemod4 <- arima(transm, order=c(0,1,2))
crimemod5 <- arima(transm, order=c(1,1,2))

crimemod1
crimemod2
crimemod3
crimemod4
crimemod5
#Summary outputs of the models
#As we know, the models will not have intercepts due to the differencing involved
#The model with the lowest AIC is the ARIMA(1,1,2), but none of the coeff are significant
#The other models have a mix of significant and not significant coeff
#ADD them in to the project write up HERE!!!
#Important to show the confidence intervals to show work analyzing the models

#Residuals:
tsdiag(crimemod1)
tsdiag(crimemod2)
tsdiag(crimemod3)
tsdiag(crimemod4)
tsdiag(crimemod5)
#Based on all of the work above, and from various outputs, Model 1 seems to be the best choice
#This is based on the coefficient being statistically significant
#The aic was in the middle of all of the models, but the coeff was sig
#Plus model 5 had no sig coeff, and model 3 was mixed and both aic's were within 0.9 of the model 3 aic value
#It kept the model the least complicated while maintaining residual plots that are accepted
#Only one possible flaw is the p-value that falls right along the Ljung-Box Test, and that is not enough to make us change the model

#Model Checking:
#ARIMA(0,1,1) is the chosen model for this dataset
tsdiag(crimemod1)
resid <- residuals(crimemod1)
qqnorm(resid); qqline(resid)
shapiro.test(resid)
#The qqplot although not perfect, suggest reasonable normality, and when investigated
#the other models also displayed some deviation, thus it was not taken as a setback
#Also the shapiro test agrees with a p-value = 0.3433
#Overall, this model is an appropriate, albeit not perfect, fit for this dataset

#All of the above points to the ARIMA(0,1,1) model

#Forecasting:
#This will predict 15 months ahead, although only 11 months were removed for the initial 10%
pred <- predict(crimemod1, n.ahead=15)
pred
pred$pred - 1.96*pred$se
pred$pred + 1.96*pred$se

dev.new(width=8, height=4)
plot(crimemod1, n.ahead=15, ylab="Boston Armed Robberies")
plot(crimemod1, n.ahead=15, ylab="Boston Armed Robberies", transform=exp, n1=c(1974,1))
#Transform.exp is utilized to undo the log transformation that was done
#Very similar to the oil price data that we investigated in class
#We can clearly see that the prediction limits are getting wider with time.
#This arises since the series is non-stationary, i.e., we cannot be too
#certain as to what the series will do next.

dev.new(width=10, height=8)
par(mfrow=c(2,1))
plot(rob)
#Data without 10% removed
plot(crimemod1, n.ahead=15, ylab="Boston Armed Robberies")


dev.new(width=10, height=8)
par(mfrow=c(2,1))
plot(rob, xlim=c(1974,1976))
plot(crimemod1, n.ahead=15, ylab="Boston Armed Robberies", transform=exp, n1=c(1974,1))
#It is important to note that the y-axis scales for these graphs are different
#We see that the predicted values maintain a linear pattern, but their limits are getting wider with time
#This is due to the trend in the original series, and so we are uncertain of the exact next steps in the series
#Thus when comparing the two, although the actual data has more fluctuation in the values
#The prediction limits capture all of those values
#Thus although the predicted points themselves are not accurate, the overall prediction captures what occurs in the actual dataset
#This is displayed in the plotted side by side comparison zoomed into the predicted years
