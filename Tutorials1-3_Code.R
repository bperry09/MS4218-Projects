#Tutorial One:
#Question 4:
#part a & b:
install.packages("TSA")
library(TSA)
x <- rnorm(50, mean = 10, sd = 6)
xnew <- ts(x)
plot(xnew)

#part c:
xm <- ts(x, freq=12, start=c(2010,5))
plot(xm)
points(y=xm, x=time(xm), pch=as.vector(season(xm)), col=4, cex=0.8)

#Question 5:
#part a:
y <- rnorm(50, mean=0, sd=1)
plot(y)
cor(y=y, x=zlag(y, d=1), use="pairwise")

#part b:
yn <- cumsum(y)
cor(y=yn, x=zlag(yn, d=1), use="pairwise")

#part c:
l <- rnorm(50, mean=0, sd=1)
m <- rnorm(50, mean=0, sd=1)
n <- rnorm(50, mean=0, sd=1)

ln <- cumsum(l)
mn <- cumsum(m)
nn <- cumsum(n)

dev.new(width=12, height=10)
par(mfrow=c(3,1))
plot(l)
plot(m)
plot(n)
par(mfrow=c(3,1))
plot(ln)
plot(mn)
plot(nn)

#Tutorial Two:
#Question 9:
F100m <- get.hist.quote("^ftse", start="2000-01-01", end="2015-12-31", quote=c("Close"), provider=c("yahoo"), compression="m")
F100m <- ts(F100m, freq=12, start=c(2000,1))
acf(F100m)
F100d <- diff(F100m, lag=12, diff=1)
acf(F100d)
F100d2 <- diff(F100m, lag=12, diff=2)
acf(F100d2)
#check back on this later to see if done correctly, diff twice did not produce good data

#Tutorial Three:
#Question 2:
#part a:
install.packages("TSA")
library(TSA)
data(wages)
plot(wages)
points(y=wages, x=time(wages), pch=as.vector(season(wages)), col=4, cex=0.8)

#part b and c:
ti <- as.vector(time(wages))
lw <- lm(wages~ti)
summary(lw)
lines(x=ti,y=predict(lw), col=2)

qw <- lm(wages~ ti + I(ti^2))
summary(qw)
lines(x=ti,y=predict(qw), col=5)

#part d:
predl <- predict(lw)
plot(predl, reslw, xlab="Predicted Values", ylab="Residuals")

predq <- predict(qw)
plot(predq, resqw, xlab="Predicted Values", ylab="Residuals")

#part d (extra):
dev.new(width=8, height=4)
par(mfrow=c(2,2))

reslw <- residuals(lw)
hist(reslw, main="LM Res")
qqnorm(reslw)
qqline(reslw)

resqw <- residuals(qw)
hist(resqw, main="QU Res")
qqnorm(resqw)
qqline(resqw)

#part e:
runs(reslw)
runs(resqw)

#part f:
acf(reslw, main="LM")
acf(resqw, main="QU")

#Question 3:
#part a:
data(beersales)
plot(beersales)
points(y=beersales, x=time(beersales), pch=as.vector(season(beersales)), col=4, cex=0.8)

#part b:
bsdecom <- decompose(beersales)
plot(bsdecom)

#part c:
months <- season(beersales)
modelbeer <- lm(beersales~ months)
summary(modelbeer)
plot(beersales, xlab="Year", ylab="Beer Sales", type="p")
lines(as.vector(time(beersales)), predict(modelbeer), col=2)

#part d:
resb <- residuals(modelbeer)
predb <- predict(modelbeer)
plot(predb, resb, xlab="Predicted Values", ylab="Residuals")

#part e:
months <- season(beersales)
ti <- as.vector(time(beersales))
modelbeer <- lm(beersales~ months + ti + I(ti^2))
summary(modelbeer)
#Higher R squared, very good fit.

#part f:
plot(x=ti, y=residuals(modelbeer), type="o")
#No significant trend in these residuals.

#part g:
#PROVIDED CODE
dev.new(width=8, height=4)
plot(beersales, xlim=c(1975,2000))
tinew <- seq(1975, 2000, by=1/12)
newdata <- data.frame(cbind(ti=tinew, months=season(beersales)))
newdata$months <- factor(newdata$months, labels=levels(season(beersales)))
pred <- predict(modelbeer, newdata=newdata)
lines(x=tinew, y=pred, col=2, lty=2)
#HIS NOTES:
# The model fits the data well in the observed time window but then predicts
# that beer sales will decline due to quadratic trend. This is relatively
# unlikely and shows the danger in developing a model for the trend compared
# to eliminating trend through differencing for example.

#Question 4:
#part a:
data(winnebago)
plot(winnebago)
points(y=winnebago, x=time(winnebago), pch=as.vector(season(winnebago)), col=4, cex=0.8)

#part b:
wdecom <- decompose(winnebago)
plot(wdecom)
#HIS CODE:
plot(wdecom$seasonal)
points(x=time(wdecom$seasonal), y=wdecom$seasonal,
pch=as.vector(season(wdecom$seasonal)), col=4, cex=0.8) 
# Sales are highest from March - June (peaking in April) and lowest from 
# October - January (lowest in January).

#part c:
logwinn <- log(winnebago)
plot(logwinn)
#looks to now have more increasing trend and less variation in actual values

#part d:
months <- season(winnebago)
ti <- as.vector(time(winnebago))
modellwinn <- lm(logwinn~ months + ti)
summary(modellwinn)

#part e:
residw <- residuals(modellwinn)
plot(x=ti, y=residw, type="o")
#Values seem to hang together, i.e., the are correlated.

#part f:
acf(residw)
#As expected, there are significant autocorrelations.
runs(residw)
#Runs test also confirms lack of independence with very small p-value.

#part g:
#From residuals part of lecture 3, exact code:
dev.new(width=7, height=7)
par(mfrow=c(2,1))
hist(residw)
qqnorm(residw); qqline(residw)
shapiro.test(residw)
#The residuals appear to reasonably normally distributed apart from one
#(or maybe two) outliers, based on normal QQ plot.

#part h:
plot(predict(modellwinn), residw, xlab="Predicted Values", ylab="Residuals")
#There appears to be slightly less variance for larger predicted values.

#Question 5:
data(oilfilters)
plot(oilfilters)
points(y=oilfilters, x=time(oilfilters), pch=as.vector(season(oilfilters)), col=4, cex=0.8)

#part b:
acf(oilfilters)
#Clear lag-12 autocorrelation because spike at 1.0.

#part c:
oildecom <- decompose(oilfilters)
plot(oildecom)
plot(oildecom$seasonal)
points(x=time(oildecom$seasonal), y=oildecom$seasonal,
pch=as.vector(season(oildecom$seasonal)), col=4, cex=0.8) 

#part d:
months <- season(oilfilters)
ti <- as.vector(time(oilfilters))
modeloil <- lm(oilfilters~ months)
summary(modeloil)

dev.new(width=8, height=4)
plot(oilfilters)
lines(x=as.vector(time(oilfilters)), y=predict(modeloil), col=2)
#The fit is good - although the pattern is slightly different in 1984
#compared to the later years.

#part e:
resido <- residuals(modeloil)
plot(x=ti, y=resido, type="o")
#Reasonably random, although higher variation in 1984 time frame.
#Same code as above:
dev.new(width=7, height=7)
par(mfrow=c(2,1))
hist(resido)
qqnorm(resido); qqline(resido)
shapiro.test(resido)
#Appears to be non-normal - mainly due to a few outliers.

dev.new(width=7, height=7)
plot(predict(modeloil), resido, xlab="Predicted Values", ylab="Residuals")
#Variance appears to be constant.
acf(resido)
#No significant autocorrelation.
runs(resido)
#P-value is high (0.801) so we accept the hypothesis that the series is
#indpendent - this is in line with the ACF.

#part f:
oilmodhar <- lm(oilfilters~harmonic(oilfilters,m=1))
summary(oilmodhar)
#Low R-squared (0.3672).

dev.new(width=8, height=4)
plot(oilfilters)
lines(x=as.vector(time(oilfilters)), y=predict(oilmodhar), col=2)
#The fit is clearly poor, does not match all the variation.

#part g:
oilmodhar <- lm(oilfilters~harmonic(oilfilters,m=5))
summary(oilmodhar)
#R-squared is 0.697. More complexity does not improve from here.

dev.new(width=8, height=4)
plot(oilfilters)
lines(x=as.vector(time(oilfilters)), y=predict(oilmodhar), col=2)
#The fit is much better and seems to line up along as much variation as possible.
