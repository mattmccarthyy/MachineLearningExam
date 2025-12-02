# ------------------------------------------------------------------
# ST6026  Workshop 3
# 02/2025
# Cormac
# ------------------------------------------------------------------

rm(list=ls()) # this clears up the environment (clean slate)

# source("workshops/ST6026_package.R")
source("scripts\\ST6026_package.R")

# ------------------------------------------------------------------
# Question 1
#install.packages("bootstrap")
library(bootstrap)
B = 100 # number of bootstrap resamples
x = law$LSAT #?law
y = law$GPA
cor(x,y)

#Get bootstrap estimate of correlation
n = length(x) # sample size
bcor = numeric(B) # used for storing bootstrapped values
for(b in 1:B){
  ib = sample(1:n, size=n, replace=TRUE)
	xstar = x[ib]
	ystar = y[ib]
	# wrong way:
	# xb = sample(x, size=n, replace=TRUE)	
	# yb = sample(y, size=n, replace=TRUE)
	# we want to be sampling pairs not predictor and response values independently
	bcor[b] = cor(xstar, ystar)
}

summary(bcor)
hist(bcor)
abline(v=cor(x,y), lwd=3, col='navy')
sd(bcor)

#We never used bootstrap
bootstrap(law$LSAT,100,mean)
hist(bootstrap(law$LSAT,40,mean)$thetastar)

# ------------------------------------------------------------------
# Question 2
#install.packages("MASS")
library(MASS)

x = cars
n = nrow(x)
summary(lm(dist~speed, data=cars))
plot(cars, pch=20, cex=1)

#Q2a
lm(dist~speed, data=cars)$coef

#Q2b
B = 10000 # number of bootstrap resamples
b.coefs = matrix(NA, nrow=B, ncol=2)
# OR
b.intercept = numeric(B)
b.slope = numeric(B)

for(b in 1:B){
	ib = sample(1:n, size=n, replace=TRUE)
	xb = x[ib, ] # b-th bootstrap resample
	lmob = lm(xb$dist ~ xb$speed)
	b.coefs[b,] = lmob$coef
	# alternative parameter holding variables ↓ 
	b.intercept[b] = lmob$coef[1]
	b.slope[b] = lmob$coef[2]
}

lm(dist~speed, data=cars)$coef #original coefficients
#-------
apply(b.coefs, 2, mean)
apply(b.coefs, 2, sd)
# OR
mean(b.intercept)
mean(b.slope)
sd(b.intercept)
sd(b.slope)

#2c
boxplot(b.coefs, names=c('Intercept','Slope'))
boxplot(b.coefs[,1])
boxplot(b.coefs[,2], main="estimated effects of speed on stopping distance")

# sampling distribution for the effect of speed on stopping distance
hist(b.coefs[,2])
mean(b.coefs[,2]) # estimated expected value of the estimator 
sd(b.coefs[,2])   # estimated standard error of the estimator 
# naive 95% confidence interval around that effect
qs = quantile(b.coefs[,2], c(.025,.975))
abline(v=qs, lwd=3)

#Q2d
plot(b.coefs[,1]~b.coefs[,2], xlab="slope",ylab="intercept")
plot(speed~dist,data=cars)

#This is purely a visual, you do not need to understand this
#?kde2d # computes a two-dimensional kernel density estimate
#?persp # creates a 3D perspective plot (a surface plot) of a matrix of values
persp(kde2d(b.coefs[,1],b.coefs[,2]), phi = 20,theta = 10)

#2d alternative
#install.packages("rgl")
library(rgl)
persp3d(kde2d(b.coefs[,1],b.coefs[,2]), col= "lightblue",alpha= 0.9,
        xlab= "Intercept",
        ylab= "Slope",
        zlab= "Density")
#bimodal distribution

# ------------------------------------------------------------------
# Question 3
#install.packages("MASS")
library(MASS)
#?Boston
set.seed(6026)
n = nrow(Boston)
dat = Boston[sample(1:n, n), ] #shuffling
x = dat[, c('crim', 'indus', 'rm', 'tax')] 
y = dat$medv

x <- as.matrix(x)
lmo = lm(y~x) #lm(medv~crim+indus+rm+tax,data=dat)
summary(lmo)

# (a) single split evaluation
i.train = 1:(n/2)
y.test = y[-i.train]
fit = lm(medv~., data=dat[i.train,])
pred = predict(fit, newdata=dat[-i.train,])
err.split = mean( (pred-y.test)^2 )
RMSQ=sqrt(err.split)

# (b) LOO-CV
err.LOO = numeric(n)
err.Loo.RMSQ=numeric(n)
for(i in 1:n){
	i.train = c(1:n)[-i]
	y.test = y[-i.train] #y[i]
	fit = lm(medv~., data=dat[i.train,])
	pred = predict(fit, newdata=dat[-i.train,]) #predict(fit, newdata=dat[i,])
	err.LOO[i] = mean( (pred-y.test)^2 )
	err.Loo.RMSQ[i]= sqrt(mean( (pred-y.test)^2 ))
}
mean(err.LOO)
sd(err.LOO)
mean(err.Loo.RMSQ)
sd(err.Loo.RMSQ)

# (c) 
K = 5
err.K5 = numeric(K) ; RMSE.K5= numeric(K)
folds = cut(1:n, K, labels=F)
for(k in 1:K){
	i.train = which(folds!=k)
	y.test = y[-i.train]
	fit = lm(medv~., data=dat[i.train,])
	pred = predict(fit, newdata=dat[-i.train,])
	err.K5[k] = mean( (pred-y.test)^2 )
	RMSE.K5[k]=sqrt(mean( (pred-y.test)^2 ))
}
mean(err.K5)
sd(err.K5)
mean(RMSE.K5)
sd(RMSE.K5)

# (d) 
#cut(1:n, K, labels=c("a","b","c","d","e"))
#cut(1:n, K, labels=FALSE) #messing with cut function
K = 10
fit.K10 = err.K10 = numeric(K)
folds = cut(1:n, K, labels=FALSE)
for(k in 1:K){
	i.train = which(folds!=k)
	err.K10y.test = y[-i.train]
	fit = lm(medv~., data=dat[i.train,])
	fit.K10[k] = mean( (fit$residuals)^2 ) # training error
	pred = predict(fit, newdata=dat[-i.train,])
	err.K10[k] = mean( (pred-err.K10y.test)^2 )
}
mean(err.K10)
sd(err.K10)
boxplot(fit.K10, err.K10, names=c("training error","test error"))

#e
# CV estimates of prediction error (RMSE) 
sqrt(c(err.split, mean(err.LOO), mean(err.K5), mean(err.K10)))

par(mfrow=c(1,2))
boxplot(err.LOO, err.K10, err.K5 ,
	main="Different CV error estimates",
	names=c("LOO-CV","K=10","K=5"))
abline(h=err.split)
# close-up
boxplot(err.LOO, err.K10, err.K5, 
	ylim=c(0,50), #adjust (i.e. reduce) range of y-axis in plot
	main="Close-up",
	names=c("LOO-CV","K=10","K=5"))
abline(h=err.split)
# ------------------------------------------------------------------
# Question 4

# No code required :)

# (a) A and B are likely to be training set errors, and C and D test set errors.
# It's typical from a CV framework to have more spread around the test set error.
# A and C could come from one model and B and D from the other, as they come from 
# models that are applied to the same data and that are similar in shape/pattern
# (2 linear models). This suggests they should behave similarly.
# Since one training error is lower than the other, it suggests more overfitting
# (or maybe just a better fit!!) from that first model. As a result the test set 
# error from the first model is likely to be higher.

# (b) That's because fewer points are used in evaluating test set errors:
# N = 263, K=10 means we use about 26 points in each test fold (and 230+ 
# points for model training). Averaging errors over a larger number of values 
# will reduce variability ie we end up with a less variable error estimate
# from training data.

# (c) One possible explanation is that Model 1 (lower training error) is a
# simple GLM (ie standard regression model) and Model 2 is a regularised model.
# (Since regularisation leads to less overfitting).

# (d) Model 2 maybe, since less overfitting? But...
# - the 2 distributions of test errors are in fact similar (Statistically)
# - interpretability of the simpler model may be key - people would prefer
# a model with a clear story attached to it

