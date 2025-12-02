# ------------------------------------------------------------------
# ST6026  Workshop 5
# 04/2025
# Cormac
# ------------------------------------------------------------------

rm(list=ls()) # this clears up the environment (clean slate)

# Load necessary files and libraries
## Note: You need to use the exact location on you PC for the ST6026_package.R
#source("C:\\Users\\User\\Documents\\PGDipPRS\\ST6026\\ST6026_package.R")
source("scripts\\ST6026_package.R")
## Note: You may need to install the libraries before loading them
library(glmnet)
library(caret)
library(ISLR)
library(MASS)
library(randomForest)
# ------------------------------------------------------------------
# Question 1
# Load the Hitters dataset
## Removing observations with missing values
dat = na.omit(Hitters)
# Create a training and testing set
## Set seed so the shuffle and split is the same every time
set.seed(6026)
head(dat)
dim(dat)
## Choose a randomly shuffled 70% of the data for training
i.cv = sample(1:nrow(dat), round(nrow(dat)*.70), replace=FALSE)
dat.cv = dat[i.cv,]
## Set Salary to be the y (outcome variable)
dat.cv.y = dat.cv$Salary
## Remove Salary from you x data (predictor variables)
dat.cv$Salary = NULL
## Create test data (x variables)
dat.test = dat[-i.cv,]
## Create test outcome (y variable)
dat.test.y = dat.test$Salary
## Remove Salary from you test x data (predictor variables)
dat.test$Salary = NULL

# # if not splitting 70-30:
# dat.cv = dat
# dat.cv.y = dat.cv$Salary
# dat.cv$Salary = NULL

# 5-fold CV
## Set seed so the shuffle and split is the same every time
set.seed(6026)
## Set up the format for training:
### i.e. 5-fold CV
trc_5cv = trainControl(method="cv",number=5)
caret.kcv = train(dat.cv, dat.cv.y, method="rf", trControl=trc_5cv)
## View
caret.kcv

#Train a a random forest to the training set via 50 bootstrap resamples, by adapting the above code
# OOB=Out of Bag 
## Set seed so the shuffle and split is the same everytime
set.seed(6026)
## Set up the format for training:
### i.e. 50 bootstrap resamples
trc_50b = trainControl(method="boot",number=50)
## Train your data
caret.oob = train(dat.cv, dat.cv.y, method="rf", trControl=trc_50b) # <- takes a bit to run
## View
caret.oob 

#Q1a
# comparing outputs:
## Look at each
caret.kcv
caret.oob
## Calculate the average RMSE across each fold/bootstrap
mean(caret.kcv$resample$RMSE)
mean(caret.oob$resample$RMSE)
#Compare CIs
quantile(caret.kcv$resample$RMSE,c(0.025,0.975))
quantile(caret.oob$resample$RMSE,c(0.025,0.975))

## Boxplot of RMSE for each model
boxplot(caret.kcv$resample$RMSE, caret.oob$resample$RMSE,
        names = c("5-fold CV", "50 Bootstrap"))

#Q1b
## Predict the Salary for the training sample using each model
yhat_cv = predict(caret.kcv, newdata=dat.cv)
yhat_oob = predict(caret.oob, newdata=dat.cv)
## RMSE for each observation in each model
sqrt(mean((yhat_cv-dat.cv.y)^2))
sqrt(mean((yhat_oob-dat.cv.y)^2))

# predictions:
## Predict the Salary for the test sample using each model
pred.kcv = predict(caret.kcv, newdata=dat.test)
pred.oob = predict(caret.oob, newdata=dat.test)
## RMSE for each test observation in each model
rmse.kcv = sqrt( mean( (pred.kcv-dat.test.y)^2 ) )
rmse.oob = sqrt( mean( (pred.oob-dat.test.y)^2 ) )
c(rmse.kcv,rmse.oob)

#Be cautious when comparing point estimates.

# ------------------------------------------------------------------
# Question 2
# - inspect correlations
# - pre-filtering > see if it boosts prediction
# - assess over-fitting

dat = na.omit(Hitters)

# a)
#cor(dat) #generates an error
summary(dat)
## We can see NewLeague and League devision is a categorical variable (won't work)
## We can also see that this includes Salary
### we can both remove the Y variable and "massage" 
### the feature matrix within one line:
### (one-hot encoding)
datm = model.matrix(Salary~.+0,data=dat)
summary(datm)
## Number of parameters/predictor variables
P = ncol(datm)
## Correlation matrix
M = cor(datm)
## Rounded to 2 decimal places 
round(M,2) # just to see what this looks like!
#or
library(DataExplorer)
plot_correlation(dat)
## how many features are highly correlated to others?
sum(abs(M)>.9)-P # "-P" excludes the correlation with the same variable (correlation with itself)
(sum(abs(M)>.9)-P)/2 #We have 13 pairs that are highly correlated (run abs(M[1:5,1:5])>.9 to see how pairs duplicate)

# b)
## find variables that have high correlation with others
## to reduce the number of predictors
fc = findCorrelation(M)
## How many variables are we removing
length(fc)
## now pretend you're an MLB (Major League Baseball) expert and know which variables to remove
fi = c(grep("League",colnames(datm)), 
		grep("Division",colnames(datm)),
		which(colnames(datm)=="Assists")) #similar to grep("Assists",colnames(datm))

# create 2 feature sets: 
# the full set and a subset with no high correlations
## Full
x = datm
## Without high correlation
xs = datm[,-fc]
## Without MLB expert removed
xi = datm[,-fi]
## Salary as y variable
y = dat$Salary

# Fit RF
## Set seed so the shuffle and split is the same every time
set.seed(6026)
## Set up the format for training:
### i.e. 5-fold cv
trc = trainControl(method="cv",number=5)
## Train your full data
mod = train(x, y, method="rf", trControl=trc)

set.seed(6026)
## Train your model without high correlation data
mods = train(xs, y, method="rf", trControl=trc)

set.seed(6026)
## Train your expert data
modi = train(xi, y, method="rf", trControl=trc)

# inspect variable importance:
varImp(mod)
varImp(mods)
varImp(modi)
# RMSE
mean(mod$resample$RMSE)
mean(mods$resample$RMSE)
mean(modi$resample$RMSE)
#CI
quantile(mod$resample$RMSE,c(0.025,0.975))
quantile(mods$resample$RMSE,c(0.025,0.975))
quantile(modi$resample$RMSE,c(0.025,0.975))

# c)
# conclusion of multiple univariate paired t-tests?
## Set up 20 empty values
p.values = numeric(ncol(x))
## Univariate t-test for each predictor
for(i in 1:P){
	p.values[i] = t.test(x[,i], y)$p.value
}
#Could scale to t.test(x[,i]/sd(x[,i]), y/sd(y))$p.value, would this be useful?
## Histogram of the p-values
hist(p.values)
## How many are greater than 0.05
which(p.values>.05)
which(p.values>.05/P)  #Bonferroni correction.

# d)
# perform (recursive feature elimination) RFE on initial model and: 
# - identify best subset
# - inspect variable importance and compare with rankings from full set
# - compare RMSEs
# - repeat on RF model fit to feature subset based on correlation pre-filtering

## Set up the format for training:
### i.e. 5-fold cv
rfc = rfeControl(method="cv",number=5)
#rfc = rfeControl(method="cv",number=5,allowParallel=TRUE),  using package "doParallel"

## Train your data str(x)
rfe.mod = rfe(x, y, method="rf", rfeControl=rfc)  #Might take a couple minutes.


## Identify the best feature subset 
rfe.mod$optVariables
## VIMP
varImp(rfe.mod)/max(varImp(rfe.mod)[,1])*100
varImp(mod)
## RMSE
mean(mod$resample$RMSE)
mean(rfe.mod$resample$RMSE)
## Using correlation filtered subset
rfe.modi = rfe(xi, y, method="rf", rfeControl=rfc)

## Identify the best feature subset
rfe.modi$optVariables

## RMSE
mean(rfe.modi$resample$RMSE)

## e) What we did wrong?
## No training and testing split
## So can't make conclusions about the accuracy of the model for new data

# ------------------------------------------------------------------
# Question 3 
#rm(list=ls()) 
##  Blood Pressure data
#dat = read.table(file="C:\\Users\\User\\Documents\\PGDipPRS\\ST6026\\Blood Pressure.txt",header=TRUE)
dat=read.table("data\\Blood Pressure.txt"
               ,header=T) 
## Have a look at it
str(dat)
dat$PatientID = NULL # getting rid of this variable

# GLM fit
glm.fit = glm(Systolic~., data=dat)
summary(glm.fit)
# Ridge Fit
## Recall ST6026_package.R for a more user-friendly function!!!
source("scripts\\ST6026_package.R")
## regularized.model()
set.seed(6026)
ridge.fit = regularized.model(dat,"Systolic",alpha=0)

## Find highly correlated variables
str(dat) 
fi = findCorrelation(cor(dat),cutoff=.8)

# GLM fit on subset
glm.sfit = glm(Systolic~., data=dat[,-fi])
summary(glm.sfit)
# compare to original fit:
summary(glm.fit)
# ridge fit on subset
set.seed(6026)
ridge.sfit = regularized.model(dat[,-fi],"Systolic",alpha=0)

## Compare coefficients 
effects = cbind(as.numeric(coef(glm.fit)),
                as.numeric(coef(ridge.fit$model)))
rownames(effects) = rownames(coef(ridge.fit$model))
colnames(effects) = c("GLM","ridge")

effects_sub = cbind(as.numeric(coef(glm.sfit)),
                    as.numeric(coef(ridge.sfit$model)))
rownames(effects_sub) = rownames(coef(ridge.sfit$model))
colnames(effects_sub) = c("GLM_sub","ridge_sub")
effects
effects_sub

round(effects,3)
round(effects_sub,3)

# ------------------------------------------------------------------
# Question 4 (cf. Workshop 3 Question 3)

#dat = read.csv(file="C:\\Users\\User\\Documents\\PGDipPRS\\ST6026\\cancer_data.csv")
dat = read.csv("data\\cancer_data.csv")

# y = 1 if pt survived more than 2 yrs
## Set x
x = dat
## Change y to a factor
dat$y = as.factor(dat$y)
## Create y variable 
y = dat$y
## Remove y from predictor set
x$y = NULL
## Set the number of predictors
P = ncol(x)

# a)
## Inspect correlation structure 
fi = findCorrelation(cor(x),cutoff=.8)
length(fi)
#library(DataExplorer)
plot_correlation(dat)

# b)
## Fit logistic regression model of full set
lmo = glm(y~., data=dat, family="binomial")
summary(lmo) #None are significant

#c
## Use the first 30 patients for univariate t-test 
i.sel = c(1:30)
## Set up empty values
ps = numeric(P)
## Run Univariate tests for all predictors
for(j in 1:ncol(x)){
	ps[j] = t.test(x[i.sel,j]~y[i.sel])$p.value
}

## What proportion have p-value < 0.05
mean(ps<.05);sum(ps<.05)
mean(ps<(.05/P)); sum((ps<(.05/P)))


## Select all patients not used in univariate tests
dat.cv = dat[-i.sel,]
## Select all predictors which have p-value < 0.05
dat.cv.subset = dat.cv[,which(ps<(.05))]  #str(dat.cv.subset)
## Select all predictors except first column
x.cv = dat.cv[,-1]
## Find the highly correlated variables
fi = (findCorrelation(cor(x.cv)) + 1)  #Add 1 since the first column was removed
length(fi)
## And remove them
dat.cv.subset = dat.cv[,-fi]


# Set up for 5 fold-CV
K = 5
gof.K5 = gof.K5.subset = numeric(K) #gof = goodness of fit
err.K5 = err.K5.subset = numeric(K)
folds = cut(1:nrow(dat.cv), K, labels=FALSE)
for(k in 1:K){
	i.train = which(folds!=k)
	y.test = dat.cv[-i.train,]$y
	
	# Fit RF on the full data and find goodness of fit and prediction error
	fit = randomForest(y~., data=dat.cv[i.train,])
	gof.K5[k] = sum(diag(fit$confusion))/sum(fit$confusion[,c(1,2)])
	pred = predict(fit, newdata=dat.cv[-i.train,])
	cm = confusionMatrix(pred,reference=y.test)
	err.K5[k] = cm$overall[1] 
	#↑ same statistic as sum(diag(fit$confusion))/sum(fit$confusion[,c(1,2)])
	
	# Fit RF on the subset data and find goodness of fit and prediction error
	fit.subset = randomForest(y~., data=dat.cv.subset[i.train,])
	gof.K5.subset[k] = sum(diag(fit.subset$confusion))/sum(fit.subset$confusion[,c(1,2)])
	pred.subset = predict(fit.subset, newdata=dat.cv.subset[-i.train,])
	cm = confusionMatrix(pred.subset,reference=y.test)
	err.K5.subset[k] = cm$overall[1]
}
## Compare
boxplot(gof.K5, err.K5, gof.K5.subset, err.K5.subset, 
	main="Accuracies",
	names=c(paste(c("full\n"),c("(train)","(test)")),
			paste(c("reduced\n"),c("(train)","(test)"))))

mean(gof.K5-err.K5)
mean(gof.K5.subset-err.K5.subset)
mean(gof.K5)
mean(gof.K5.subset)
mean(err.K5)
mean(err.K5.subset)
