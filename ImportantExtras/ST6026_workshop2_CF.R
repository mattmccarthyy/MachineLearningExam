# ------------------------------------------------------------------
# ST6026  Workshop 2
# 02/2025

# ------------------------------------------------------------------

rm(list=ls()) # this clears up the environment (clean slate)

source("C:\\Users\\User\\Documents\\PGDipPRS\\ST6026\\Workshop2\\ST6026_package.R")
source("scripts\\ST6026_package.R")

# ------------------------------------------------------------------
# Question 1
#install.packages("glmnet")
library(glmnet)

dat <- read.table(file="C:\\Users\\User\\Documents\\PGDipPRS\\ST6026\\Workshop2\\Blood Pressure.txt",
	header=TRUE)
dat <- read.table(file="data\\Blood Pressure.txt",
                  header=TRUE)
View(dat)	
str(dat)
head(dat)
dat$PatientID = NULL # getting rid of this variable

# GLM fit
glm.fit <- glm(Systolic ~ ., data = dat)
summary(glm.fit)

# MSE
mean(glm.fit$residuals^2)

#install.packages("car")
library(car)
vif(glm.fit) # variable inflation factor 
# (minimum 1, high VIF indicates collinearity)
cor(dat)
round(cor(dat), 3)
plot(dat$Waist, dat$BMI, pch=20)

################################################
# LASSO - note: fitting these models is rather cody...
alpha = 1
# prepare the data:
xm = model.matrix(Systolic~., data=dat)[,-1]
y = dat$Systolic
# compute best lambda:
lam = cv.glmnet(xm, y, alpha=alpha)
# c) fit model using best lambda:
lasso = glmnet(xm, y, lambda=lam$lambda.min)
# d) recover fitted values:
lasso.fit = predict(lasso, newx=xm)
# e) calculate RSS or MSE from LASSO
mean((lasso.fit-y)^2)
#################################################

# cf. ST6026_package.R for a more user-friendly function!!!
# regularized.model()

set.seed(4060)
ridge.fit = regularized.model(dat,"Systolic",alpha=0)
enet.fit = regularized.model(dat,"Systolic",alpha=.6)
lasso.fit = regularized.model(dat,"Systolic",alpha=1)


effects = cbind(as.numeric(coef(glm.fit)),
                as.numeric(coef(ridge.fit$model)),
                as.numeric(coef(enet.fit$model)),
                as.numeric(coef(lasso.fit$model)))
rownames(effects) = rownames(coef(ridge.fit$model))
colnames(effects) = c("GLM","ridge","e-net","LASSO")
effects

y = dat$Systolic
Errors = c(mean(glm.fit$residuals^2), 
           mean((ridge.fit$fitted.values-y)^2),
           mean((enet.fit$fitted.values-y)^2),
           mean((lasso.fit$fitted.values-y)^2))

effects = data.frame(rbind(effects, Errors))
round(effects, 3)

# ------------------------------------------------------------------
# Question 2
#install.packages("ISLR")
#install.packages("MASS")
#install.packages("glmnet")
library(ISLR)
library(MASS)
#library(glmnet)

Hitters.train = read.csv(file="C:\\Users\\User\\Documents\\PGDipPRS\\ST6026\\Workshop2\\Hitters_train.csv", 
	stringsAsFactors=TRUE)
Hitters.test = read.csv(file="C:\\Users\\User\\Documents\\PGDipPRS\\ST6026\\Workshop2\\Hitters_test.csv", 
	stringsAsFactors=TRUE)

Hitters.train = read.csv(file="data\\Hitters_train.csv", 
                         stringsAsFactors=TRUE)
Hitters.test = read.csv(file="data\\Hitters_test.csv", 
                        stringsAsFactors=TRUE)

# quick look at the data:
dim(Hitters.train)
dim(Hitters.test)
str(Hitters.train)
# distributions:
par(mfrow=c(2,2))
for(i in 1:ncol(Hitters.train)){
	hist(as.numeric(Hitters.train[,i]), # histogram for i-th variable
		col=8, # paint in grey
		main=names(Hitters.train[i])) # add plot title as variable name
}
# relationships to Y:
par(mfrow=c(2,2))
for(i in 1:ncol(Hitters.train)){
	plot(Hitters.train$Salary~Hitters.train[,i], 
		col=8, 
		main=names(Hitters.train[i]))
}
par(mfrow = c(1,1))

#Q2a
# fit GLM to "training set"
glm.fit = glm(Salary ~ ., data = Hitters.train)
summary(glm.fit)

# generate test set predictions from fitted GLM model
glm.pred = predict(glm.fit, newdata=Hitters.test)

#Q2b
# compare "training" and "test" errors from GLM 
train.err = mean(glm.fit$residuals^2)
test.err = mean((glm.pred-Hitters.test$Salary)^2)
test.err;train.err;test.err-train.err

#Q2c
# fit and assess LASSO
lasso.fit = regularized.model(Hitters.train,"Salary",alpha=1)
lasso.pred = predict.regularized.model(lasso.fit, Hitters.test)

#Q2d
# compare "training" and "test" errors from LASSO
train.err = mean((lasso.fit$fitted.values-Hitters.train$Salary)^2)
test.err = mean((lasso.pred-Hitters.test$Salary)^2)
test.err;train.err;test.err-train.err

summary(glm.fit)
round(cbind(coef(glm.fit), coef(lasso.fit$model)), 4)

#2e
#Interpretation??
test.err-train.err
a=cbind(coef(glm.fit), coef(lasso.fit$model))
colnames(a)=c("glm.fit","lasso.fit")
round(a, 4)
x# Now encapsulate this within a loop; do it over and over on
# different "splits" of the original dataset, and see how 
# the models perform on average...

#Q2f
# First we recreate the original "complete" dataset:
#sapply(Hitters,function(x){any(is.na(x))})
dat = na.omit(Hitters) #Removes all rows with missing cases (we dont have any)
n = nrow(dat)
dat$Salary = log(dat$Salary) #why are we logging this here 
set.seed(6026)
dat = dat[sample(1:n, n, replace=FALSE),] #shuffle the dataset

REPS = 100
train.error = test.error = numeric(REPS)
lasso.train.error = lasso.test.error = numeric(REPS)
i.train = c(1:200)

for(j in 1:REPS){
	# shuffle original dataset
	dat = dat[sample(1:n, n, replace=FALSE),]
	# split into train + test sets
	train = dat[i.train,]
	test = dat[-i.train,]
	
	fit = glm(Salary~., data=train)
	pred = predict(fit, newdata=test)
	
	train.error[j] = mean(fit$residuals^2)
	test.error[j] = mean((pred-test$Salary)^2)

	lasso.fit = regularized.model(train,"Salary",alpha=1)
	lasso.pred = predict.regularized.model(lasso.fit, test)

	lasso.train.error[j] = mean((lasso.fit$fitted.values-train$Salary)^2)
	lasso.test.error[j] = mean((lasso.pred-test$Salary)^2)
}

par(mfrow=c(1,1))
par(font=2, font.axis=2)
boxplot(train.error, test.error, 
		main="MLB player salary prediction",
		names=c("Training error", "Test error"),
		col=c('pink','cyan'))
		
par(font=2, font.axis=2)
boxplot(train.error, test.error, 
		lasso.train.error, lasso.test.error, 
		main="MLB player salary prediction",
		names=c("Training error", "Test error", "LASSO training error", "LASSO test error"),
		#names=c("Training \n error", "Test \n error", "LASSO \ntraining error", "LASSO \ntest error"),
		col=c('pink','cyan','darkgreen','orange'))
abline(h=median(train.error), lty=2, col='cyan')
abline(h=median(lasso.train.error), lty=2, col='darkgreen')
abline(h=median(test.error), lty=2, col='pink')
abline(h=median(lasso.test.error), lty=2, col='orange')


# ------------------------------------------------------------------
# Question 3
#library(glmnet)
rm(list=ls())

dat.train = read.csv(file="C:\\Users\\User\\Documents\\PGDipPRS\\ST6026\\Workshop2\\titanic_train.csv", 
	stringsAsFactors=TRUE)
dat.test = read.csv(file="C:\\Users\\User\\Documents\\PGDipPRS\\ST6026\\Workshop2\\titanic_test.csv", 
	stringsAsFactors=TRUE)

dat.train = read.csv(file="data\\titanic_train.csv", 
                     stringsAsFactors=TRUE)
dat.test = read.csv(file="data\\titanic_test.csv", 
                    stringsAsFactors=TRUE)
str(dat.train)
dat.train$PassengerId = NULL
dat.test$PassengerId = NULL
dat.train$Name = NULL
dat.test$Name = NULL
dat.train$Ticket = NULL
dat.test$Ticket = NULL
dat.train$Cabin = NULL
dat.test$Cabin = NULL

# clean out a couple of tricky observations:
i.rm = which(!(dat.train$Embarked %in% levels(dat.test$Embarked))) #what levels of embarked are in the training set but not the testing one 
dat.train = dat.train[-i.rm,] #remove those observations
dat.train = droplevels(dat.train)  #drop the unused levels
str(dat.train)

# remove incomplete observations:
dat.train = na.omit(dat.train)
dat.test = na.omit(dat.test)

# fix problem specification to classification:
dat.train$Survived = as.factor(dat.train$Survived)
dat.test$Survived = as.factor(dat.test$Survived)

# quick look at the data:
dim(dat.train)
dim(dat.test)
str(dat.train)
# distributions:
par(mfrow=c(3,3))
for(i in 1:ncol(dat.train)){
	hist(as.numeric(dat.train[,i]), # histogram for i-th variable
		col=8, # paint in grey
		main=names(dat.train[i])) # add plot title as variable name
}
# relationships to Y:
par(mfrow=c(3,3))
for(i in 1:ncol(dat.train)){
	plot(dat.train[,i]~dat.train$Survived, 
		col=8, 
		main=names(dat.train[i]))
}

#Q3a
# fit GLM to "training set"
glm.fit = glm(Survived ~ . , data=dat.train, family = "binomial")
# generate test set predictions from fitted GLM model
glm.pred = predict(glm.fit, newdata=dat.test, type="response") #If you dont use response you get the log-odds

#Q3b
# fit and assess LASSO
lasso.fit = regularized.model(dat.train,"Survived",alpha=1)
lasso.pred = predict.regularized.model(lasso.fit, dat.test, 
	type="response")

#Q3c
#install.packages("pROC")
library(pROC)
y.true = dat.test$Survived
roc.glm = roc(response=y.true, predictor=glm.pred)
roc.lasso = roc(response=y.true, predictor=lasso.pred[,1])
roc.glm$auc
roc.lasso$auc

par(mfrow = c(1,1))
plot(roc.glm)
plot(roc.lasso, col=4, add=TRUE)
legend("bottomright", legend=c("GLM", "LASSO"),
	lwd=3, col=c(1,4), bty='n')

