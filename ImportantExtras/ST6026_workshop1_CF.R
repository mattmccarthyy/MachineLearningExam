##########################################
##########################################
# ST6026 Workshop 1
# 02/2025
##########################################
##########################################

# Question 1
### NOTE: If you have not set up an R project
### Where I have "data\\..."
### you will need to input the location on your computer
## a)
### Read in the Blood_Pressure.txt file
blood_pressure_txt <- read.table("data\\Blood_Pressure.txt",
           header = T)

head(blood_pressure_txt)
### Write out the blood_pressure_txt dataframe as a .csv file
write.csv(blood_pressure_txt,
          file = "write/Blood Pressure.csv")

head(read.csv("write/Blood Pressure.csv"))
# NOTICE the extra column with row numbers? Let's fix this...
write.csv(blood_pressure_txt,
          file = "write/Blood Pressure.csv",
          row.names = FALSE)
head(read.csv("write/Blood Pressure.csv"))


## b)
### Read in the Blood Pressure.csv file
blood_pressure_csv <- read.csv("data/Blood Pressure CSV.csv")

### Write out the blood_pressure_csv dataframe as a .txt file
write.table(blood_pressure_csv,
            file = "write/Blood_Pressure.txt",
            row.names = FALSE)


head(read.table("write/Blood_Pressure.txt"))
#we can't notice these quotation marks by reading the table back in
#We must use a text editor to see them

# NOTICE the "quotation marks" around the column headings? Let's fix this...
write.table(blood_pressure_csv,
            file = "write/Blood_Pressure.txt",
            row.names = FALSE,
            quote = FALSE)

# NOTICE the information is very crowded? Let's fix this...
write.table(blood_pressure_csv,
            file = "write/Blood_Pressure.txt",
            row.names = FALSE,
            quote = FALSE,
            sep = "\t")

#Note that creates a tab separated file, 
#The extension could be changed to .tsv
write.table(blood_pressure_csv,
            file = "write/Blood_Pressure.tsv",
            row.names = FALSE,
            quote = FALSE,
            sep = "\t")

##########################################
##########################################
# Question 2 - Regression
#install.packages("ISLR")
library(ISLR)

## a)
### What variables are in the Wage dataset?
names(Wage)

### What "classes" are they?
class(Wage)
class(Wage$wage)
class(Wage$age)
class(Wage$education)
Wage$education

sapply(Wage,class)
str(Wage)
## b)
### Basic plot
plot(x = Wage$age, y = Wage$wage)
### Improve the styling, change the dot type (pch),
## change the colour (col)
plot(Wage$age, Wage$wage,
     pch = 16,
     col = "red")
### Improve the styling, add x-label (xlab) and y-label (ylab)
plot(Wage$age, Wage$wage,
     pch = 16,
     col = 2,
     xlab = "Age",
     ylab = "Wage",
     cex=1)


## c
### fit a basic GLM to this
wage.glm <- glm(Wage$wage ~ Wage$age, data = Wage,family=gaussian)
wage.lm <- lm(Wage$wage~Wage$age,data=Wage)
summary(wage.glm)
summary(wage.lm)

## d
### obtain a non-parametric regression curve
plot(Wage$age, Wage$wage,
     pch = 16,
     col = 2,
     xlab = "Age",
     ylab = "Wage",
     cex=0.5)

np.curve = lowess(Wage$age, Wage$wage)
#str(np.curve)
### add this smooth, non-parametric curve to the plot
lines(np.curve, col = "navy", lwd = 3)

## e
### What do the distributions of each of the features
## and of the response variable look like?
### Create a suitable plot to inspect the distributions
##of variables wage, age and education from this dataset.
hist(Wage$wage)
hist(Wage$age)
plot(Wage$education)
### NOTICE the categories of education are crowded? Let's fix this...
par(mar = c(8, 4, 4, 2))
plot(Wage$education, las = 2)
par(mar = c(5, 4, 4, 2))
plot(Wage$education,cex.names=0.6)

plot(Wage$education,names=c("<HS","HS","SC","CG","AD"))
table(Wage$education)

## f
### Do the features have any relationship with the response variable?
### Create a plot to visualize relationships between variable wage and each of year, age and education.
names(Wage)

scatter.smooth(Wage$year, Wage$wage,
     pch = 16,
     col = 3,
     xlab = "Year",
     ylab = "Wage")

scatter.smooth(Wage$age, Wage$wage,
     pch = 16,
     col = 2,
     xlab = "Age",
     ylab = "Wage")

plot(Wage$education, Wage$wage,
     col=2:6,
     xlab="Education",
     ylab="Wage")
### OR
boxplot(Wage$wage ~ Wage$education,
        data = Wage,
        col = c(2:6),
        xlab = "Education",
        ylab = "Wage",
        names=c("<HS","HS","SC","CG","AD"))

boxplot(Wage$wage ~ Wage$year,
        data = Wage,
        col = 2:8,
        xlab = "Year",
        ylab = "Wage")

colours = c(2, "blue", 5, 3, "lightgrey")
plot(Wage$age, Wage$wage,
     pch = 16,
     col = colours[Wage$education],
     xlab = "Age",
     ylab = "Wage")

legend("topright", 
       legend = levels(Wage$education), 
       pch=16,
       col = colours,
       cex = 0.6,    # Scales the text size to 60% of default
       pt.cex = 0.6, # Scales the legend symbols
       bty = "n" )   #boxtype=none

##########################################
##########################################
# Question 3 - Classification
iris
head(iris)
## a)
### Plot Sepal.Width against Sepal.Length
plot(x = iris$Sepal.Width, y = iris$Sepal.Length)
### Colour of dots with respect to Species
colours <- c("black", "red", "blue")
plot(iris$Sepal.Width, iris$Sepal.Length,
     col = colours[iris$Species],
     pch = 16)
legend("topright", 
       legend = levels(iris$Species), 
       pch=16,
       col = colours)

### Increase the dot size (cex)
plot(iris$Sepal.Width, iris$Sepal.Length,
     col = colours[iris$Species],
     cex = 3,
     pch = 16)

## c)
hist(iris$Petal.Width) #Doesn't need to be rescaled
class(iris$Petal.Width)
### Set the dot with respect to Petal.Width
plot(iris$Sepal.Width, iris$Sepal.Length,
     col = colours[iris$Species],
     cex = iris$Petal.Width,
     pch = 16)
legend("topright",legend = levels(iris$Species),
       col=colours,
       pch=16
       )

##########################################
##########################################
# Question 4 - Unsupervised Learning (Clustering)
### Idea: What if we didn't know the Species?
## a)
head(iris)
table(iris$Species)
head(iris[,c(1:4)])
### Independent Variables (Used to make predictions)
x = iris[,c(1:4)]
### Dependent Variables (What we are trying to predict)
y = iris[,5]
# or y = iris$Species
### Number of categories (clusters) we want to separate into
K = 3
### Clustering function (without scaling)
ko = kmeans(x, K)
str(ko)
table(ko$cluster)
### Colour dots with respect to clusters
plot(iris$Sepal.Width, iris$Sepal.Length,
     col = ko$cluster,
     pch = 16,
     main="K-means Without Scaling")
#Compare to plot of species
plot(iris$Sepal.Width, iris$Sepal.Length,
     col = colours[iris$Species],
     pch = 16) #     cex = iris$Petal.Width,
legend("topright",legend = levels(iris$Species),
       col=colours,
       pch=16
)

## b)
### Clustering with scaling
### Scale the date
summary(x)
#apply(x, 2, scale) works, but scales using (x-mean(x))/sd(x). 
#we want to scale using (x-min(x))/(max(x)-min(x))
?scale
head(x)
z = apply(x, 2, function(x){(x-min(x))/(max(x)-min(x))})
z
head(z)
summary(z)
### Clustering function (with scaling)
?kmeans
koSc = kmeans(z, K)
#table(koSc$cluster)
### Colour dots with respect to clusters
plot(iris$Sepal.Width, iris$Sepal.Length,
     col = koSc$cluster,
     pch = 16,
     main="K-means With Scaling")
#Compare
plot(iris$Sepal.Width, iris$Sepal.Length,
     col = colours[iris$Species],
     pch = 16) #     cex = iris$Petal.Width,
legend("topright",legend = levels(iris$Species),
       col=colours,
       pch=16
)

##########################################
##########################################
# Question 5
### Regression example with performance analysis
#install.packages("MASS")
library(MASS)

?Boston
## a)
### first shuffle the data
n = nrow(Boston)
set.seed(6026)
dat = Boston[sample(1:n, n, replace=FALSE),]
dim(dat)
sdat = dat[,7:14] # subset eliminating the first 6 features
names(sdat)
## b)
### Create a training set and testing set for the full data
dat_train <- dat[1:400,]
dat_test <- dat[401:506,]
### GLM of training set of full data
dat_train.glm <- glm(medv ~ ., data = dat_train)
summary(dat_train.glm)
## c)
### Create a training set and testing set for the subset
sdat_train <- sdat[1:400,]
sdat_test <- sdat[401:506,]
### GLM of training set of subset data
sdat_train.glm <- glm(medv ~ ., data = sdat_train)
summary(sdat_train.glm)
## d)
### Compare the models AICs
dat_train.glm$aic 
sdat_train.glm$aic

## e)
### Make predictions for the test sets
dat_pred <- predict(dat_train.glm, newdata = dat_test)
sdat_pred <- predict(sdat_train.glm, newdata = sdat_test)

### Compare the mean squared error
# MSE's of the test set predictions
mean((dat_test$medv - dat_pred)^2)
mean((sdat_test$medv - sdat_pred)^2)

##########################################
##########################################
# Question 6 - Classification and ROC analysis
#You will need to install these packages if you haven't already
#install.packages("tree")
#install.packages("pROC")
library(tree)
library(pROC)
library(ISLR) 

## a)
?Default
### Load the Default dataset
dat = Default
### Remove the student variable
head(dat) #cor(dat$balance,dat$income)
dat$student = NULL

### Create a tree classifier
classifier = tree(default ~ ., data = dat)
classifier
### display the tree
plot(classifier)
text(classifier)
#Note the tree algorithm found that income did not improve the splits enough
#The two No's on the left will differ in probability
summary(classifier)

## b)
### Find the classification prediction probability
pred <- predict(classifier)
head(pred)
#hist(pred[,1],breaks=10, main="Probablity of not defaulting")
#hist(pred[,2],breaks=10, main="Probablity of defaulting")
## c)
### Calculate the ROC
roc <- roc(response=dat$default,predictor=pred[,1])

## d)
### Plot the ROC
plot.roc(roc,print.auc=T)
plot.roc(roc,legacy.axes = T)

#Note the x axis decreases from left to right

## e)
### Calculate the AUC
auc(roc)

#Very high AUC, but important to note: 
#We are testing the model on the same data we trained it on

