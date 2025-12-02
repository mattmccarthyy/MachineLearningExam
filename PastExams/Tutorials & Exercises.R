# Question 0.1 ---------------------------------------------------------------
# (a)
# setwd("~/Library/CloudStorage/OneDrive-UniversityCollegeCork/2025_26/Semester 1/ST4060")
# getwd()

#(b)
airquality <- airquality
head(airquality)
?airquality
dim(airquality)
names(airquality)
summary(airquality)

#(c)
summary(airquality$Ozone)
airquality[,1:4]
airquality[,c(1,2,4)]
attach(airquality)
summary(Ozone)
median(Ozone) #cant be dont bc of NAs
median(Ozone, na.rm = TRUE)
mean(Ozone, na.rm = TRUE)
sum(is.na(Ozone))
Oz <- na.omit(Ozone)
mean(Oz); median(Oz); sd(Oz); quantile(Oz, probs = 0.25)

#(d)
airquality[(Wind>10) & (Temp<70),]
cool_n_windy <- airquality[(Wind>10) & (Temp<70),]
cor(cool_n_windy$Wind,cool_n_windy$Temp)
cor(na.omit(cool_n_windy$Wind),na.omit(cool_n_windy$Temp))

#(e)
plot(airquality)
pairs(airquality[,1:4])
hist(Oz)
plot(Ozone~Temp)
boxplot(Ozone~Month)

#(f)
# skip I don't care

#(g)
iqr <- function(x){
  unname(quantile(x,0.75)-quantile(x,0.25))
}
iqr(Oz)

detach(airquality)


# Question 0.2 -------------------------------------------------------------
#(a)

#(b)
dim5 <- scan("cinq.pgm", what = integer(), nmax = 2, skip = 2)
dim5
vals5 <- scan("cinq.pgm", what = integer(), skip = 4)
mat5 <- matrix(vals5, nrow = dim5[2], ncol = dim5[1], byrow = TRUE)
image(mat5)
# things that are wrong:
# Image rotated by 90 degrees
# Wrong colour
# aspect ratio wrong
# includes scales which we dont need

# (c)
flipv <- function(m) {
  t(m[nrow(m):1, ])
}

mimage <- function(m, ...) {
  image(m,
        col  = gray.colors(256),  # greyscale
        axes = FALSE,
        ...)
}
mimage(flipv(mat5))

#(d)
wrap <- function(fname) {
  path <- file.path(fname)
  
  dims <- scan(path, what = integer(), nmax = 2, skip = 2, quiet = TRUE)
  
  w <- dims[1]; h <- dims[2]
  
  vals <- scan(path, what = integer(), skip = 4, quiet = TRUE)
  
  m <- matrix(vals, nrow = h, ncol = w, byrow = TRUE)
  
  mimage(flipv(m))
}

wrap("cinq.pgm")
wrap("quatre.pgm")
wrap("trois.pgm")
wrap("deux.pgm")
wrap("un.pgm")
wrap("zero.pgm")

# (e)
wrap1 <- function(fname) {
  path <- file.path(fname)
  
  dims <- scan(path, what = integer(), nmax = 2, skip = 2, quiet = TRUE)
  
  w <- dims[1]; h <- dims[2]
  
  vals <- scan(path, what = integer(), skip = 4, quiet = TRUE)
  
  m <- matrix(vals, nrow = h, ncol = w, byrow = TRUE)
  
  flipv(m)
}

frame5 <- wrap1("cinq.pgm")
frame4 <- wrap1("quatre.pgm")
frame3 <- wrap1("trois.pgm")
frame2 <- wrap1("deux.pgm")
frame1 <- wrap1("un.pgm")
frame0 <- wrap1("zero.pgm")

movie <- array(c(frame5,frame4,frame3,frame2,frame1,frame0), 
               dim = c(nrow(frame5), ncol(frame5), 6))
  
for (i in 1:6) {
  mimage(movie[, ,i])
  Sys.sleep(0.2) 
}

# (f)
install.packages("animation")
library(animation)

gif.gen <- function() {
  for (i in 1:6) {
    mimage(movie[ , , i])
  }
}

gif.gen()

saveGIF(gif.gen(),"mygif.gif",outdir="...")




# Question 1.1 (simulation) ------------------------------------------

N = 1000
sample <- rexp(N, rate = 0.5)
sample_est <- density(sample)

hist(sample,
     main = "Histogram of 1000 random samples of exp(1/2)",
     xlab = "",
     prob = TRUE)

curve(dexp(x, rate = 0.5),
      from = 0,
      to   = max(sample),
      add  = TRUE,
      lwd  = 2)

lines(sample_est, lwd = 2, lty = 2)

legend("topright",
       legend = c("Theoretical pdf Exp(1/2)",
                  "Kernel density estimate"),
       lwd = 2,
       lty = c(1, 2),
       bty = "n")





# Question 1.2 (simulation) -------------------------------------------------------------
# (a)
rhuber <- function(N, epsilon = 0, dof = 3) {
  # 1 = Normal, 0 = t
  I <- rbinom(N, size = 1, prob = epsilon)
  
  z <- rnorm(N)         
  t <- rt(N, df = dof)
  
  x <- ifelse(I == 1, z, t)
  return(x)
}

sample1 <- rhuber(100,epsilon = 0.95)
sample2 <- rhuber(100,epsilon = 0.40)
sample3 <- rhuber(100,epsilon = 0.20)

mean(sample1)
mean(sample2)
mean(sample3)

sd(sample1)
sd(sample2)
sd(sample3)

# (b)
par(mfrow=c(3,1))
hist(sample1, prob = TRUE, xlim = c(-6,6), ylim = c(0,0.55), main = "epilon = 0.95")
hist(sample2, prob = TRUE, xlim = c(-6,6), ylim = c(0,0.55), main = "epilon = 0.40")
hist(sample3, prob = TRUE, xlim = c(-6,6), ylim = c(0,0.55), main = "epilon = 0.20")
par(mfrow=c(1,1))

# (c)
df <- matrix(data = c(sample1,sample2,sample3),ncol = 3, byrow = FALSE)
df_cols <- c("e095","e040","e020")
colnames(df) <- df_cols
write.csv(df,file = "huber_samples", row.names = FALSE)


# Question 1.3 (kernel density estimation) --------------------------------

n <- 1000
# (a)
par(mfrow=c(1,1))
sample_norm <- rnorm(n,mean = 2, sd = 1)
plot(density(sample_norm))
rug(sample_norm)
curve(dnorm(x,mean = 2, sd =1), col = "blue", lty = 2, add = TRUE)

# (b)
sample_t <- rt(n, df=3)
plot(density(sample_t))
rug(sample_t)
curve(dt(x,df=3), col = "blue", lty = 2, add = TRUE)

# (c)
sample_exp <- rexp(n, rate=2)
plot(density(sample_exp))
rug(sample_exp)
curve(dexp(x,rate = 2), col = "blue", lty = 2, add = TRUE)



# Question 1.4 (multivariate density estimation) --------------------------

# (a)
faithful <- faithful
library(KernSmooth)

x <- faithful$eruptions
y <- faithful$waiting

kde2d <- bkde2D(
  cbind(x, y),
  bandwidth = c(bw.nrd(x), bw.nrd(y)),   # simple bandwidth choice
  gridsize  = c(50, 50)
)

# Plot the 2D KDE as an image + contours
image(kde2d$x1, kde2d$x2, kde2d$fhat,
      xlab = "Eruption duration (minutes)",
      ylab = "Waiting time to next eruption (minutes)",
      main = "2D Kernel Density Estimate: Old Faithful")
contour(kde2d$x1, kde2d$x2, kde2d$fhat, add = TRUE)
points(x, y, pch = 16, cex = 0.4)   


# another example
data(geyser, package="MASS")
x <- cbind(geyser$duration, geyser$waiting)
est <- bkde2D(x, bandwidth=c(0.7, 7))
contour(est$x1, est$x2, est$fhat)
persp(est$fhat)

# (b)
library(KernSmooth)

x <- faithful$eruptions
y <- faithful$waiting

# Choose a bandwidth for regression
h <- dpill(x, y)   # data-driven bandwidth from KernSmooth

# Local polynomial regression (degree = 1 = local linear)
fit <- locpoly(x, y,
               bandwidth = h,
               degree    = 1)

# Plot data + regression curve
plot(x, y,
     xlab = "Eruption duration (minutes)",
     ylab = "Waiting time to next eruption (minutes)",
     main = "Local Polynomial Regression: Old Faithful",
     pch  = 16, cex = 0.5, col = "grey")

lines(fit$x, fit$y,
      lwd = 2)


# Question 1.5 (multivariate density estimation) --------------------------

# (a)
library(MASS)
n <- 1000
mu    <- c(0, 0)
Sigma <- matrix(c(1, 0.4,
                  0.8, 1), 2, 2)

X  <- mvrnorm(n, mu = mu, Sigma = Sigma)
x  <- X[, 1]
y  <- X[, 2]

# (b)
kde_MASS <- kde2d(x, y, n = 50)  # 50×50 grid

# Scatter + contour
plot(x, y, pch = 16, cex = 0.5, col = "grey",
     xlab = "X1", ylab = "X2",
     main = "kde2d: scatter + contour")
contour(kde_MASS, add = TRUE)

# (c)
K <- function(z) dnorm(z)

product_kde <- function(x, y, gridx, gridy, h1 = 0.5, h2 = 0.5) {
  n  <- length(x)
  nx <- length(gridx)
  ny <- length(gridy)
  
  fhat <- matrix(0, nrow = nx, ncol = ny)
  
  for (ix in seq_len(nx)) {
    for (iy in seq_len(ny)) {
      u1 <- gridx[ix]
      u2 <- gridy[iy]
      
      # product of univariate kernels, averaged over sample
      fhat[ix, iy] <- mean(
        (1 / h1) * K((u1 - x) / h1) *
          (1 / h2) * K((u2 - y) / h2)
      )
    }
  }
  
  list(x = gridx, y = gridy, z = fhat)
}

# Use the same (x,y) grid as kde2d
kde_prod <- product_kde(x, y,
                        gridx = kde_MASS$x,
                        gridy = kde_MASS$y,
                        h1 = 0.5, h2 = 0.5)

# (d)
par(mfrow = c(1, 2))

## kde2d
plot(x, y, pch = 16, cex = 0.5,
     xlab = "X1", ylab = "X2",
     main = "kde2d: scatter + contour")
contour(kde_MASS, add = TRUE)

## product-kernel KDE
plot(x, y, pch = 16, cex = 0.5,
     xlab = "X1", ylab = "X2",
     main = "Product-kernel KDE: scatter + contour")
contour(kde_prod$x, kde_prod$y, kde_prod$z, add = TRUE)

par(mfrow = c(2, 2))

## kde2d, theta = 30
persp(kde_MASS$x, kde_MASS$y, kde_MASS$z,
      theta = 30, phi = 30,
      xlab = "X1", ylab = "X2", zlab = "f(x)",
      main = "kde2d, theta = 30")

## kde2d, theta = -60
persp(kde_MASS$x, kde_MASS$y, kde_MASS$z,
      theta = -60, phi = 30,
      xlab = "X1", ylab = "X2", zlab = "f(x)",
      main = "kde2d, theta = -60")

## product-kernel, theta = 30
persp(kde_prod$x, kde_prod$y, kde_prod$z,
      theta = 30, phi = 30,
      xlab = "X1", ylab = "X2", zlab = "f(x)",
      main = "Product-kernel, theta = 30")

## product-kernel, theta = -60
persp(kde_prod$x, kde_prod$y, kde_prod$z,
      theta = -60, phi = 30,
      xlab = "X1", ylab = "X2", zlab = "f(x)",
      main = "Product-kernel, theta = -60")

par(mfrow=c(1,1))


# Question 2.1 (Monte-Carlo integration) ----------------------------------

M <- 1000000
x <- runif(M, min=2, max=4)
theta.hat <- (4-2)*mean(exp(-x))
theta.hat
# real answer ~ 0.11702


# Question 2.2 (Monte-Carlo integration) ----------------------------------

M <- 10000
G <- 10
x <- seq(0.1, 2.5, length=G)
cdf <- numeric(G)
vmc <- numeric(G) # variance of MC estimator
ciU <- numeric(G) # Upper bound of CI for MC estimator
ciL <- numeric(G) # Lower bound of CI for MC estimator

for(i in 1:G){
  u <- runif(M, min=0, max=x[i])
  g <- exp(-u^2/2)
  # now compute the adjusted sample average: gbar * (x-0)
  # (remember to add the negative part of the integral! = 0.5 bc N(0,1))
  cdf[i] <- x[i] * mean(g) / sqrt(2*pi) + 0.5 
  # compute the associated variance
  vmc[i] <- mean( (g-mean(g))^2 ) / M
  # compute the 95% CI
  ciL[i] <- cdf[i] - 1.96*sqrt(vmc[i])
  ciU[i] <- cdf[i] + 1.96*sqrt(vmc[i])	
}

# Check with theoretic values:
Phi <- pnorm(x)
print( round(rbind(x, cdf, Phi), 3) )

# Display evaluated CI:
print( cbind(round(x,2), round(cbind(ciL, cdf, ciU),4)) )
# Note that something should be done to truncate the CI within (0,1)!


# Question 2.3 (Monte-Carlo Estimation) -----------------------------------

# (a)
trimmed_mean <- function(k,x){
  xs <- sort(x)
  tmean <- mean(xs[(k+1):(length(x)-k)] )
  tmean
}

# (b)

n <- 20
M <- 1000
k <- 5
xbar <- numeric(M)
tmean <- numeric(M)

for(i in 1:M){
  x = rnorm(n)
  xbar[i] = mean(x)
  tmean[i] = trimmed_mean(k,x)
}

rr = range(c(xbar,tmean))
par(mfrow=c(2,1))
hist(xbar, xlim=rr)
hist(tmean, xlim=rr)
par(mfrow=c(1,1))
plot(xbar,tmean)

theta <- 0  # true mean of N(0,1)

# Biases:
mean(xbar-theta)
mean(tmean-theta)
# Variances:
sd(xbar)
sd(tmean)
# MSEs:
mean((xbar-theta)^2)
mean((tmean-theta)^2)

# (c)
n <- 20
M <- 1000
k <- 5
xbar <- numeric(M)
tmean <- numeric(M)

for(i in 1:M){
  x = rt(n, df=1)
  xbar[i] = mean(x)
  tmean[i] = trimmed_mean(k,x)
}

q <- quantile(c(xbar, tmean), c(0.01, 0.99))

dens_xbar  <- density(xbar)
dens_tmean <- density(tmean)
par(mfrow=c(1,1))
# Plot both densities on same axes
plot(dens_xbar,
     xlim = q,
     ylim = range(dens_xbar$y, dens_tmean$y),
     main = "Standard mean vs trimmed mean (t df = 1)",
     xlab = "estimate",
     lwd  = 2)

lines(dens_tmean, col = 2, lwd = 2, lty = 2)

legend("topright",
       legend = c("xbar (ordinary mean)", "trimmed mean"),
       col    = c(1, 2),
       lty    = c(1, 2),
       lwd    = 2,
       bty    = "n")

boxplot(xbar, tmean, names = c("xbar", "trimmed"),
        main = "Spread of estimators (t df = 1)")

theta <- 0  # true mean of N(0,1)

# Biases:
mean(xbar-theta)
mean(tmean-theta)
# Variances:
sd(xbar)
sd(tmean)
# MSEs:
mean((xbar-theta)^2)
mean((tmean-theta)^2)


# Question 2.4 (Monte-Carlo estimation) -----------------------------------

# (a)
N = 50                          # Sample size
M = 100                         # Number of Monte-Carlo repetitions
a = 7                           # Intercept parameter (to be estimated)
b = 3                           # Slope parameter (to be estimated)
x = rep(c(1:5),N/5)             # Vector of regressors (design)
m = 0.5                         # Location for noise distribution
s = 1.2                         # Scale for noise distribution
rseed = 0                       # Random seed
LSvec = RMvec = matrix(0,2,M)   # Storage
MYvec = LSvec                   # More storage
ev = yv = matrix(0,N,M)         # Even more storage



library(MASS)
library(VGAM)

# (b)
for (mc in 1:M) {
  # generate data
  #e = rlaplace(N,location=m,scale=s)
  e = rlnorm(N,meanlog=m,sdlog=s)	
  y = a + b*x + e
  
  # estimate (a,b) via Least Squares estimation
  LS = lm(y~x)$coef
  
  # estimate (a,b) via Least Squares estimation, using our own mylm()
  #MY = c(mylm(x,y)$myintercept,mylm(x,y)$mycoef)
  
  # estimate (a,b) via Robust M-estimation
  RM = rlm(y~x,method="MM")$coef
  
  # store generated data
  yv[,mc] = y
  ev[,mc] = e
  
  # store estimates for this Monte-Carlo experiment
  LSvec[,mc] = rbind(LS[1],LS[2])
  #MYvec[,mc] = rbind(MY[1],MY[2])
  RMvec[,mc] = rbind(RM[1],RM[2])	
}

# (c)
# distributions
par(mfrow=c(2,3))
#
hist(LSvec[1,],main="Estimates of a by LS"); 
hist(RMvec[1,],main="Estimates of a by RM") 
plot(density(LSvec[1,]),col='red'); points(density(RMvec[1,]),t='l',col='blue')
#
hist(LSvec[2,],main="Estimates of b by LS"); 
hist(RMvec[2,],main="Estimates of b by RM") 
plot(density(LSvec[2,]),col='red'); points(density(RMvec[2,]),t='l',col='blue')

# bias: intercept 
mean(LSvec[1,])-a; mean(MYvec[1,])-a; mean(RMvec[1,])-a
# bias: slope
mean(LSvec[2,])-b; mean(MYvec[2,])-b; mean(RMvec[2,])-b
# variance: intercept
var(LSvec[1,]); var(MYvec[1,]); var(RMvec[1,])
# variance: slope
var(LSvec[2,]); var(MYvec[2,]); var(RMvec[2,])
# MSE: intercept (MSE = Bias(estimates)^2 + Var(estimates))
mean((LSvec[1,]-a)^2); mean((MYvec[1,]-a)^2); mean((RMvec[1,]-a)^2)
# MSE: slope
mean((LSvec[2,]-b)^2); mean((MYvec[2,]-b)^2); mean((RMvec[2,]-b)^2)

# (d)
allres = cbind(x,t(MYvec),t(LSvec),t(RMvec))
results = data.frame(allres)
names(results) = c("x","a_MY","b_MY","a_LS","b_LS","a_RM","b_RM")
head(results)
write.csv(results,"montecarlo.csv")

#Test this file

mydata = read.csv("montecarlo.csv")
head(mydata)
# We should get the same stats as before again! 
# Check bias of intercept estimates 
mean(mydata$a_LS)-a; mean(mydata$a_MY)-a; mean(mydata$a_RM)-a
mean(LSvec[1,])-a; mean(MYvec[1,])-a; mean(RMvec[1,])-a


# Question 2.5 (Monte-Carlo estimation) -----------------------------------

# (a)
M <- 1000
N <- 50
Ns <- c(10,20,50,100)
L <- length(Ns)

# initialise the storing vectors
sd1 = sd2 = matrix(NA,nrow=M,ncol=L)
colnames(sd1) = colnames(sd2) = Ns

for (j in 1:L) {
  N <- Ns[j]
  for (i in 1:M){
    x <- rnorm(N, mean = 0, sd = 2)
    sd1[i,j] <- sqrt(sum((x-mean(x))^2)/(N-1))
    sd2[i,j] <- sqrt(sum((x-mean(x))^2)/(N))
  }
}

# Evaluate biases:
# (MC can be used to approximate the bias numerically)
true = 2 # true value of sd
(mean(sd1)-true)
(mean(sd2)-true)
# %-errors give a better idea:
(mean(sd1)-true)/true
(mean(sd2)-true)/true

# Evaluate inaccuracies:
round(sd(sd1),3)
round(sd(sd2),3)
# they seem comparable; the real difference is in terms of bias


# (b)
# sd corresponds to sd1

# (c) & (e)
par(mfrow=c(2,2))
hist(sd1[,1],main="sd1")
hist(sd2[,1],main="sd2")
boxplot(sd1,main="sd1"); abline(h=true)
boxplot(sd2,main="sd2"); abline(h=true)
par(mfrow=c(1,1))


# Question 2.6 (Monte-Carlo Estimation) -----------------------------------

# (a)
M <- 100
n <- 100
ndf <- c(2,4,10)
ms <- matrix(nrow=M,ncol = 3)

for (j in 1:length(ndf)) {
  df <- ndf[j]
  for (i in 1:M) {
    x <- rchisq(n,df)
    ms[i,j] <- mean(x)
  }
}

# (b)
boxplot(ms, names=ndf, xlab="Numbers of degrees of freedom")
abline(h=ndf, lwd=1, col='pink')
hist(ms[,3])

apply(ms,2,sd)


# Question 2.7  -----------------------------------------------------------
# (a)
rhuber <- function(N,epsilon=0,dof=3){
  # Generates N pseudo-random realizations of Huber's contamination model
  # 			f(u) = epsilon g(u) + (1-epsilon) h(u)
  # using the Standard Normal distribution for g(.) and the t-distribution 
  # with 3 degrees of freedom for h().
  # Here epsilon is expected to be within [0.5, 1].
  # to randomly sample from either distribution:
  if(epsilon<0 | epsilon>1){
    stop("epsilon must be a value within 0 and 1.")
  }
  draws = runif(N,0,1)
  # initialise output vector of realisations:
  if(epsilon<1){
    x = numeric(N)
    i.h = which(draws<(1-epsilon))
    x[i.h] = rt(length(i.h),dof)
    x[-i.h] = rnorm((N-length(i.h)))
  } else {
    x = rnorm(N)
  }
  return( x )
}

N <- 100
eps <- c(0.95,0.4,0.2)
H <- matrix(nrow = N, ncol = length(eps))
smeans <- numeric(length(eps))
ssds <- numeric(length(eps))
  
for (i in 1:length(eps)) {
  x <- rhuber(N,epsilon = eps[i], dof=3)
  
  smeans[i] <- mean(x)
  ssds[i] <- sd(x)
  H[,i] <- x
}

smeans
ssds


# (b)
xlims <- range(H)
par(mfrow=c(3,1))
hist(H[,1], col='navy', xlim=xlims, breaks=20)
hist(H[,2], col='navy', xlim=xlims, breaks=20)
hist(H[,3], col='navy', xlim=xlims, breaks=20)

par(mfrow=c(1,1))

# (c)
colnames(H) <- c("e095","e040","e020")
write.csv(H, file = "huber.csv", row.names = FALSE)

# (d)
M <- 500
N <- 100
mean.f = mean.g = sd.f = sd.g = numeric(M)

for (i in 1:M) {
  f <- rhuber(N, epsilon = 0.4, dof = 3)
  g <-  rnorm(N, mean = 0, sd = 1)
  
  mean.f[i] <- mean(f)
  mean.g[i] <- mean(g)
  sd.f[i] <- sd(f)
  sd.g[i] <- sd(g)
}

mean(mean.f)
mean(mean.g)
mean(sd.f)
mean(sd.g)


# Question 2.8 (Bootstrap estimation of standard error) -------------------
library(bootstrap)

data("law")

plot(law, pch=20, cex=2)
abline(lm(law$GPA~law$LSAT), col=3, lwd=4)
cor(law$LSAT,law$GPA)

B = 1000
bcor = numeric(B)
n = nrow(law)
for(b in 1:B){
  ib = sample(1:n, size=n, replace=TRUE)
  xb = law$LSAT[ib]
  yb = law$GPA[ib]	
  bcor[b] = cor(xb,yb)
}
mean(bcor) - cor(law$LSAT,law$GPA)
sd(bcor)
hist(bcor)
quantile(bcor, c(.025,.975))


# Question 2.9 (Bootstrap linear regression estimates) --------------------

# (a)

data("cars")

plot(cars, pch=20)
x = cars$speed
y = cars$dist
lmo = lm(y~x)
lmo$coef  # original coef estimates
abline(lmo, col=3, lwd=3)

# (b)

M <- 10000
bcoef = matrix(NA, nrow=M, ncol=2)
n <- nrow(cars)

for (bs in 1:M) {
  ib = sample(1:n, replace=TRUE)
  xb = x[ib]
  yb = y[ib]
  lmb = lm(yb~xb)
  bcoef[bs,] = coef(lmb)
}

# (c)
par(mfrow=c(1,2))
hist(bcoef[,1], col='pink', main='Intercepts')
hist(bcoef[,2], col='pink', main='Slopes')
par(mfrow=c(1,1))

lmo$coef[1]
mean(bcoef[,1])
mean(bcoef[,1]) - lmo$coef[1]       # bias estimate (intercept)
sd(bcoef[,1])
quantile(bcoef[,1], c(.025, .975))

lmo$coef[2]
mean(bcoef[,2])
mean(bcoef[,2]) - lmo$coef[2]  # bias estimate (slope)
sd(bcoef[,2])
quantile(bcoef[,2], c(.025,.975))

# (d)
plot(bcoef[,1], bcoef[,2],
     xlab = "Intercept",
     ylab = "Slope",
     main = "Bootstrap joint distribution: (Intercept, Slope)",
     pch  = 16, cex = 0.4)
abline(v = lmo$coef[1], h = lmo$coef[2], col = 2, lwd = 2)

cor(bcoef[,1], bcoef[,2])


# Question 2.10 (Nonlinear estimation and bootstrapping) ------------------

data("mtcars")
x <- mtcars$disp
y <- mtcars$mpg

# (a)
plot(x,y,pch=20)
nlso = nls(y~exp(a+b*x), start=list(a=3,b=-.01))

# (i)
coef(nlso)
# (ii)
plot(fitted(nlso), y, pch=20, cex=2) # plot of fitted values
plot(fitted(nlso)- y, pch=20, cex=2) # plot of residuals
hist(fitted(nlso)- y, col='pink') # hist of residuals
#slightly positively skewed residuals
# (iii)
plot(x, y, pch=20, cex=2)
points(x, fitted(nlso), pch=20, cex=2, col="red")
is = order(x)
is
lines(x[is], fitted(nlso)[is], pch=20, cex=2, col="red", lwd=2)

cc = coef(nlso)
exp(cc[1] + cc[2]*x)-fitted(nlso) # check that the model is fitting correctly to formula

# (b)
# locpoly, KernSmooth etc

# (c)
set.seed(1)
B <- 100
n = nrow(mtcars)
ab = bb = numeric(B)
for(b in 1:B){
  ib = sample(1:n,n,replace=TRUE)
  xb = x[ib]
  yb = y[ib]
  nlsb = nls(yb~exp(a+b*xb), start=list(a=3,b=-.01))	
  ab[b] = coef(nlsb)[1]
  bb[b] = coef(nlsb)[2]
}

# (i)
mean(ab)
mean(bb)
sd(ab)
sd(bb)

# (ii)
sd(ab)

par(mfrow=c(1,2))
hist(ab, main='Estimates of A')
hist(bb, main='Estimates of B')
par(mfrow=c(1,1))

# (iii)
round(quantile(ab, c(.025,.975)), 3)

# (iv)
# original estimate is withinn CI




# Question 2.11 (Cross-validation frameworks) -----------------------------

library(MASS)
data("Boston")
x <- Boston[,c("crim","indus","rm","tax")]
y <- Boston$medv

x <- as.matrix(x)
lmo <- lm(y~x)
summary(lmo)

# (a)

n <- length(y)
tsamp <- sample(1:n, size = n/2)
x <- as.data.frame(x)
train <- data.frame(y = y[tsamp], x[tsamp, ])
test  <- data.frame(y = y[-tsamp], x[-tsamp, ])

lm1 <- lm(y ~ ., data = train)

pred1 <- predict(lm1, newdata = test)

rmse1 <- sqrt(mean((pred1 - test$y)^2))
rmse1

# (b)
n <- length(y)
loo_pred <- numeric(n)

for (i in 1:n) {
  train_idx <- setdiff(1:n, i)
  
  train <- data.frame(y = y[train_idx], x[train_idx, ])
  test  <- data.frame(y = y[i], x[i, , drop = FALSE])
  
  fit_i <- lm(y ~ ., data = train)
  loo_pred[i] <- predict(fit_i, newdata = test)
}

# LOO-CV RMSE:
loo_rmse <- sqrt(mean((loo_pred - y)^2))
loo_rmse

# (c)
n <- length(y)
k5_pred <- numeric(n)
K <- 5

# Randomly assign each observation to one of K folds
fold_id <- sample(rep(1:K, length.out = n))

for (k in 1:K) {
  # Test = fold k, Train = all other folds
  test_idx  <- which(fold_id == k)
  train_idx <- setdiff(1:n, test_idx)
  
  train <- data.frame(y = y[train_idx], x[train_idx, ])
  test  <- data.frame(y = y[test_idx],  x[test_idx, ])
  
  fit_k <- lm(y ~ ., data = train)
  
  # Store predictions in the right positions
  k5_pred[test_idx] <- predict(fit_k, newdata = test)
}

# K-fold CV RMSE:
k5_rmse <- sqrt(mean((k5_pred - y)^2))
k5_rmse

# (d)
n <- length(y)
k10_pred <- numeric(n)
K <- 10

# Randomly assign each observation to one of K folds
fold_id <- sample(rep(1:K, length.out = n))

for (k in 1:K) {
  # Test = fold k, Train = all other folds
  test_idx  <- which(fold_id == k)
  train_idx <- setdiff(1:n, test_idx)
  
  train <- data.frame(y = y[train_idx], x[train_idx, ])
  test  <- data.frame(y = y[test_idx],  x[test_idx, ])
  
  fit_k <- lm(y ~ ., data = train)
  
  # Store predictions in the right positions
  k10_pred[test_idx] <- predict(fit_k, newdata = test)
}

# K-fold CV RMSE:
k10_rmse <- sqrt(mean((k10_pred - y)^2))
k10_rmse

# (e)
rmse1; loo_rmse; k5_rmse; k10_rmse



# Question 3.1 (linear regression) ----------------------------------------
# (a) skip

# (b)
data("faithful")

par(mfrow=c(1,1))
plot(faithful$waiting, faithful$eruptions)


lmo <- lm(eruptions ~ waiting, data = faithful)
summary(lmo)
coefs <- lmo$coefficients

abline(a=coefs[1], b=coefs[2], col="red")
names(lmo)
names(summary(lmo))

adj_r2 <- summary(lmo)$adj.r.squared
rsq

# (c)
new80 <- data.frame(waiting = 80)

pred80 <- predict(lmo, newdata = new80, interval = "confidence", level = 0.95)
pred80

newdata <- data.frame(waiting = max(faithful$waiting) + c(1:5))

prednew <- predict(lmo, newdata = newdata, interval = "confidence", level = 0.95)
prednew

par(mfrow=c(1,1))
plot(faithful$waiting, faithful$eruptions, 
     xlim= range(c(faithful$waiting,newdata)),
     ylim = range(c(faithful$eruptions, prednew)),
     pch = 19
     )
abline(a=coefs[1], b=coefs[2], col="blue")
points(newdata$waiting, prednew[,"fit"], col = "red", pch = 19)

# (d)
coefs
summary(lmo)$coefficients[,4] # p values for estimates

par(mfrow = c(1, 2))

## 1. Residuals vs fitted (or vs waiting)
plot(fitted(lmo), resid(lmo),
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs fitted",
     pch  = 20)
abline(h = 0, col = "red", lty = 2)

## 2. QQ-plot of residuals
qqlm <- qqnorm(resid(lmo),
               main = "Normal Q-Q plot of residuals",
               pch  = 20)

# Fit straight line through the QQ points:
qq_fit <- lm(qqlm$y ~ qqlm$x)
abline(qq_fit, col = "red", lwd = 2)
par(mfrow=c(1,1))

# (e)
mylm <- function(x,y){
  # returns my own linear fit ?
  x <- as.numeric(x)
  y <- as.numeric(y)
  
  # centred versions
  xm <- x - mean(x)
  ym <- y - mean(y)
  
  # slope beta: (xm^T xm)^(-1) (xm^T ym)
  # in R: t(xm) %*% ym and t(xm) %*% xm are 1x1 matrices
  b <- as.numeric( (t(xm) %*% ym) / (t(xm) %*% xm) )
  
  # intercept alpha = mean(y) - b * mean(x)
  a <- mean(y) - b * mean(x)
  
  # residuals
  res <- y - (a + b * x)
  
  # return list with 3 components
  out <- list(
    intercept = a,
    slope     = b,
    residuals = res
  )
  
  return(out)
}

lm(eruptions~waiting, data = faithful)
mylm(faithful$waiting,faithful$eruptions)

# (f)
# Improve display...
summary.mylm <- function(out){
  print("")
  print("Coefficients:")
  print(paste("(Intercept)      x",sep=""))
  print(c(out$intercept,out$slope))
}
# Test it!
summary.mylm( mylm(faithful$waiting,faithful$eruptions) )
# Improve (i.e. rewrite) this function to allow for plotting!
mylm <- function(x, y, DOPLOT=FALSE, ...){
  # returns my own linear fit ?
  x <- as.numeric(x)
  y <- as.numeric(y)
  
  # centred versions
  xm <- x - mean(x)
  ym <- y - mean(y)
  
  # slope beta: (xm^T xm)^(-1) (xm^T ym)
  # in R: t(xm) %*% ym and t(xm) %*% xm are 1x1 matrices
  b <- as.numeric( (t(xm) %*% ym) / (t(xm) %*% xm) )
  
  # intercept alpha = mean(y) - b * mean(x)
  a <- mean(y) - b * mean(x)
  
  # residuals
  res <- y - (a + b * x)
  
  # return list with 3 components
  out <- list(
    intercept = a,
    slope     = b,
    residuals = res
  )
  if(DOPLOT){
    plot(x, y, cex=1.2, ...)
    abline(a=a, b=b, col='red', lwd=1.5)
  }
  return(out)
}
# Test again!
mylm(faithful$waiting,faithful$eruptions)
mylm(faithful$waiting,faithful$eruptions,DOPLOT=T)



# Question 3.2 (OLS and WLS) -------------------------------------------

X <- c(1,2,5,5.5,9)
a <- 3
b <- 1.5
N <- 100
x <- rep(X, each = N / length(X))
eps <- rnorm(N,mean = 0, sd= sqrt(1.2))
y <- a + b*x + eps

plot(x,y)
ols_fit <- lm(y ~ x)
summary(ols_fit)
coef(ols_fit)

weights <- c(0.1,0.1,0.35,0.35,0.1)
w <- rep(weights, each = N / length(weights))

wls_fit <- lm(y ~ x, weights = w)
summary(wls_fit)
coef(wls_fit)

plot(x, y, pch = 20, main = "OLS vs WLS",
     xlab = "X", ylab = "Y")
abline(ols_fit, col = "blue", lwd = 2)
abline(wls_fit, col = "red",  lwd = 2)
legend("topleft",
       legend = c("OLS", "WLS"),
       col    = c("blue", "red"),
       lwd    = 2, bty = "n")

ols_rmse <- sqrt(mean(residuals(ols_fit)^2))
wls_rmse <- sqrt(mean(residuals(wls_fit)^2))

c(OLS_RMSE = ols_rmse, WLS_RMSE = wls_rmse)

# Question 3.3 (Polynomial and nonlinear regression) ----------------------

plot(pressure, pch=20, cex=1.5,
     main = "Example: polynomial regression",
     xlab = "Temperature (Celcius)",
     ylab = "Vapor Pressure (ml of mercury)")
x = pressure$temperature
y = pressure$pressure

lin.reg = lm(y~x)
pol.reg2 = lm(y ~ x + I(x^2))
pol.reg3 = lm(y ~ x + I(x^2) + I(x^3))
pol.reg4 = lm(y ~ x + I(x^2) + I(x^3) + I(x^4))
pol.reg5 = lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5))
pol.reg6 = lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6))
lines(x, fitted(pol.reg2), col="blue", lwd=3)
lines(x, fitted(pol.reg3), col="red", lwd=3) # just plot this one for exact question
lines(x, fitted(pol.reg4), col='orange', lwd=3)
lines(x, fitted(pol.reg5), col='green', lwd=3)
lines(x, fitted(pol.reg6), col='purple', lwd=3)

plot(lin.reg$fitted.values,lin.reg$residuals)
plot(pol.reg2$fitted.values,pol.reg2$residuals)
plot(pol.reg3$fitted.values,pol.reg3$residuals,
     main = "Residual plot indicates an ill-suited model",
     xlab = "Fitted values",
     ylab = "Residuals",
     pch=20, cex=1) # fits question
plot(pol.reg4$fitted.values,pol.reg4$residuals)
plot(pol.reg5$fitted.values,pol.reg5$residuals)
plot(pol.reg6$fitted.values,pol.reg6$residuals)


# Question 3.4 (Polynomial and nonlinear regression) -----------------------

par(mfrow = c(1,1), font.lab = 2, font.axis = 2)

model <- function(x, theta){
  theta * x
}

crit <- function(theta, x, y){
  sum( (y - model(x, theta))^2 )   # least squares
}

thbar <- 1.8
x <- rep(c(1, 3, 7, 8), len = 100)
y <- model(x, thbar) + rnorm(length(x))

plot(x, y, pch = 20, cex = 1.5, main = "optim example 1: data")
points(x, model(x, thbar), col = 'green', pch = 20, cex = 2)

optim.out <- optim(par = 1, fn = crit, x = x, y = y,
                   method = "L-BFGS-B",
                   lower  = 0.01,
                   upper  = 3.05)

optim.out


# Question 3.5 (Polynomial and nonlinear regression) ----------------------

## 1. Simulate data from Y = exp(-theta * X) + eps

N      <- 100
theta0 <- 0.8              
sigma  <- 0.2       

x  <- seq(0, 5, length.out = N)         # design points
y0 <- exp(-theta0 * x)                  # true mean curve
y  <- y0 + rnorm(N, mean = 0, sd = sigma)

plot(x, y, pch = 20,
     main = "Simulated data from Y = exp(-θX) + ε",
     xlab = "X", ylab = "Y")
lines(x, y0, col = "darkgreen", lwd = 2)   # true curve
legend("topright", legend = "true curve", col = "darkgreen", lwd = 2, bty = "n")

## 2. Define model and least-squares criterion for optim

# model(theta, x) = exp(-theta * x)
model <- function(theta, x) {
  exp(-theta * x)
}

# sum of squared residuals: sum (y - model(theta, x))^2
crit <- function(theta, x, y) {
  sum( (y - model(theta, x))^2 )
}

## 3. Minimise sum of squares with optim

start_theta <- 0.5   # initial guess

optim.out <- optim(par   = start_theta,
                   fn    = crit,
                   x     = x,
                   y     = y,
                   method = "L-BFGS-B",
                   lower  = 0,          # theta >= 0
                   upper  = 5)

optim.out$par          # estimated theta
optim.out$value        # minimum sum of squares

## 4. Plot fitted curve vs true curve and data

theta_hat <- optim.out$par
y_hat     <- model(theta_hat, x)

plot(x, y, pch = 20,
     main = "OLS fit via optim",
     xlab = "X", ylab = "Y")
lines(x, y0,      col = "darkgreen", lwd = 2)   # true curve
lines(x, y_hat,   col = "red",      lwd = 2)    # fitted curve

legend("topright",
       legend = c("data", "true curve", "fitted curve"),
       col    = c("black", "darkgreen", "red"),
       pch    = c(20, NA, NA),
       lty    = c(NA, 1, 1),
       bty    = "n")


# Question 3.6 (optimisation) ---------------------------------------------

## 1. Simulate data from Y = exp(-theta * X) + eps

N      <- 100
theta0 <- 0.8              
sigma  <- 0.2       

x  <- seq(0, 5, length.out = N)         # design points
y0 <- exp(-theta0 * x)                  # true mean curve
y  <- y0 + rnorm(N, mean = 0, sd = sigma)

plot(x, y, pch = 20,
     main = "Simulated data from Y = exp(-θX) + ε",
     xlab = "X", ylab = "Y")
lines(x, y0, col = "darkgreen", lwd = 2)   # true curve
legend("topright", legend = "true curve", col = "darkgreen", lwd = 2, bty = "n")

## 2. Define model and least-squares criterion for optim

# model(theta, x) = exp(-theta * x)
model <- function(theta, x) {
  exp(-theta * x)
}

# sum of squared residuals: sum (y - model(theta, x))^2
crit <- function(theta, x, y) {
  sum( (y - model(theta, x))^2 )
}

## 3. Minimise sum of squares with optim

start_theta <- 0.5   # initial guess

library(optimx)
optimx.out <- optimx(par   = start_theta,
                   fn    = crit,
                   x     = x,
                   y     = y,
                   method = "L-BFGS-B",
                   lower  = 0,          # theta >= 0
                   upper  = 5)

optimx.out$p1         # estimated theta
optimx.out$value        # minimum sum of squares

## 4. Plot fitted curve vs true curve and data

theta_hatx <- optimx.out$p1
y_hatx     <- model(theta_hatx, x)

plot(x, y, pch = 20,
     main = "OLS fit via optim & optimx",
     xlab = "X", ylab = "Y")
lines(x, y0,      col = "darkgreen", lwd = 2)   # true curve
lines(x, y_hat,   col = "red",      lwd = 2)    # fitted curve using optim
lines(x, y_hatx,   col = "blue",      lwd = 2)  #  fitted curve using optimx

legend("topright",
       legend = c("data", "true curve", "fitted curve (optim)","fitted curve (optimx)" ),
       col    = c("black", "darkgreen", "red", "blue"),
       pch    = c(20, NA, NA, NA),
       lty    = c(NA, 1, 1, 1),
       bty    = "n")






# Question 3.7 (regularisation) -------------------------------------------






# Question 4.1 ------------------------------------------------------------


# QUESTION 4.1
LT = read.table("irl_lifetable_2005.txt", sep=",", header=TRUE)
head(LT)
# keep only the first 106 rows from LT:
SLT = LT[c(1:106),] 
mx = SLT[,8]
x = SLT$Age # age grid
plot(x, log(mx), t='l', lwd=2)
# roughly fit a Makeham model to this data:
onls = nls(mx~A+B*c^x,start=list(A=.0003, B=.00002, c=1.108))
ABc = summary(onls)$coef[,1]
# now add noise to the fitted f.o.m. curve:
set.seed(1)
x = seq(0, 110, by=1)
mx = ABc[1]+ABc[2]*ABc[3]^x
mxn = mx
s1 = which(x<86)
s2 = which(x>85)
mxn[s1] = pmax(0.005,mx[s1]+rnorm(length(s1),0,.03))
mxn[s2] = mx[s2]+rnorm(length(s2),0,.06)
dat = data.frame(x,mx,mxn)

x = dat$x
mxn = dat$mxn
plot(x,mx,pch=21,col=8)
points(x,mxn,pch=20,cex=.8)

# (a) fit NLS:
nls.ns = nls(mxn~A+B*c^x,start=list(A=.0003, B=.00002, c=1.108))
ABc.ns = summary(nls.ns)$coef[,1]
ABc.ns

# (b) smooth the raw data and fit NLS then:
pspl = smooth.spline(x, mxn)
sy = pspl$y
nls.ps = nls(sy~A+B*c^x,start=list(A=.0003, B=.00002, c=1.108))
ABc.ps = summary(nls.ps)$coef[,1]

# compare errors:
cbind(ABc,ABc.ns,ABc.ps)
round(sqrt((ABc-ABc.ns)^2),6)
round(sqrt((ABc-ABc.ps)^2),6)
round(((ABc-ABc.ns)/ABc),4)
round(((ABc-ABc.ps)/ABc),4)


# Question 4.1 (splines) --------------------------------------------------
LT = read.table("irl_lifetable_2005.txt", sep=",", header=TRUE)
# keep only the first 106 rows from LT:
SLT = LT[c(1:106),]
mx = SLT[,8]
x = SLT$Age # age grid
# roughly fit a Makeham model to this data:
onls = nls(mx~A+B*c^x,start=list(A=.0003, B=.00002, c=1.108))
ABc = summary(onls)$coef[,1]
# now add noise to the fitted f.o.m. curve:
set.seed(1)
x = seq(0, 110, by=1)
mx = ABc[1]+ABc[2]*ABc[3]^x
mxn = mx
s1 = which(x<86)
s2 = which(x>85)
mxn[s1] = pmax(0.005,mx[s1]+rnorm(length(s1),0,.03))
mxn[s2] = mx[s2]+rnorm(length(s2),0,.06)
dat = cbind(x,mx,mxn)
dat <- as.data.frame(dat)

# Quick plot of noisy vs true
plot(dat$x, dat$mxn, pch = 20, col = "grey",
     xlab = "Age x", ylab = "Mortality rate",
     main = "Noisy Makeham rates")
lines(dat$x, dat$mx, col = "darkgreen", lwd = 2)
legend("topleft", legend = "true curve", col = "darkgreen", lwd = 2, bty = "n")

# True parameters from the question
theta_true <- c(A = 0.0003, B = 0.00002, c = 1.108)

# (a)
str(dat)

fit_raw <- nls(
  mxn ~ A + B * c^x,
  data  = dat,
  start = list(A = 0.0003, B = 0.00002, c = 1.108)
)

coef_raw <- coef(fit_raw)
coef_raw

se_raw  <- (coef_raw - theta_true)^2   # squared error for each parameter
mse_raw <- mean(se_raw)                # mean squared error over (A,B,c)
se_raw
mse_raw

# (b)
library(mgcv)

# Fit a P-spline smoothing model to the noisy rates
ps_fit <- gam(mxn ~ s(x, bs = "ps"), data = dat)

# Smoothed curve
dat$mx_ps <- fitted(ps_fit)

# Plot to see smoothing effect
plot(dat$x, dat$mxn, col = "grey", pch = 20,
     xlab = "Age x", ylab = "Mortality rate",
     main = "Noisy, true, and P-spline smoothed Makeham rates")
lines(dat$x, dat$mx,    col = "darkgreen", lwd = 2)  # true
lines(dat$x, dat$mx_ps, col = "blue",      lwd = 2)  # smoothed
legend("topleft",
       legend = c("noisy", "true", "P-spline"),
       col    = c("grey", "darkgreen", "blue"),
       pch    = c(20, NA, NA),
       lty    = c(NA, 1, 1),
       bty    = "n")

fit_ps <- nls(
  mx_ps ~ A + B * c^x,
  data  = dat,
  start = list(A = 0.0003, B = 0.00002, c = 1.108)
)

coef_ps <- coef(fit_ps)
coef_ps

# Parameter MSE after smoothing
se_ps  <- (coef_ps - theta_true)^2
mse_ps <- mean(se_ps)
se_ps
mse_ps

mse_raw; mse_ps



# Question 4.2 (splines) --------------------------------------------------

dat = read.csv("insdata.csv")
age = dat$Age
mF = dat$mF
plot(age,mF,t='b')
plot(age,log(mF),t='b')

# (a)
p1 = smooth.spline(age, mF, cv=TRUE)
plot(age, mF, pch=20, t='b')
lines(p1, col=2, lwd=3)

# (b)
par = p1$spar
p2 = smooth.spline(age, mF, spar=par/2)
lines(p2, col=4, lwd=3)

# (c)
# ?smooth.spline
p1$x-p2$x
plot(p1$x, p2$x); abline(a=0, b=1)

# (d) NB: we need to check that the P-spline data is ordered 
# the same way as the original data; it's the case here 
# because the original sample was already ordered, but this 
# is not always true...
p1$x-age 

mse1 = mean( (mF-p1$y)^2 )
mse2 = mean( (mF-p2$y)^2 )
c(mse1, mse2)
sqrt(c(mse1, mse2)) # maybe easier to compare RMSEs instead?
# Explain the difference?
c(sd(p1$y-mF), sd(p2$y-mF))
c(mean(p1$y-mF), mean(p2$y-mF))
# can we explain? (hint: it has to do with the data...)

# (e)
knots = quantile(age, c(.25,.50,.75))
BM = bs(age, knots=knots)
matplot(age,BM,xlab="Age",ylab="Spline basis",t='b')
attributes(BM)
knots
# Note: df = length(knots) + degree... cf. notes!!

# (f) coordinates of age 60 on B-spline basis:
round(BM[which(age==60),],4)
matplot(age,BM,xlab="Age",ylab="Spline basis",t='b')
abline(v=60, lty=3)
abline(h=BM[which(age==60),], lty=2)

# (g) corresponding B-spline for (age, mF) data
bsp = lm(mF~BM)
coef(bsp)
summary(bsp)

# (h) 
mseb = mean((fitted(bsp)-mF)^2)
sqrt(c(mse1, mse2, mseb))

plot(age,mF,t='b',pch=20,main="Crude force of mortality (Females)",
     xlab="Age (years)",ylab="Force of mortality",cex=1.2)
points(p1,col=2,t='l',pch='x',lwd=2)
points(age,fitted(bsp),col=3,t='l',pch='x',lwd=4)
points(p2,col=4,t='l',pch='x',lwd=2)

# This B-spline seems like a compromise between the 2 P-splines

# (i) 
# first, see that we're missing some ages:
plot(age)
# interpolation grid:
all.ages = seq(min(age),max(age),by=1)
plot(all.ages)

# - P-spline
sp1 = approx(p1,xout=all.ages)
sum((sp1$y)^2)
sd(sp1$y)

# - lowess
lo = loess(mF~age)
lpred = predict(lo,newdata=data.frame(age=all.ages))
sd(lpred)

plot(age,mF,t='b',pch=20,main="Crude force of mortality (Females)",
     xlab="Age (years)",ylab="Force of mortality",cex=1.2)
points(all.ages,sp1$y,col=2,t='b',pch='x',lwd=2)
points(all.ages,lpred,col=4,t='b',pch=20,lwd=2)
points(age,mF,pch=20)



# Question 4.3 ------------------------------------------------------------

library(splines)
library(car)

head(Prestige)
x = Prestige$income
y = Prestige$prestige

plot(x, y, pch=20)

# polynomial regression (for comparison)
inc.100 <- seq(min(x), max(x), len=100)
mod.lo.inc <- loess(y ~ x, span=.7, degree=1) 
names(mod.lo.inc)
points(mod.lo.inc$x, mod.lo.inc$fitted, pch=15, col=2)

# show the location of the "new points" on graph:
rug(inc.100) 

# generate predictions (note that we need to call the 
# points "x" in the data.frame of new evaluation points,
# so as to match the name of the predictor variable in 
# the loess() call that yielded mod.lo.inc)
pres <- predict(mod.lo.inc, newdata=data.frame(x=inc.100)) 

# display these points
points(inc.100, pres, pch=20, cex=.8, col=4)

# Compute a P-spline
ssp <- smooth.spline(x, y)

# Compute a B-spline
B = bs(x, df=6)
sbp <- lm(y~B)$fitted

# plot(x, y, pch=20)
lines(ssp, lwd=3, col=4)
reorder = order(x)
points(x[reorder], sbp[reorder], pch=20, col=2, t='b', lwd=4)
reorder = order(mod.lo.inc$x)
lines(mod.lo.inc$x[reorder], mod.lo.inc$fitted[reorder], 
      lwd=3, col=8)

legend("bottomright", pch=c(20), col=c(1,2,3,8), bty='n',
       legend=c("data","P-spline","B-spline",
                "polynomial regression"))

plot(x, y, pch=20)
lines(ssp, lwd=3, col=4)
reorder = order(x)
points(x[reorder], sbp[reorder], pch=20, col=2, t='b', lwd=4)

# Now practice calculating the MSEs for these curves...
length(x)
length(y)
length(sbp)
length(ssp$y) # not the original N!!!

plot(sbp, y)
mse.b = mean( (sbp-y)^2 )

# For the P-spline, we need to interpolate!
spp = approx(ssp$x, ssp$y, xout=x, rule=2)$y
mse.p = mean( (spp-y)^2 )

c(mse.b, mse.p)
