###############################################################################
## ST4060, 2024_2025_Q4
###############################################################################
data(mtcars)

x <- mtcars # feature set
x$mpg <- NULL # remove response from features
y <- mtcars$mpg # response


###############################################################################
# a). Scaled PCA on x
###############################################################################
pca <- prcomp(x, scale. = TRUE)

# Rotated data (image of x under PCA)
z <- as.matrix(pca$x)

# Proportion of variance explained
sdev2 <- pca$sdev^2
prop_var <- sdev2 / sum(sdev2)

# PCs with > 5% of total variance
pc_over_5 <- which(prop_var > 0.05)
pc_over_5
prop_var[pc_over_5]


###############################################################################
# b). SD of first three PCs of z
###############################################################################
apply(z, 2, sd)[1:3]



###############################################################################
# c). Linear regressions on original x and on z
###############################################################################
fit1 <- lm(y ~ ., data = x)
fit2 <- lm(y ~ ., data = as.data.frame(z))

# Sums of squared residuals
ssr1 <- sum(residuals(fit1)^2)
ssr2 <- sum(residuals(fit2)^2)

# Adjusted R^2
adjR2_1 <- summary(fit1)$adj.r.squared
adjR2_2 <- summary(fit2)$adj.r.squared

ssr1; ssr2
adjR2_1; adjR2_2
# For both models we obtain the same sum of squared residuals (≈147.49) and adjusted R^2 (≈0.807).
# This is because z is just an orthogonal rotation of x and we regress y on all PCs.
# The column space of the design matrix is unchanged, so the linear fit, residuals and goodness-of-fit measures are identical.



###############################################################################
# d). Critique of PCA on mtcars (conceptual, no code)
###############################################################################
# Several predictors in mtcars (e.g. cyl, gear, carb, vs, am) are discrete/categorical counts,
# so treating them as continuous in PCA is questionable.
# The Euclidean geometry underlying PCA then depends on arbitrary numeric codings of categories,
# making the principal components harder to interpret and potentially misleading.



###############################################################################
# e). k-means clustering of unscaled x with 3 clusters
###############################################################################
set.seed(123)
km3 <- kmeans(x, centers = 3, nstart = 50)

# Cluster labels for each car
km3$cluster

# Variable means by cluster (to inspect what drives clustering)
aggregate(x, by = list(cluster = km3$cluster), FUN = mean)
# Interpretation of k-means clustering (k = 3, unscaled x)
# The clusters are mainly driven by engine / power variables:
#   - cyl (number of cylinders)
#   - disp (displacement)
#   - hp   (horsepower)
# with wt (vehicle weight) also aligned with these.
# Cluster 2 has very high cyl/disp/hp (large, powerful cars),
# Cluster 3 has low cyl/disp/hp (small 4-cylinder cars),
# Cluster 1 lies in between. Thus k-means is essentially grouping cars
# by powertrain size and performance level.