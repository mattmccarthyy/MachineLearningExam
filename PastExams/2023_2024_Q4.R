###############################################################################
## ST4060, 2023_2024_Q4
###############################################################################
library(MASS)

x <- Pima.tr
x$type <- NULL # feature matrix
y <- Pima.tr$type # response (Yes / No)


###############################################################################
# a). Is this a regression or classification problem?
###############################################################################
# y contains diabetes status ("Yes"/"No"), i.e. a categorical / binary outcome.
# Predicting y from the numeric measurements in x is therefore a
# CLASSIFICATION problem, not regression.


###############################################################################
# b). k-means clustering of x into k = 2 clusters
###############################################################################
set.seed(4060)

k <- 2
km2 <- kmeans(x, centers = k, nstart = 50)

# (b)(i) Confusion matrix between cluster labels and true classes y
table(cluster = km2$cluster, class = y)

# (b)(ii) Scatterplot of x[,1:2] with points coloured by cluster
{
  plot(x[, 1:2],
       col = c("black", "red")[km2$cluster], # black/red by cluster
       pch = 20, # filled circles
       xlab = colnames(x)[1],
       ylab = colnames(x)[2],
       main = "Pima.tr: k-means clusters (k = 2) on first two features")
  
  legend("topright",
         legend = paste("Cluster", 1:2),
         col    = c("black", "red"),
         pch    = 20,
         bty    = "n")
}



###############################################################################
# c). Comment on spatial distribution and cluster membership
###############################################################################
# From the scatterplot (npreg vs glu), the two clusters separate mainly in the
# vertical direction: red points (cluster 2) tend to have higher glucose (glu),
# while black points (cluster 1) have lower glu values.
# There is substantial overlap in npreg (horizontal direction), so number of
# pregnancies does not drive the clustering as strongly as glucose.
# This suggests k-means has primarily partitioned the data into "low–glu" and
# "high–glu" groups, which we would expect to be related to diabetes status.



###############################################################################
# d). Scaled PCA on x: number of PCs capturing 90% of the information
###############################################################################
pca_sc <- prcomp(x, scale. = TRUE)

# Proportion of variance explained by each PC
sdev2_sc <- pca_sc$sdev^2
prop_sc <- sdev2_sc / sum(sdev2_sc)
cumprop_sc <- cumsum(prop_sc)

pca_summary_sc <- data.frame(
  PC = seq_along(prop_sc),
  PropVar = prop_sc,
  CumProp = cumprop_sc
)
pca_summary_sc

# Smallest number of PCs needed to reach 90% cumulative variance:
num_pc_90 <- which(cumprop_sc >= 0.90)[1]
num_pc_90
# In the exam: quote num_pc_90 and justify by saying that the cumulative
# proportion of variance from scaled PCA first exceeds 90% at that PC.
# 5, the cumulative proportion of variance from scaled PCA first exceeds 90% at this PC.


###############################################################################
# e). Unscaled PCA on x: variables mainly influencing first two PCs
###############################################################################
pca_un <- prcomp(x, scale. = FALSE)
loadings_12 <- pca_un$rotation[, 1:2]
loadings_12
# Variables with the largest absolute loadings in each column
# are the ones that mainly influence PC1 and PC2:
apply(abs(loadings_12), 2, function(z) names(z)[order(z, decreasing = TRUE)])

# In the exam:
# - State which variables have the largest |loading| on PC1 and on PC2.
# - Justify by noting that in PCA, the contribution of each original
#   feature to a component is given by its loading; large |loadings|
#   mean that feature is most influential for that principal component.
# - Because PCA here is UNscaled (covariance-based), variables with
#   larger raw variance tend to dominate the first components.

# In this specific case. 
# For PC1, the largest absolute loadings are for:
#   glu, age, and bp  (then skin, bmi, ...)
# so PC1 is mainly a contrast driven by glucose level, age and blood pressure.
#
# For PC2, the largest absolute loadings are for:
#   skin, bp, age, and bmi
# so PC2 is mainly influenced by skin thickness and blood pressure, with
# additional contribution from age and BMI.
#
# Justification:
# In PCA, the contribution of each original variable to a principal component
# is measured by its loading; variables with the largest absolute loadings are
# the most influential on that component. Because this PCA is UNscaled, PCs
# are also dominated by variables with larger raw variance (e.g. glu, skin).
