## ------------------------------------------------------------------------
## Script: Simulating a Fake Multiverse
## ------------------------------------------------------------------------

# This script simulate a multiverse meta-analysis. The simulation code
# create a multiverse with only plausible (i.e., within a plausible range)
# values but without empirical meaning.

# Packages ----------------------------------------------------------------

library(metafor)
library(Matrix)
library(MASS)
library(tidyverse)
library(cowplot)
library(jointest)

# Parameters --------------------------------------------------------------

# seed for the simulation
set.seed(2080)

# Multiverse
ns <-  162 # number of multiverse scenarios
nc <- ns^2/2 - ns/2 # number of correlations

# Meta-analysis
k <- 30 # number of studies
theta <- 0.4 # real effect size
tau2 <- 0.2 # real heterogeneity
n <- 30 # sample size for each group per study

# Simulating --------------------------------------------------------------

# Effect sizes
yi <- rnorm(k, rnorm(k, theta, sqrt(tau2)), sqrt(1/n + 1/n))

# Sampling variances
vi <- (rchisq(k, n + n - 2) / (n + n - 2)) * (1/n + 1/n)

# Model fitting -----------------------------------------------------------

# this is a single hypothetical meta-analysis
fit <- rma(yi, vi, method = "REML")

# Simulating the multiverse -----------------------------------------------

# Now we simulate some variability in effect sizes and sampling variances
# due to different analytically approach to the meta-analysis. For example
# guessing a missing correlation, removing a study or choosing a specific
# meta-analysis model

b <- fit$b # average effect
se <- fit$se # standard error

bs <- runif(ns, b - b*0.5, b + b*0.5) # some variability in the average effect
rs <- runif(nc, 0.6, 1) # correlations between scenarios
ses <- runif(ns, se - 0.25*se, se + 0.25*se) # some variability in the standard error

# correlation matrix of the multiverse
R <- matrix(NA, ns, ns)
R[upper.tri(R)] <- rs
R[lower.tri(R)] <- rs
diag(R) <- 1

# variance-covariance matrix of the multiverse
V <- diag(sqrt(ses^2 + tau2)) %*% R %*% diag(sqrt(ses^2 + tau2))
V <- as.matrix(Matrix::nearPD(V)$mat) # make positive definite

# generating observed effects across the multiverse
X <- MASS::mvrnorm(k, bs, V)

# fitting a meta-analysis model for each scenario

fitl <- vector(mode = "list", length = ns)

for(i in 1:ns){
  # some variability in the sampling variances
  vis <- runif(k, vi - 0.25*vi, vi + 0.25*vi)
  fitl[[i]] <- rma(X[, i], vis)
}

# Multiverse analysis -----------------------------------------------------

# permutations of each scenario
multi <- multiverse(fitl)

multi_comb <- jointest::combine(multi)
summary(multi_comb)

# adjusting each p-value using maxT
res <- p_adjust_fwer(multi, method = "maxT")
res_sum <- summary(res)

multiverse <- list(res_sum = res_sum,
                   multi_comb = multi_comb,
                   multi_sum = multi_sum,
                   X = X)

saveRDS(multiverse, "conferences/sips2023/poster/objects/poster.rds")
