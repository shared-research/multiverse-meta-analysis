
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Multiverse meta-analysis

This project aim to implement a framework for valid statistical
inference for a multiverse analysis. The method is based on a
multivariate permutation testing framework.

This is a work in progress project with the aim to develop the
`joinmeta` package to perform a multiverse meta-analysis. Now the
project depends on:

``` r
pckgs <- c("metafor", "flip")
install.packages(pckgs)
```

Furthermore there is the temporary need to install the
[`jointest`](https://github.com/livioivil/jointest) package available on
Github:

``` r
devtools::install_github("livioivil/jointest")
```

# Minimal example

To perform a multiverse meta-analysis you can clone this repository and
open the R Project. Then we can load the relevant packages:

``` r
devtools::load_all() # to load all functions in the projects
library(metafor)
library(Matrix)
library(MASS)
library(jointest)
library(flip)
```

Then we can simulate a multiverse matrix. In real applications the
matrix is the result of fitting different models on the same dataset
creating several different scenarios.

``` r
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

# Effect sizes
yi <- rnorm(k, rnorm(k, theta, sqrt(tau2)), sqrt(1/n + 1/n))

# Sampling variances
vi <- (rchisq(k, n + n - 2) / (n + n - 2)) * (1/n + 1/n)

# this is a single hypothetical meta-analysis
fit <- rma(yi, vi, method = "REML")

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

# fitl is the list of fitted models to each multiverse scenario. this is usually the starting point for the multiverse analysis.
```

Now we can combine the multiverse obtaining the overall p-value and the
post-hoc corrected p-values for each scenario.

``` r
# permutations of each scenario
multi <- multiverse(fitl)

multi_comb <- jointest::combine(multi)

# overall pvalue
summary(multi_comb)
#>   Coeff Stat nMods        S     p
#> 1     Y maxT   162 7.233146 4e-04

# adjusting each p-value using maxT
res <- p_adjust_fwer(multi, method = "maxT")
head(summary(res))
#>         b       est         se        z       p_meta     ci_lb     ci_ub t_flip
#> 1 intrcpt 0.3512770 0.09341330 3.760461 1.696006e-04 0.1681903 0.5343637 3.7936
#> 2 intrcpt 0.3924156 0.10767338 3.644500 2.679123e-04 0.1813797 0.6034516 3.6606
#> 3 intrcpt 0.4081365 0.10062835 4.055880 4.994587e-05 0.2109086 0.6053644 4.0932
#> 4 intrcpt 0.3096348 0.10557849 2.932745 3.359797e-03 0.1027047 0.5165648 2.9118
#> 5 intrcpt 0.3782149 0.09383529 4.030625 5.562865e-05 0.1943011 0.5621287 4.0398
#> 6 intrcpt 0.4701441 0.10484802 4.484053 7.323837e-06 0.2646458 0.6756425 4.4841
#>   tail_flip p_flip  p.adj
#> 1        >< 0.0008 0.0152
#> 2        >< 0.0004 0.0188
#> 3        >< 0.0008 0.0080
#> 4        >< 0.0048 0.0688
#> 5        >< 0.0004 0.0084
#> 6        >< 0.0004 0.0040
```

# Conferences

- Poster at [SIPS 2023](https://www.improvingpsych.org/SIPS2023/):
  [HTML](conferences/sips2023/poster/sips-2023.html),
  [PDF](conferences/sips2023/poster/sips-2023.pdf)

# Suggested References

\[1\] L. Finos, with contributions by Florian Klinglmueller, D. Basso,
et al. *flip: Multivariate Permutation Tests*. R package version 2.5.0.
2018. <https://CRAN.R-project.org/package=flip>.

\[2\] P. Girardi, A. Vesely, D. Lakens, et al. *Post-selection Inference
in Multiverse Analysis (PIMA): an inferential framework based on the
sign flipping score test*. 2022. arXiv: 2210.02794 \[stat.ME\].

\[3\] J. J. Goeman and A. Solari. “Multiple hypothesis testing in
genomics”. En. In: *Stat. Med.* 33.11 (May. 2014), pp. 1946-1978. ISSN:
0277-6715,1097-0258. DOI: 10.1002/sim.6082.

\[4\] J. Hemerik, J. J. Goeman, and L. Finos. “Robust testing in
generalized linear models by sign flipping score contributions”. En. In:
*J. R. Stat. Soc. Series B Stat. Methodol.* 82.3 (Jul. 2020), pp.
841-864. ISSN: 1369-7412,1467-9868. DOI: 10.1111/rssb.12369.

\[5\] S. Steegen, F. Tuerlinckx, A. Gelman, et al. “Increasing
Transparency Through a Multiverse Analysis”. En. In: *Perspect. Psychol.
Sci.* 11.5 (Sep. 2016), pp. 702-712. ISSN: 1745-6916,1745-6924. DOI:
10.1177/1745691616658637.

\[6\] P. H. Westfall and S. Stanley Young. *Resampling-Based Multiple
Testing: Examples and Methods for p-Value Adjustment*. En. John Wiley &
Sons, Jan. 1993. ISBN: 9780471557616.
