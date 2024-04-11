
# yodel

<!-- badges: start -->

[![check-standard](https://github.com/rich-payne/yodel/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/rich-payne/yodel/actions/workflows/check-standard.yaml)
[![covr](https://github.com/rich-payne/yodel/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/rich-payne/yodel/actions/workflows/test-coverage.yaml)
[![lint](https://github.com/rich-payne/yodel/actions/workflows/lint.yaml/badge.svg)](https://github.com/rich-payne/yodel/actions/workflows/lint.yaml)
<!-- badges: end -->

The goal of yodel is to provide a general framework to do baYesian mODEL
averaging. Models are fit separately and model weights are then
calculated based on the log posterior predictive density of the observed
data. See Ando & Tsay (2010) and Gould (2019) for references.

## Example

We will begin by doing Bayesian model averaging of some simple linear
regression. We will generate data, and the analyze it separately with a
linear and quadratic Bayesian model.

``` r
library(rjags)
#> Loading required package: coda
#> Linked to JAGS 4.3.0
#> Loaded modules: basemod,bugs
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
linear_jags <- "
  model {
    for(i in 1:n) {
      y[i] ~ dnorm(b1 + b2 * z[i], tau2)
      y_log_density[i] <- log(dnorm(y[i], b1 + b2 * z[i], tau2))
    }
    b1 ~ dnorm(0, .001)
    b2 ~ dnorm(0, .001)
    tau2 ~ dgamma(1, .001)
  }
"

quad_jags <- "
  model {
    for(i in 1:n) {
      y[i] ~ dnorm(b1 + b2 * z[i]+ b3 * z[i] * z[i], tau2)
      y_log_density[i] <- log(dnorm(y[i], b1 + b2 * z[i] + b3 * z[i] * z[i], tau2))
    }
    b1 ~ dnorm(0, .001)
    b2 ~ dnorm(0, .001)
    b3 ~ dnorm(0, .001)
    tau2 ~ dgamma(1, .001)
  }
"

set.seed(85)
n <- 100
z <- runif(n, 0, 10)
b1 <- 2
b2 <- 1.5
b3 <- .01
y <- b1 + b2 * z + b3 * z ^ 2 + rnorm(n, 0, .75)

jags_data <- list(z = z, y = y, n = n)

jmod_linear <- jags.model(
  file = textConnection(linear_jags),
  data = jags_data,
  n.chains = 2
)
#> Compiling model graph
#>    Resolving undeclared variables
#>    Allocating nodes
#> Graph information:
#>    Observed stochastic nodes: 100
#>    Unobserved stochastic nodes: 3
#>    Total graph size: 607
#> 
#> Initializing model
samples_linear <- coda.samples(
  jmod_linear,
  variable.names = c("b1", "b2", "tau2", "y_log_density"),
  n.iter = 1e3
)

jmod_quad <- jags.model(
  file = textConnection(quad_jags),
  data = jags_data,
  n.chains = 2
)
#> Compiling model graph
#>    Resolving undeclared variables
#>    Allocating nodes
#> Graph information:
#>    Observed stochastic nodes: 100
#>    Unobserved stochastic nodes: 4
#>    Total graph size: 708
#> 
#> Initializing model
samples_quad <- coda.samples(
  jmod_quad,
  variable.names = c("b1", "b2", "b3", "tau2", "y_log_density"),
  n.iter = 1e3
)
```

To calculate the posterior weight of each model, we need to calculate
the log-likelihood of each observation for each iteration of the MCMC.
The jags code above already calculated it (“y_log_density”), so we’ll
put it into a matrix form. We also want the MCMC samples in a
dataframe/tibble.

``` r
mcmc_linear <- as_tibble(as.matrix(samples_linear))
log_post_pred_linear <- mcmc_linear %>%
  dplyr::select(contains("y_log_density")) %>%
  as.matrix()

mcmc_quad <- as_tibble(as.matrix(samples_quad))
log_post_pred_quad <- mcmc_quad %>%
  dplyr::select(contains("y_log_density")) %>%
  as.matrix()
```

To calculate the Bayesian model averaging of a quantity of interest (in
our case, the mean at a particular value of z), we need to define
functions which calculate the mean at a given value of z for each
iteration of the MCMC. The functions for calculating a posterior
quantity of interest must take the MCMC samples as the first argument,
and output a dataframe with a column named “iter” which identifies the
MCMC iteration. There is no restriction on the number of rows or columns
of the output of the function, which provides flexibility for a number
of modeling scenarios. There is also no restriction on the number of
arguments, so long as the first argument is the MCMC samples.

``` r
linear_fun <- function(mcmc, z) {
  data.frame(
    iter = 1:nrow(mcmc),
    z = z,
    mean = mcmc$b1 + mcmc$b2 * z
  )
}

quad_fun <- function(mcmc, z) {
  data.frame(
    iter = 1:nrow(mcmc),
    z = z,
    mean = mcmc$b1 + mcmc$b2 * z + mcmc$b3 * z ^ 2
  )
}
```

The following code calculates the posterior weights for each model. Note
that the `mcmc` and `fun` arguments are optional if weights are all that
are desired. If obtaining posterior samples for a quantity of interest
are wanted (using the `posterior()` function), then then these arguments
must be specified.

``` r
library(yodel)
bma_fit <- bma(
  linear = model_bma_predictive(
    log_post_pred = log_post_pred_linear,
    adjustment = - 3 / 2,
    w_prior = .5,
    mcmc = mcmc_linear,
    fun = linear_fun
  ),
  quad = model_bma_predictive(
    log_post_pred = log_post_pred_quad,
    adjustment = - 4 / 2,
    w_prior = .5,
    mcmc = mcmc_quad,
    fun = quad_fun
  )
)
```

The `bma()` function returns the prior weights, posterior weights,
models (lists from `model_bma()`) and a model index of which model is to
be used on which iteration.

``` r
names(bma_fit)
#> [1] "w_prior" "w_post"  "models"  "seed"
bma_fit$w_prior
#> linear   quad 
#>    0.5    0.5
bma_fit$w_post
#>    linear      quad 
#> 0.2886865 0.7113135
```

The `posterior()` function will provide posterior samples for a quantity
of interest. In our case, this is the posterior mean at a particular
value of z. The posterior function takes the output from `bma()` as well
as other arguments which are needed for the calculation (specified in
the `fun` arguments of `model_bma()`).

The output provides the estimated mean for each iteration of the MCMC,
and also specified which model the estimate came from.

``` r
bma_fit$w_post
#>    linear      quad 
#> 0.2886865 0.7113135
post <- posterior(bma_fit, z = 5)
head(post)
#>   iter z      mean  model
#> 1    1 5 12.377548   quad
#> 2    2 5 11.643794   quad
#> 3    3 5 10.602492   quad
#> 4    4 5  9.961733   quad
#> 5    5 5  9.805239   quad
#> 6    6 5 10.051760 linear
```

Once posterior samples are obtained, the user can then calculate
statistics of interest, e.g., the posterior mean and credible intervals.

``` r
post %>%
  group_by(z) %>%
  summarize(
    lb = quantile(mean, .025),
    ub = quantile(mean, .975),
    mean = mean(mean),
    .groups = "drop"
  )
#> # A tibble: 1 × 4
#>       z    lb    ub  mean
#>   <dbl> <dbl> <dbl> <dbl>
#> 1     5  9.38  9.89  9.66
```

## Installation

``` r
install.packages("yodel")
```

## Reference

Ando, T., & Tsay, R. (2010). Predictive likelihood for Bayesian model
selection and averaging. International Journal of Forecasting, 26(4),
744-763.

Gould, A. L. (2019). BMA‐Mod: A Bayesian model averaging strategy for
determining dose‐response relationships in the presence of model
uncertainty. Biometrical Journal, 61(5), 1141-1159.
