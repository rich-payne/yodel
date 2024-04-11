# functions which caclulate the dose response for a linear and quadratic model
fun_linear <- function(x, dose) {
  mean_response <- x$b1 + x$b2 * dose
  data.frame(iter = 1:nrow(x), dose = dose, mean = mean_response)
}
fun_quad <- function(x, dose) {
  mean_response <- x$b1 + x$b2 * dose + x$b3 * dose ^ 2
  data.frame(iter = 1:nrow(x), dose = dose, mean = mean_response)
}

# Bayesian model averaging
fit <- bma(
  linear = model_bma_predictive(
    mcmc = data.frame(b1 = 1:5, b2 = 11:15, sigma = seq(.1, .5, .1)),
    log_post_pred = matrix(log(1:100), 5, 20),
    adjustment = - 3 / 2,
    w_prior = .5,
    fun = fun_linear
  ),
  quad = model_bma_predictive(
    mcmc = data.frame(b1 = 1:5 / 2, b2 = 11:15 / 2, b3 = 5:1, sigma = seq(.1, .5, .1)),
    log_post_pred = matrix(log(2:101), 5, 20),
    adjustment = - 4 / 2,
    w_prior = .5,
    fun = fun_quad
  )
)

# posterior samples using Bayesian model averaging
posterior(fit, dose = 1)
posterior(fit, dose = 2)
