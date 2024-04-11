# Minimal example
fit <- bma(
  linear = model_bma_predictive(
    # mcmc = data.frame(b1 = 1:5, b2 = 11:15, sigma = seq(.1, .5, .1)),
    log_post_pred = matrix(log(1:100), 5, 20),
    adjustment = - 3 / 2,
    w_prior = .5
  ),
  quad = model_bma_predictive(
    # mcmc = data.frame(b1 = 1:5 / 2, b2 = 11:15 / 2, b3 = 5:1, sigma = seq(.1, .5, .1)),
    log_post_pred = matrix(log(2:101), 5, 20),
    adjustment = - 4 / 2,
    w_prior = .5
  )
)

fit$w_prior
fit$w_post
