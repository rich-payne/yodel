# preliminaries
b1 <- 1:5
b2 <- 11:15
sigma <- seq(.1, .5, .1)
log_post_pred <- matrix(log(1:100), 5, 20)
w_prior <- .8

b1_quad <- 1:5 / 2
b2_quad <- 11:15 / 2
b3_quad <- 5:1
sigma_quad <- seq(.1, .5, .1) / 2
log_post_pred_quad <- matrix(log(2:101), 5, 20)

fit <- bma(
  linear = model_bma_predictive(
    mcmc = data.frame(b1, b2, sigma),
    log_post_pred = log_post_pred,
    adjustment = - 3 / 2,
    w_prior = w_prior
  ),
  quad = model_bma_predictive(
    mcmc = data.frame(
      b1 = b1_quad,
      b2 = b2_quad,
      b3 = b3_quad,
      sigma = sigma_quad
    ),
    log_post_pred = log_post_pred_quad,
    adjustment =  - 4 / 2,
    w_prior = 1 - w_prior
  )
)

test_that("bma weights", {
  numerator <- exp(
    c(
      sum(log(apply(exp(log_post_pred), 2, mean)))  - 3 / 2,
      sum(log(apply(exp(log_post_pred_quad), 2, mean))) - 4 / 2
    )
  ) * c(w_prior, 1 - w_prior)
  w_post <- numerator / sum(numerator)
  expect_equal(unname(fit$w_post), w_post)
  expect_equal(unname(fit$w_prior), c(w_prior, 1 - w_prior))
})

test_that("model index", {
  fit2 <- bma(
    linear = model_bma_predictive(
      mcmc = data.frame(b1, b2, sigma),
      log_post_pred = log_post_pred,
      adjustment = - 3,
      w_prior = 0
    ),
    quad = model_bma_predictive(
      mcmc = data.frame(
        b1 = b1_quad,
        b2 = b2_quad,
        b3 = b3_quad,
        sigma = sigma_quad
      ),
      log_post_pred = log_post_pred_quad,
      adjustment = - 4,
      w_prior = 1
    )
  )

  fit3 <- bma(
    linear = model_bma_predictive(
      mcmc = data.frame(b1, b2, sigma),
      log_post_pred = log_post_pred,
      adjustment = - 3,
      w_prior = 1
    ),
    quad = model_bma_predictive(
      mcmc = data.frame(
        b1 = b1_quad,
        b2 = b2_quad,
        b3 = b3_quad,
        sigma = sigma_quad
      ),
      log_post_pred = log_post_pred_quad,
      adjustment = - 4,
      w_prior = 0
    )
  )

  # no MCMC, so not model index
  fit4 <- bma(
    linear = model_bma_predictive(
      log_post_pred = log_post_pred,
      adjustment = - 3,
      w_prior = .5
    ),
    quad = model_bma_predictive(
      log_post_pred = log_post_pred_quad,
      adjustment = - 4,
      w_prior = .5
    )
  )

  expect_true(all(fit2$model_index == "quad"))
  expect_true(all(fit3$model_index == "linear"))
  expect_equal(length(fit4$model_index), 0L)
})

test_that("weight calculation marginal model", {
  fit_marg <- bma(
    linear = model_bma_marginal(
      mcmc = data.frame(b1, b2, sigma),
      log_marginal = - 3,
      w_prior = w_prior
    ),
    quad = model_bma_marginal(
      mcmc = data.frame(
        b1 = b1_quad,
        b2 = b2_quad,
        b3 = b3_quad,
        sigma = sigma_quad
      ),
      log_marginal = - 4,
      w_prior = 1 - w_prior
    )
  )
  numerator <- exp(c(- 3, - 4)) * c(w_prior, 1 - w_prior)
  true_weights <- numerator / sum(numerator)
  names(true_weights) <- c("linear", "quad")
  expect_equal(fit_marg$w_post, true_weights)
})
