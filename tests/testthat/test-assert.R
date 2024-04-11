test_that("assertions", {
  expect_error(
    bma(
      a = model_bma_predictive(
        log_post_pred = matrix(3, 1, 1),
        adjustment = - 1,
        w_prior = 0
      )
    ),
    class = "yodel"
  )
  expect_error(bma(3), class = "yodel")
  expect_error(
    bma(
      model_bma_predictive(
        log_post_pred = matrix(3, 1, 1),
        adjustment = - 1,
        w_prior = 0
      )
    ),
    class = "yodel"
  )
  expect_error(
    bma(
      a = model_bma_predictive(
        log_post_pred = 3,
        adjustment = - 1,
        w_prior = 0
      )
    ),
    class = "yodel"
  )
  expect_error(
    bma(
      model_bma_predictive(
        log_post_pred = matrix(3, 1, 1),
        adjustment = - 1,
        w_prior = 0,
        mcmc = 1
      )
    ),
    class = "yodel"
  )
  expect_error(
    bma(
      model_bma_predictive(
        log_post_pred = matrix(3, 1, 1),
        adjustment = - 1,
        w_prior = 0,
        mcmc = data.frame(sigma = 1:5)
      )
    ),
    class = "yodel"
  )
  expect_error(assert_fun(3), class = "yodel")
  expect_error(assert_exists(NULL), class = "yodel")
  expect_error(assert_samples(3, "mod1"), class = "yodel")
  expect_error(assert_samples(data.frame(beta = 3), "mod1"), class = "yodel")

  expect_error(
    assert_models(
      list(
        a = model_bma_predictive(
          mcmc = data.frame(beta = 1:5),
          log_post_pred = matrix(3, 5, 1),
          adjustment = - 1,
          w_prior = .5
        ),
        b = model_bma_marginal(
          mcmc = data.frame(beta = 1:5),
          log_marginal = - 3,
          w_prior = .5
        )
      )
    ),
    class = "yodel"
  )

  expect_error(
    assert_n_mcmc(data.frame(iter = c(1, 2, 1), model = c("a", "a", "b"))),
    class = "yodel"
  )

  expect_error(assert_seed(3:4), class = "yodel")
})
