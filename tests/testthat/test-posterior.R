test_that("posterior", {
  # functions which caclulate the dose response for a linear and quadratic model
  fun_linear <- function(x, dose) {
    mean_response <- x$b1 + x$b2 * dose
    data.frame(iter = seq_len(nrow(x)), dose = dose, mean = mean_response)
  }

  fun_quad <- function(x, dose) {
    x <- as.data.frame(do.call("rbind", x))
    mean_response <- x$b1 + x$b2 * dose + x$b3 * dose ^ 2
    data.frame(iter = seq_len(nrow(x)), dose = dose, mean = mean_response)
  }

  b1 <- 1:5
  b2 <- 11:15
  sigma <- seq(.1, .5, .1)

  b1_quad <- 1:5 / 2
  b2_quad <- 11:15 / 2
  b3_quad <- 5:1
  sigma_quad <- seq(.1, .5, .1)

  mcmc_quad <- data.frame(
    b1 = b1_quad,
    b2 = b2_quad,
    b3 = b3_quad,
    sigma = sigma_quad
  )
  mcmc_quad <- list(
    as.matrix(mcmc_quad[1:3, ]),
    as.matrix(mcmc_quad[4:5, ])
  )
  class(mcmc_quad) <- "mcmc.list"

  fit <- bma(
    linear = model_bma_predictive(
      mcmc = data.frame(b1, b2, sigma),
      log_post_pred = matrix(log(1:100), 5, 20),
      adjustment = - 3 / 2,
      w_prior = .5,
      fun = fun_linear
    ),
    quad = model_bma_predictive(
      mcmc = mcmc_quad,
      log_post_pred = matrix(log(2:101), 5, 20),
      adjustment = - 4 / 2,
      w_prior = .5,
      fun = fun_quad
    ),
    seed = 5L
  )

  dose <- 2
  tab <- posterior(fit, dose = dose)
  ind <- tab$model == "linear"
  tab <- tab %>%
    dplyr::mutate(
      true_vals = c(
        !!b1[ind] + !!b2[ind] * !!dose,
        !!b1_quad[!ind] +
          !!b2_quad[!ind] * !!dose +
            !!b3_quad[!ind] * !!dose ^ 2
      )[c(3, 1, 2, 4, 5)]
    )
  expect_equal(tab$mean, tab$true_vals)

  expect_equal(
    posterior(fit, dose = dose),
    posterior(fit, dose = dose)
  )
})
