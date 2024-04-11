#' @rdname bma
#' @param log_post_pred a matrix containing the log likelihood for each
#'   observation on each iteration of the MCMC.  The matrix should have
#'   dimensions (number-of-MCMC-iteration) by (number of observations).
#' @param adjustment an adjustment to be applied to the posterior log-predictive
#'   likelihood.  A simple bias correction in Ando & Tsay (2010) is: - (number
#'   of parameters in the model) / 2.
#' @param w_prior the prior weight for the model.
#' @param mcmc a named list (or dataframe) of MCMC samples of model parameters.
#' @param fun a function which takes the MCMC samples and returns a value of
#'   interest.
#' @return model_bma: A named list of the arguments, with a
#'  "yodel_bma_candidate" class attached.
#' @references Ando, T., & Tsay, R. (2010). Predictive likelihood for Bayesian
#'   model selection and averaging. International Journal of Forecasting, 26(4),
#'   744-763.
#' @export
model_bma_predictive <- function(
  log_post_pred,
  adjustment = 0,
  w_prior = 1,
  mcmc = NULL,
  fun = NULL
) {
  assert_mcmc(mcmc)
  assert_log_post_pred(log_post_pred)
  assert_fun(fun)
  out <- list(
    mcmc = mcmc,
    log_post_pred = log_post_pred,
    adjustment = adjustment,
    fun = fun,
    w_prior = w_prior
  )
  class(out) <- c("yodel_model_predictive", "yodel_bma_candidate")
  out
}

#' @rdname bma
#' @param log_marginal The log marginal likelihood of the model.
#' @return model_bma: A named list of the arguments, with a
#'  "yodel_bma_candidate" class attached.
#' @export
model_bma_marginal <- function(
  log_marginal,
  w_prior = 1,
  mcmc = NULL,
  fun = NULL
) {
  assert_mcmc(mcmc)
  assert_fun(fun)
  out <- list(
    log_marginal = log_marginal,
    mcmc = mcmc,
    fun = fun,
    w_prior = w_prior
  )
  class(out) <- c("yodel_model_marginal", "yodel_bma_candidate")
  out
}
