#' @title Posterior Weights and Model Averaging Setup
#' @description Calculate posterior weights of each model and optionally
#'   supply MCMC samples and functions (through the `bma_model()` function) to
#'   calculate a quantity of interest from each model using the `posterior()`
#'   function.
#' @param ... Named calls to the `bma_model()` function.
#' @param seed an integer which is used to specify the seed when sampling
#'   from the different models (e.g. in `posterior()`).
#' @details It is required that if MCMC samples are supplied, that each
#'   MCMC run must have the same number of collected samples.
#' @return bma: A list containing the prior and posterior weights for each
#'   model, the sampled model (`model_index`) at each MCMC iteration and
#'   the arguments supplied to each `bma_model()` call.
#' @example man/examples/ex_bma.R
#' @export
bma <- function(..., seed = sample(.Machine$integer.max, 1)) {
  models <- list(...)
  type <- assert_models(models)
  assert_seed(seed)
  assert_prior_weights(models)
  w_post <- calc_post_weights(models, type)
  out <- list(
    w_prior = get_prior_weights(models),
    w_post = w_post,
    models = models,
    seed = seed
  )
  class(out) <- "yodel_bma"
  out
}

calc_post_weights <- function(models, type) {
  if (type == "predictive") {
    out <- calc_post_weights_predictive(models)
  } else if (type == "marginal") {
    out <- calc_post_weights_marginal(models)
  }
  out
}

calc_post_weights_predictive <- function(models) {
  eta <- vapply(models, calc_eta, numeric(1))
  w_prior <- get_prior_weights(models)
  log_numerator <- eta + log(w_prior)
  calc_weight(log_numerator)
}

calc_post_weights_marginal <- function(models) {
  w_prior <- get_prior_weights(models)
  log_marginals <- vapply(models, function(xx) xx$log_marginal, numeric(1))
  log_numerator <- log_marginals + log(w_prior)
  calc_weight(log_numerator)
}

calc_weight <- function(log_numerator) {
  a <- max(log_numerator)
  log_denominator <- a + log(sum(exp(log_numerator - a)))
  w_post <- exp(log_numerator - log_denominator)
  w_post
}

calc_eta <- function(model) {
  eta <- apply(model$log_post_pred, 2, calc_eta_impl) %>%
    sum()
  eta <- eta + model$adjustment
}

calc_eta_impl <- function(log_post_pred) {
  a <- max(log_post_pred)
  n_mcmc <- length(log_post_pred)
  - log(n_mcmc) + a + log(sum(exp(log_post_pred - a)))
}

get_prior_weights <- function(models) {
  vapply(models, function(yy) yy$w_prior, numeric(1))
}
