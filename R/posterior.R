#' @title Calculate Posterior Quantities
#' @description Calculate posterior quantities specifically of interest for
#'   a given model.
#' @param x MCMC output.
#' @param ... additional arguments passed to S3 methods.
#' @return a dataframe or tibble with the posterior probabilities.
#' @example man/examples/ex_posterior.R
#' @export
posterior <- function(x, ...) {
  UseMethod("posterior", x)
}

#' @title Posterior Samples from Bayesian Model Averaging
#' @description Calculate posterior quantities of interest using Bayesian
#'   model averaging.
#' @param x output from a call to bma().
#' @param ... additional arguments to be passed to each of the functions used
#'   to calculate the quantity of interest.
#' @return A dataframe with the posterior samples for each iteration of the
#'   MCMC.  The dataframe will have, at a minimum, the columns "iter" and
#'   "model" indicating the MCMC iteration and the model that was used
#'   in the calculations.  The functions used for each model are defined
#'   within the `model_bma()` function and used in the `bma()` function.  See
#'   the example below.
#' @example man/examples/ex_posterior.R
#' @export
posterior.yodel_bma <- function(x, ...) { # nolint
  samps <- purrr::map2_dfr(
    x$models,
    names(x$models),
    posterior_impl,
    ...
  )
  n_mcmc <- assert_n_mcmc(samps)
  model_index <- get_model_index(x$w_post, n_mcmc, x$seed)
  dplyr::inner_join(samps, model_index, by = c("iter", "model")) %>%
    dplyr::arrange(.data$iter)
}

posterior_impl <- function(model, model_name, ...) {
  mcmc <- model$mcmc
  fun <- model$fun
  assert_exists(mcmc)
  assert_exists(fun)
  rlang::exec(fun, mcmc, ...) %>%
    assert_samples(model_name) %>%
    dplyr::mutate(model = !!model_name)
}

get_model_index <- function(w_post, n_mcmc, seed) {
  set.seed(seed)
  model <- sample(names(w_post), n_mcmc, replace = TRUE, prob = w_post)
  set.seed(NULL)
  data.frame(
    model = model,
    iter = 1:n_mcmc
  )
}
