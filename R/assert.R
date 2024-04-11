assert_prior_weights <- function(x) {
  w_sum <- sum(get_prior_weights(x))
  if (!isTRUE(all.equal(1, w_sum))) {
    rlang::abort(
      paste0("Prior weights must sum to 1: ", w_sum),
      class = "yodel"
    )
  }
}

assert_models <- function(x) {
  all_bma <- vapply(
    x,
    inherits,
    FUN.VALUE = logical(1),
    what = "yodel_bma_candidate",
  ) %>%
    all()
  if (!all_bma) {
    rlang::abort(
      "All arguments to `bma()` must be bma candidate models.",
      class = "yodel"
    )
  }
  if (is.null(names(x)) || isTRUE(any(names(x) == ""))) {
    rlang::abort(
      "All arguments to `bma()` must be named.",
      class = "yodel"
    )
  }
  all_marginal <- vapply(
    x,
    inherits,
    FUN.VALUE = logical(1),
    what = "yodel_model_marginal"
  ) %>%
    all()
  all_predictive <- vapply(
    x,
    inherits,
    FUN.VALUE = logical(1),
    what = "yodel_model_predictive"
  ) %>%
    all()
  if (!all_marginal && !all_predictive) {
    rlang::abort(
      "All arguments must use the same model function.",
      class = "yodel"
    )
  }
  if (all_marginal) {
    type <- "marginal"
  } else if (all_predictive) {
    type <- "predictive"
  }
  type
}

assert_mcmc <- function(x) {
  if (!is.null(x) && !is.list(x)) {
    rlang::abort("Object `mcmc` must be a list().", class = "yodel")
  }
}

assert_log_post_pred <- function(log_post_pred, mcmc) {
  if (!inherits(log_post_pred, "matrix")) {
    rlang::abort("Object `log_post_pred` must be a matrix.", class = "yodel")
  }
}

assert_fun <- function(fun) {
  if (!is.null(fun) && !is.function(fun)) {
    rlang::abort("`fun` must be a function or NULL.", class = "yodel")
  }
}

assert_n_mcmc <- function(samps) {
  n_mcmc <- samps %>%
    dplyr::group_by(.data$model) %>%
    dplyr::summarize(n_mcmc = max(.data$iter), .groups = "drop") %>%
    dplyr::pull(n_mcmc)
  if (length(unique(n_mcmc)) > 1) {
    msg <- "MCMC samples have different numbers of iterations between models."
    rlang::abort(msg, class = "yodel")
  }
  return(n_mcmc[1])
}

assert_exists <- function(x) {
  if (is.null(x)) {
    xname <- deparse(substitute(x))
    rlang::abort(paste0(xname, " must not be NULL."), class = "yodel")
  }
}

assert_samples <- function(x, model_name) {
  if (!is.list(x)) {
    rlang::abort(
      paste0("`fun` must return a data.frame for model ", model_name, "."),
      class = "yodel"
    )
  }
  if (!rlang::has_name(x, "iter")) {
    rlang::abort(
      paste0(
        "`fun` must return a column `iter` for model ", model_name, "."
      ),
      class = "yodel"
    )
  }
  x
}

assert_seed <- function(seed) {
  if (length(seed) != 1) {
    msg <- "\"seed\" must be an integer of length 1."
    rlang::abort(msg, class = "yodel")
  }
}
