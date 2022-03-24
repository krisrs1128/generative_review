
#' @importFrom cmdstanr cmdstan_model
#' @importFrom dplyr n_distinct
#' @export
multinomial_logistic <- function(x, design, model_path=NULL) {
  if (is.null(model_path)) {
    model_path <- system.file("extdata", "multinomial_logistic.stan", package = "generative")
  }
  model <- cmdstan_model(model_path)
  inputs <- list(
    N = nrow(x),
    D = ncol(x),
    T = nlevels(design$treatment),
    S = n_distinct(design, "subject"),
    t_ix = as.integer(design$treatment),
    s_ix = design$subject,
    x = as.matrix(x)
  )
  
  model$sample(data = inputs, chains = 1)
}

#' @importFrom tidybayes tidy_draws
#' @importFrom dplyr select starts_with mutate
#' @importFrom stringr str_extract str_remove
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#' @export
process_estimate <- function(fit, coef_name) {
  tidy_draws(fit) %>%
   select(`.chain`, `.iteration`, starts_with(coef_name)) %>%
    pivot_longer(starts_with("treatment"), names_to = "coef") %>%
    mutate(
      treatment = as.integer(str_extract(coef, "[0-9]+")),
      feature = str_extract(coef, "[0-9]+\\]$"),
      feature = as.integer(str_remove(feature, "]"))
    )
}

#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_remove
#' @importFrom magrittr %>%
#' @export
process_truth <- function(effects) {
  effects %>%
    as.data.frame() %>%
    mutate(treatment = 1:n()) %>%
    pivot_longer(-treatment, names_to = "feature") %>%
    mutate(feature = as.integer(str_remove(feature, "V")))
}

#' @importFrom MCMCpack rdirichlet
#' @importFrom mvtnorm rmvt
#' @importFrom magrittr %>%
#' @importFrom purrr map_dfr
#' @export
t_mixture <- function(N, K = 5, df = 8) {
  pi <- rdirichlet(1, rep(2, K))
  Ns <- rmultinom(1, N, prob = pi)
  deltas <- matrix(rnorm(K * 2, 0, 5), K, 2)
  sigmas <- map(1:K, ~ diag(rgamma(2, 5, 5)))
  x <- list()
  for (k in seq_len(K)) {
    x[[k]] <- rmvt(Ns[k], sigmas[[k]], df = df, delta = deltas[k, ], type = "shifted")
  }
  
  x %>%
    map_dfr(~ as_tibble(.), .id = "k")
}

#' @importFrom cmdstanr cmdstan_model
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @export
fit_gmm <- function(x, K = 5, model_path = NULL) {
  if (is.null(model_path)) {
    model_path <- system.file("extdata", "gmm.stan", package = "generative")
  }
  
  model <- cmdstan_model(model_path)
  input <- list(
    K = K,
    N = nrow(x),
    x = x %>% select(-k)
  )
  
  model$variational(input)
}

#' @importFrom cmdstanr cmdstan_model
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @export
fit_gmm_cov <- function(x, K = 5, model_path = NULL) {
  if (is.null(model_path)) {
    model_path <- system.file("extdata", "gmm_cov.stan", package = "generative")
  }
  
  model <- cmdstan_model(model_path)
  input <- list(
    K = K,
    N = nrow(x),
    x = x %>% select(-k),
    alpha = rep(2, K)
  )
  
  model$variational(input)
}


#' @importFrom cmdstanr cmdstan_model
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @export
fit_tmm <- function(x, K = 5, model_path = NULL) {
  if (is.null(model_path)) {
    model_path <- system.file("extdata", "tmm.stan", package = "generative")
  }
  
  model <- cmdstan_model(model_path)
  input <- list(
    K = K,
    N = nrow(x),
    x = x %>% select(-k),
    alpha = rep(2, K)
  )
  
  model$variational(input)
}

#' importFrom tidybayes tidy_draws
#' @importFrom dplyr filter select starts_with mutate
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom stringr str_extract str_remove
#' @export
mixture_predictive <- function(fit, max_iter = 10, outlier_value = 100) {
  tidy_draws(fit) %>%
    filter(.iteration < max_iter) %>%
    select(.iteration, .chain, starts_with("x_sim")) %>%
    pivot_longer(starts_with("x_sim")) %>%
    mutate(
      i = str_extract(name, "[0-9]+"),
      j = str_extract(name, "[0-9]\\]$"),
      j = str_remove(j, "]")
    ) %>%
    select(-name) %>%
    filter(abs(value) < outlier_value) %>%
    pivot_wider(names_from = "j", values_from = "value")
}

#' @importFrom dplyr bind_rows select mutate filter rename
#' @export
bind_dicriminative <- function(x, x_sim, iter = 1) {
  bind_rows(
    x %>%
      select(-k) %>%
      mutate(y = "true"),
    x_sim %>%
      filter(.iteration == iter) %>%
      select(`1`, `2`) %>%
      rename(V1 = `1`, V2 = `2`) %>%
      mutate(y = "simulated")
  )
}