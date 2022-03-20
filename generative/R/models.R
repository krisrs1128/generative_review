
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
    T = n_distinct(design, "treatment"),
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