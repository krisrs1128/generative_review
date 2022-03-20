
#' @importFrom tibble tibble
#' @export
basic_design <- function(N, n_treat = 3) {
  tibble(
    id = seq_len(N),
    subject = id,
    treatment = as.factor(sample(rep(LETTERS[1:n_treat], each = N / n_treat)))
  )
}

#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @importFrom dplyr group_by mutate ungroup
#' @export
blocked_design <- function(N, n_treat = 3) {
  tibble(
    id = seq_len(N),
    subject = rep(1:(N / n_treat), each = n_treat),
    treatment = as.factor(rep(LETTERS[1:n_treat], N / n_treat))
  ) %>%
  group_by(subject) %>%
  mutate(treatment = sample(treatment)) %>%
  ungroup()
}

#' @importFrom dplyr group_by mutate ungroup slice_head
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @export
compromise_design <- function(N, n_treat = 3, n_per_subject = 2) {
  tibble(
    id = seq_len(n_treat * N / n_per_subject),
    subject = rep(1:(N / n_per_subject), each = n_treat),
    treatment = as.factor(rep(LETTERS[1:n_treat], N / n_per_subject))
  ) %>%
  group_by(subject) %>%
  mutate(treatment = sample(treatment)) %>%
  slice_head(n = n_per_subject) %>%
  ungroup()
}

#' @export
simulate <- function(design, effects, depth = 500) {
  features <- ncol(effects$treatment)
  p <- probabilities(design, effects)
  counts <- matrix(0, n_distinct(design, "id"), features)
  for (i in seq_len(nrow(counts))) {
    counts[i, ] <- rmultinom(1, depth, p[i, ])
  }
  
  colnames(counts) <- seq_len(features)
  as_tibble(counts)
}

#' @export
design_effects <- function(subjects, treatments, features,  sigmas = c(2, 0.5), 
                           affected = 10) {
  # subject level effects
  effects <- list()
  effects[["subject"]] <- rnorm(features * subjects, 0, sigmas[1]) %>%
    matrix(nrow = subjects, ncol = features)
  
  # treatment level effects
  effects[["treatment"]] <- matrix(0, treatments, features)
  for (i in 1:treatments) {
    effects[["treatment"]][i, 1:affected] <- rnorm(affected, 0, sigmas[2])
  }
  
  effects
}

#' @export
probabilities <- function(design, effects) {
  features <- ncol(effects$treatment)
  subjects <- nrow(effects$subject)
  samples <- n_distinct(design, "id")
  
  p <- matrix(0, samples, features)
  for (i in seq_len(samples)) {
    p[i, ] <- effects$subject[design$subject[i], ] +
      effects$treatment[design$treatment[i], ]
  }
  
  exp(p) / rowSums(exp(p))
}