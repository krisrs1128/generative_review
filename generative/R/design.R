
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

#' @importFrom tibble as_tibble
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

#' @importFrom dplyr n_distinct
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

#' @export
weighted_sampling <- function(N, intervals, weights) {
  samples <- list()
  interval_ix <- rmultinom(1, N, weights)
  for (i in seq_along(interval_ix)) {
    samples[[i]] <- runif(interval_ix[i], intervals[i, 1], intervals[i, 2])
  }
  
  unlist(samples)  
}

#' @export
peak_fun <- function(times, peak_loc, height, end) {
  mu <- rep(0, length(times))
  rise <- peak_loc > times & times >= 0
  mu[rise] <- seq(0, height, length.out = sum(rise))
  fall <- times >= peak_loc & times < end
  mu[fall] <- seq(height, 0, length.out = sum(fall))
  approxfun(times, mu)
}

#' @export
gaussian_noise <- function(mean_fun, x, sigma = 1) {
  mean_fun(x) + rnorm(length(x), 0, sigma)
}

#' @importFrom splines bs
#' @export
spline_mse <- function(u, times, pf) {
  y <- gaussian_noise(pf, u)
  fit <- lm(y ~ bs(u, df = 6, degree = 1, Boundary.knots = c(-10, 10)))
  f_hat <- predict(fit, newdata = data.frame(u = times))
  mean((f_hat - pf(times)) ^ 2)
}

#' @export
make_intervals <- function(length.out, start = -10, end = 10) {
  u <- seq(start, end, length.out = length.out + 1)
  cbind(head(u, -1), tail(u, -1))
}

#' @export
evaluate_weights <- function(beta, intervals, times, pf, N = 40, n_rep = 20) {
  w <- exp(beta) / sum(exp(beta))
  mse <- vector(length = n_rep)
  for (i in seq_along(mse)) {
    u <- weighted_sampling(N, intervals, w)
    mse[i] <- spline_mse(u, times, pf)
  }
  
  median(mse)
}

rescale <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#' @export
scale_list <- function(x) {
  rescale(do.call(rbind, x))
}