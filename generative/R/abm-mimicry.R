
#' @importFrom nlrx experiment
#' @export
setup_experiment <- function(runtime = 250) {
  experiment(
    expname = "mimcry",
    outpath = "./mimcry_results/",
    repetition = 1,
    tickmetrics = "true",
    idsetup ="setup",
    idgo ="go",
    runtime = runtime,
    evalticks = seq_len(runtime),
    variables = list(
      "mutation-rate" = list(min = 1, max = 99, qfun = "qunif")
    ),
    constants = list(
      "memory-duration" = 30,
      "memory-size" = 3
    ),
    metrics.turtles = list("turtles" = c("color")),
    metrics = c("count monarchs", "count viceroys")
  )
}

#' @importFrom nlrx run_nl_one simdesign_lhs
#' @importFrom magrittr %>%
#' @export
mutation_model <- function(seed, nlobj) {
  function(rate) {
    library(tidyverse)
    nlobj@experiment@variables <- list(
      "mutation-rate" = list(min = rate - 1, max = rate + 1, qfun = "qunif")
    )
    nlobj@simdesign <- simdesign_lhs(nlobj, samples = 1, nseeds = 1)
    
    nl_obj %>%
      run_nl_one(seed = 1, siminputrow = 1) %>%
      summary_statistics()
  }
}
  
  
#' @importFrom dplyr select starts_with unnest group_by filter summarise mutate
#'   pull
#' @importFrom tidyr pivot_wider
#' @export
summary_statistics <- function(nl_results) {
  color_stats <- nl_results %>%
    select(`[step]`, starts_with("count"), metrics.turtles) %>%
    unnest(metrics.turtles) %>%
    filter(breed != "birds") %>%
    group_by(breed, `[step]`) %>%
    summarise(mean = mean(color)) %>%
    pivot_wider(names_from = "breed", values_from = "mean") %>%
    mutate(
      diff = viceroys - monarchs,
      step_bin = cut(`[step]`, 4)
    )
  
  color_stats %>%
    group_by(step_bin) %>%
    summarise(bin_diff = mean(diff)) %>%
    pull(bin_diff)
}