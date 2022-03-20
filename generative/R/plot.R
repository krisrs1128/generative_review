
#' @importFrom ggplot2 theme_minimal theme theme_set element_blank element_rect
#'   element_text
#' @export
review_theme <- function() {
  th <- theme_minimal() + 
    theme(
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#f7f7f7"),
      panel.border = element_rect(fill = NA, color = "#0c0c0c", size = 0.6),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 13),
      legend.position = "bottom"
    )
  theme_set(th)
}

#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot geom_point aes facet_wrap scale_color_brewer
#'   scale_y_sqrt
#' @export
plot_treatments <- function(x_df, max_features = 21) {
  x_df %>%
    filter(as.integer(feature) < max_features) %>%
    ggplot(aes(treatment, value)) +
    geom_point(aes(col = treatment), size = 0.5) +
    facet_wrap(~ as.integer(feature)) +
    scale_color_brewer(palette = "Set2") +
    scale_y_sqrt()
}

#ggplot(
#  treatment_estimates$blocked, 
#  aes(feature, value, col = as.factor(treatment))
#  ) +
#  stat_dots() +
#  geom_point(data = truth, size = 1.6, col = "#0c0c0c") +
#  geom_point(data = truth, size = 0.8) +
#  scale_color_brewer(palette = "Set2")
