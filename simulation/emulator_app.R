library(shiny)
library(glue)
library(bslib)
library(tidyverse)
library(laGP)


sim_data <- read_csv("covid_simulations.csv")

# code to set a clean theme
th <- theme_minimal() + 
  theme(
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#F7F7F7"),
    panel.border = element_rect(fill = NA, color = "#0c0c0c", size = 0.6),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.position = "bottom"
  )
theme_set(th)

emulator_data <- function(sim_data, symp_prob) {
  x <- sim_data %>%
    select(t, symp_prob, trace_prob, trace_time)
  x_ <- apply(x, 2, function(z) z / max(z))
  x_ref <- data.frame(
   t = seq(0, 1, length.out = 20),
   symp_prob = symp_prob,
   trace_prob = 0.8,
   trace_time = 1
  ) 
  
  y_hat <- aGP(x_, log(1 + sim_data$new_infections), x_ref)
  x_ref <- as.matrix(x_ref) %*% diag(apply(x, 2, max))
  colnames(x_ref) <- colnames(x)
  bind_cols(
    as.data.frame(x_ref), 
    data.frame(y_hat = y_hat$mean, var = y_hat$var)
  )
}

plot_trajectory <- function(y_hat) {
  ggplot(y_hat, aes(t, y_hat)) +
    geom_ribbon(aes(t, ymin = y_hat - sqrt(var), ymax = y_hat + sqrt(var)), fill = "#d3d3d3") +
    geom_smooth(se = F, col = "#0c0c0c", span = 1) +
    ylim(0, 6)
}

ui <- fluidPage(
  sliderInput("symp_prob", "Testing Probability", value = 0.8, min = 0, max = 1, step = 0.01),
  plotOutput("trajectory")
)

server <- function(input, output) {
  y_hat <- reactive({
    emulator_data(sim_data, input$symp_prob)
  })
  output$trajectory <- renderPlot({
    plot_trajectory(y_hat())
  })
}

shinyApp(ui, server)