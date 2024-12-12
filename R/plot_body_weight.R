#' @name plot_body_weight
#' @title Plots body weight over time
#' @description Plot Mouse Body Weight Over Time by Treatment Group
#' @param data A cleaned and ordered data frame
#' @return A ggplot object showing body weight trends over time.
#' @import ggplot2
#' @import tidyr

plot_body_weight <- function(data) {
  # Load necessary library
  # Reshape the data into long format for ggplot
  long_data <- data %>%
    pivot_longer(
      cols = starts_with("weight_"),
      names_to = "weight_measurement",
      values_to = "weight"
    ) %>%
    pivot_longer(
      cols = starts_with("date_"),
      names_to = "date_measurement",
      values_to = "date"
    ) %>%
    filter(substr(weight_measurement, -1, -1) == substr(date_measurement, -1, -1)) %>% # Match corresponding weights and dates
    select(ID, sex, treatment, weight, date)

  # Create the plot
  plot <- ggplot(long_data, aes(x = date, y = weight, color = treatment, group = ID)) +
    geom_line() +
    geom_point() +
    facet_wrap(~sex) +
    labs(
      title = "Mouse Body Weight Trends Over Time by Treatment Group",
      x = "Date",
      y = "Body Weight",
      color = "Treatment"
    ) +
    theme_minimal()

  return(plot)
}
