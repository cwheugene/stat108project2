#' Generate a Summary Table of Average Body Weights
#'
#' @param data A cleaned and ordered data frame from `clean_mouse_data`.
#' @return A summary table (tibble) showing average body weights by treatment and sex.
#' @import dplyr

generate_summary_table <- function(data) {
  # Load necessary library
  library(dplyr)

  # Create a summary table
  summary_table <- data %>%
    group_by(treatment, sex) %>%
    summarise(
      avg_weight_1 = mean(weight_1, na.rm = TRUE),
      avg_weight_2 = mean(weight_2, na.rm = TRUE),
      avg_weight_3 = mean(weight_3, na.rm = TRUE),
      .groups = "drop"  # Prevent grouping from carrying over
    )

  # Rename columns for clarity
  summary_table <- summary_table %>%
    rename(
      `Treatment` = treatment,
      `Sex` = sex,
      `Average Weight at Time 1` = avg_weight_1,
      `Average Weight at Time 2` = avg_weight_2,
      `Average Weight at Time 3` = avg_weight_3
    )

  return(summary_table)
}
