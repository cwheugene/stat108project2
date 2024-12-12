clean_mouse_data <- function(file_path) {
  # Load necessary libraries
  library(readxl)
  library(dplyr)

  # Check if file exists
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  # Read in sheets with error handling
  tryCatch({
    birth_data <- read_excel(file_path, sheet = "Birth")
    body_weight_data <- read_excel(file_path, sheet = "Body Weight")
    outcome_data <- read_excel(file_path, sheet = "Outcome")
  }, error = function(e) {
    stop("Error reading sheets: ", e$message)
  })

  # Rename columns
  colnames(birth_data) <- c("ID", "sex", "num", "treatment")
  colnames(body_weight_data) <- c("ID", "weight_1", "date_1", "weight_2", "date_2", "weight_3", "date_3")
  colnames(outcome_data) <- c("ID", "outcome_1", "date_1", "outcome_2", "date_2", "outcome_3", "date_3")

  # Convert relevant columns
  body_weight_data <- body_weight_data %>%
    mutate(
      weight_1 = as.numeric(weight_1),
      weight_2 = as.numeric(weight_2),
      weight_3 = as.numeric(weight_3),
      date_1 = as.Date(date_1, format = "%Y-%m-%d"),
      date_2 = as.Date(date_2, format = "%Y-%m-%d"),
      date_3 = as.Date(date_3, format = "%Y-%m-%d")
    )

  outcome_data <- outcome_data %>%
    mutate(
      date_1 = as.Date(date_1, format = "%Y-%m-%d"),
      date_2 = as.Date(date_2, format = "%Y-%m-%d"),
      date_3 = as.Date(date_3, format = "%Y-%m-%d")
    )

  # Merge datasets
  merged_data <- birth_data %>%
    inner_join(body_weight_data, by = "ID") %>%
    inner_join(outcome_data, by = c("ID", "date_1", "date_2", "date_3"))

  # Reorder columns
  ordered_data <- merged_data %>%
    select(
      ID, sex, treatment,
      weight_1, outcome_1, date_1,
      weight_2, outcome_2, date_2,
      weight_3, outcome_3, date_3
    )

  # Return cleaned and ordered dataset
  return(ordered_data)
}

