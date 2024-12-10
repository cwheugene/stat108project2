clean_mouse_data <- function(file_path) {
  # Load necessary libraries
  library(readxl)
  library(dplyr)

  # Read in sheets from the Excel file
  birth_data <- read_excel(file_path, sheet = "Birth")
  body_weight_data <- read_excel(file_path, sheet = "Body Weight")
  outcome_data <- read_excel(file_path, sheet = "Outcome")

  # Columns are read based on their position rather than their titles to avoid
  # problems with typos

  # Rename columns in 'Birth' based on their positions
  colnames(birth_data) <- c("ID", "sex", "num", "treatment")

  # Rename columns in 'Body Weight' based on their positions
  colnames(body_weight_data) <- c(
    "ID",
    "weight_1", "date_1",
    "weight_2", "date_2",
    "weight_3", "date_3"
  )

  # Rename columns in 'Outcome' based on their positions
  colnames(outcome_data) <- c(
    "ID",
    "outcome_1", "date_1",
    "outcome_2", "date_2",
    "outcome_3", "date_3"
  )

  # Convert relevant columns to appropriate types
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

  ordered_data <- merged_data %>%
    select(
      ID,
      sex,
      treatment,
      weight_1,
      outcome_1,
      date_1,
      weight_2,
      outcome_2,
      date_2,
      weight_3,
      outcome_3,
      date_3
    )

  # Return cleaned and ordered dataset
  return(ordered_data)
}

clean_mouse_data(file_path)

usethis::use_data(clean_mouse_data, overwrite = TRUE)

