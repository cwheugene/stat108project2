#' Clean Mixed Data Columns
#'
#' This function cleans specified columns in a data frame by removing non-numeric characters
#' and converting them to numeric. It is useful for handling columns that contain a mix of
#' numeric values and text, such as units or other annotations.
#' @name clean_mixed_columns
#' @title Checks and Cleans Mixed Data Type Columns
#' @description Removes strings from any numeric column
#' @param data A data frame containing the columns to clean.
#' @param ... One or more column names to clean. These columns must exist in the data frame.
#'
#' @return A data frame with the specified columns cleaned and converted to numeric.
#' Rows with entirely non-numeric values in the specified columns are replaced with `NA`.
#'
#' @export
#'
#' @examples
#' # Example dataset
#' data <- data.frame(
#'   ID = 1:4,
#'   Weight = c("20g", "25", "twenty", "21lbs"),
#'   Outcome = c("52", "63", "sixty", "fifty"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Clean the 'Weight' and 'Height' columns
#' cleaned_data <- clean_mixed_columns(data, Weight, Outcome)
#'
#' # View the cleaned data
#' print(cleaned_data)
library(dplyr)
clean_mixed_columns <- function(data, ...) {
  # Capture the column names passed in the ellipsis
  columns <- rlang::enquos(...)

  # Convert the column names to strings for easy reference
  column_names <- sapply(columns, rlang::quo_name)

  # Iterate over the specified columns and clean them
  for (col in column_names) {
    # Ensure the column exists in the data
    if (!col %in% colnames(data)) {
      stop(paste("Column", col, "does not exist in the data frame."))
    }

    # Convert the column to character for processing
    column_data <- as.character(data[[col]])

    # Replace entirely non-numeric rows with NA
    column_data <- ifelse(grepl("^[^0-9.]+$", column_data), NA, column_data)

    # Remove characters and keep only numeric parts
    column_data <- gsub("[^0-9.]", "", column_data)

    # Convert to numeric
    data[[col]] <- as.numeric(column_data)
  }

  return(data)
}
