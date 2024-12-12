clean_mixed_columns <- function(data, ...) {
  # Capture the column names passed in the ellipsis
  columns <- enquos(...)

  # Convert the column names to strings for easy reference
  column_names <- sapply(columns, quo_name)

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
