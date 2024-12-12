detect_outliers <- function(data, ..., method = "mean", threshold = 2.2, drop_outliers = FALSE) {
  # Validate the method
  if (!method %in% c("mean", "median")) {
    stop("Invalid method. Use 'mean' for Z-score or 'median' for IQR.")
  }

  # Capture column names
  columns <- enquos(...)
  if (length(columns) == 0) {
    stop("You must specify at least one column.")
  }

  # Initialize a list to store results
  results <- list()
  clean_data <- data  # Copy of the data to modify if drop_outliers = TRUE

  # Iterate over each specified column
  for (col in columns) {
    column_name <- quo_name(col)
    column_data <- data[[column_name]]

    # Check if the column exists and is numeric
    if (!is.numeric(column_data)) {
      warning(paste("Column", column_name, "is not numeric. Please clean the data first before using this function. You can use 'clean_mixed_data' function"))
      next
    }

    if (method == "mean") {
      # Z-score method
      mean_value <- mean(column_data, na.rm = TRUE)
      sd_value <- sd(column_data, na.rm = TRUE)
      z_scores <- (column_data - mean_value) / sd_value

      # Identify outliers
      outlier_indices <- which(abs(z_scores) > threshold)
      threshold_values <- z_scores[outlier_indices]

      # Save results
      results[[column_name]] <- data.frame(
        Row = outlier_indices,
        Value = column_data[outlier_indices],
        Z_Score = threshold_values
      )

      # Remove outliers if drop_outliers = TRUE
      if (drop_outliers) {
        clean_data[[column_name]][outlier_indices] <- NA
      }

    } else if (method == "median") {
      # IQR method
      Q1 <- quantile(column_data, 0.25, na.rm = TRUE)
      Q3 <- quantile(column_data, 0.75, na.rm = TRUE)
      IQR_value <- Q3 - Q1

      # Identify outliers
      lower_bound <- Q1 - threshold * IQR_value
      upper_bound <- Q3 + threshold * IQR_value
      outlier_indices <- which(column_data < lower_bound | column_data > upper_bound)

      # Calculate distances to the bounds
      distance_to_iqr <- ifelse(
        column_data[outlier_indices] < lower_bound,
        lower_bound - column_data[outlier_indices],
        column_data[outlier_indices] - upper_bound
      )

      # Save results
      results[[column_name]] <- data.frame(
        Row = outlier_indices,
        Value = column_data[outlier_indices],
        Distance_to_Bound = distance_to_iqr
      )

      # Remove outliers if drop_outliers = TRUE
      if (drop) {
        clean_data[[column_name]][outlier_indices] <- NA
      }
    }
  }

  # Return results and optionally the cleaned data
  if (drop_outliers) {
    return(list(Results = results, Cleaned_Data = clean_data))
  } else {
    return(results)
  }
}
