#' Detect and Handle Outliers in Numeric Data
#'
#' This function detects outliers in specified numeric columns of a data frame
#' using either the Z-score (mean-based) or IQR (median-based) method. Optionally,
#' outliers can be replaced with `NA`.
#'
#' @param data A data frame containing the columns to evaluate.
#' @param ... One or more numeric columns to check for outliers.
#' @param method A character string specifying the method to use for detecting outliers.
#' Options are `"mean"` for the Z-score method or `"median"` for the IQR method.
#' Defaults to `"mean"`.
#' @param threshold A numeric value specifying the threshold for identifying outliers.
#' For the Z-score method, this represents the number of standard deviations from the mean
#' (default: 2.2). For the IQR method, this represents the multiplier for the interquartile range.
#' @param drop_outliers A logical value. If `TRUE`, outliers are replaced with `NA` in the
#' returned cleaned data frame. Defaults to `FALSE`.
#'
#' @return If `drop_outliers = FALSE`, returns a list of data frames, each containing
#' information about the detected outliers for a specified column:
#' \item{Row}{The row numbers of the outliers.}
#' \item{Value}{The original values of the outliers.}
#' \item{Z_Score}{(For the Z-score method) The Z-scores of the outliers.}
#' \item{Distance_to_Bound}{(For the IQR method) The distance of the outliers from the IQR bounds.}
#'
#' If `drop_outliers = TRUE`, returns a list with:
#' \item{Results}{As described above.}
#' \item{Cleaned_Data}{A data frame with outliers replaced by `NA`.}
#'
#' @export
#'
#' @examples
#' # Example dataset
#' data <- data.frame(
#'   Weight_Day1 = c(50, 60, 55, 100, 45),
#'   Weight_Day2 = c(48, 58, 53, 110, 43),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Detect outliers using the Z-score method
#' outlier_results <- detect_outliers(data, Weight_Day1, Weight_Day2, method = "mean", threshold = 2.2)
#'
#' # View results
#' outlier_results$Weight_Day1
#'
#' # Detect outliers using the IQR method and drop them
#' cleaned_results <- detect_outliers(data, Weight_Day1, Weight_Day2, method = "median", threshold = 1.5, drop_outliers = TRUE)
#'
#' # View cleaned data
#' cleaned_results$Cleaned_Data

detect_outliers <- function(data, ..., method = "mean", threshold = 2.2, drop_outliers = FALSE) {
  # Validate the method
  if (!method %in% c("mean", "median")) {
    stop("Invalid method. Use 'mean' for Z-score or 'median' for IQR.")
  }

  # Capture column names
  columns <- rlang::enquos(...)
  if (length(columns) == 0) {
    stop("You must specify at least one column.")
  }

  # Initialize a list to store results
  results <- list()
  clean_data <- data  # Copy of the data to modify if drop_outliers = TRUE

  # Iterate over each specified column
  for (col in columns) {
    column_name <- rlang::quo_name(col)
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
      if (drop_outliers) {
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
