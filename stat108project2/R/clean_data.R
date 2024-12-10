#' Clean Data
#' These functions fix three main typos: typos where the genders of the mice are swapped,
#' when columns contain strings instead of numerics,
#' as well as outliers
#' @param data
#' @param column_name
#'
#' @return
#' @export
#'
#' @examples

usethis::use_package("stringdist")
usethis::use_package("readxl")
usethis::use_package("stringr")

# Mouse data functions
# 1. Fix gender pattern: Fixes gender when it could have been mistyped. This should fix the yellow highlight
# 2. Clean mixed columns: removes any strings from a column that's supposed to be numeric. Should fix blue highlight
# 3. Detect outliers: detects outliers based on either median (IQR) or mean (z-score). Best results with mean, threshold 2.2. Should fix both purple and blue highlight
# 4. Drop large weight reductions: drops rows if weight reduction

fix_gender_pattern <- function(data, column_name) {
  # Ensure the specified column exists
  if (!column_name %in% colnames(data)) {
    stop("Specified column does not exist in the data.")
  }

  # Extract the column
  ids <- data[[ID]]

  # Parse out the relevant components of the ID
  parsed_ids <- data.frame(
    Original = ids,
    Prefix = sub("_.*", "", ids),  # Extract the prefix before "_"
    Gender = sub(".*_(.)_.*", "\\1", ids),  # Extract the middle letter (Gender)
    Number = as.numeric(sub(".*_.*_(\\d+)", "\\1", ids))  # Extract the number after the last "_"
  )

  # Correct single mismatched genders
  corrected_genders <- parsed_ids$Gender
  for (i in seq_along(corrected_genders)) {
    if (i > 1 && i < length(corrected_genders)) {
      # Check for single mismatched genders
      if (corrected_genders[i] != corrected_genders[i - 1] &&
          corrected_genders[i] != corrected_genders[i + 1]) {
        corrected_genders[i] <- corrected_genders[i - 1]  # Switch to match the sequence
      }
    }
  }

  # Update the IDs with corrected genders
  parsed_ids <- parsed_ids %>%
    mutate(
      Correct_Gender = corrected_genders,
      Fixed_ID = paste0(Prefix, "_", Correct_Gender, "_", Number)
    )

  # Replace the original IDs with fixed IDs
  data[[ID]] <- parsed_ids$Fixed_ID
  return(data)
}

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

check_misspellings <- function(data, column_name, frequency_threshold = 3, similarity_threshold = 0.8) {
  # Ensure the specified column exists
  if (!column_name %in% colnames(data)) {
    stop("Specified column does not exist in the data.")
  }

  # Calculate the frequency of each value in the column
  value_counts <- data %>%
    count(.data[[column_name]], name = "Frequency") %>%
    arrange(desc(Frequency))

  # Separate common and less common values
  common_values <- value_counts %>%
    filter(Frequency >= frequency_threshold) %>%
    pull(.data[[column_name]])

  less_common_values <- value_counts %>%
    filter(Frequency < frequency_threshold) %>%
    pull(.data[[column_name]])

  # Initialize a results data frame
  results <- data.frame(
    Original_Value = character(),
    Similar_To = character(),
    Similarity_Score = numeric(),
    stringsAsFactors = FALSE
  )

  # Compare less common values to common values
  for (less_common in less_common_values) {
    for (common in common_values) {
      # Calculate string similarity
      similarity <- stringdist::stringsim(less_common, common, method = "jw")

      # Skip comparison if similarity is NA
      if (!is.na(similarity) && similarity > similarity_threshold) {
        results <- rbind(results, data.frame(
          Original_Value = less_common,
          Similar_To = common,
          Similarity_Score = similarity,
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  # Return the results as a tibble
  return(as_tibble(results))
}

drop_large_weight_reductions <- function(data, ..., threshold = 20) {
  # Capture the column names
  columns <- enquos(...)

  # Ensure there are at least two columns specified
  if (length(columns) < 2) {
    stop("At least two columns must be specified.")
  }

  # Convert column names to strings for easier manipulation
  column_names <- sapply(columns, quo_name)

  # Ensure all specified columns exist
  missing_columns <- setdiff(column_names, colnames(data))
  if (length(missing_columns) > 0) {
    stop(paste("The following columns do not exist in the data:", paste(missing_columns, collapse = ", ")))
  }

  # Check if all columns are numeric
  if (any(!sapply(data[, column_names], is.numeric))) {
    stop("One or more specified columns contain non-numeric values. Please clean your data before using this function.")
  }

  # Add a row number column for tracking
  data <- data %>%
    mutate(Row = row_number())

  # Initialize a list to store reduction information
  reductions <- list()

  # Loop through sequential columns and calculate percentage reduction
  for (i in seq_along(column_names)[-length(column_names)]) {
    col1 <- column_names[i]
    col2 <- column_names[i + 1]

    reduction_column <- paste0("Reduction_", col1, "_to_", col2)
    data <- data %>%
      mutate(!!reduction_column := ((.data[[col1]] - .data[[col2]]) / .data[[col1]]) * 100)

    # Identify rows with reduction above the threshold
    dropped <- data %>%
      filter(!is.na(.data[[reduction_column]]) & .data[[reduction_column]] >= threshold) %>%
      select(Row, !!sym(col1), !!sym(col2), !!sym(reduction_column)) %>%
      rename(Weight_Reduction = !!sym(reduction_column))

    reductions[[reduction_column]] <- dropped
  }

  # Combine all dropped rows across reductions
  dropped_rows <- bind_rows(reductions, .id = "Reduction_Source") %>%
    distinct(Row, .keep_all = TRUE)

  # Drop rows from the dataset
  data <- data %>%
    filter(!Row %in% dropped_rows$Row) %>%
    select(-Row, -starts_with("Reduction_"))

  return(list(Cleaned_Data = data, Dropped_Summary = dropped_rows))
}
