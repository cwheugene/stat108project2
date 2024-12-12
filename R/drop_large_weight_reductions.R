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
