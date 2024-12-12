#' Drop Large Weight Reductions
#'
#' Identifies and removes rows where the percentage reduction in weight between
#' consecutive weighings exceeds a specified threshold (default: 20%).
#'
#' This function calculates the percentage weight reduction between specified columns,
#' flags rows exceeding the threshold, and returns a cleaned dataset along with a summary
#' of dropped rows.
#' @name drop_large_weight_reductions
#' @title Drops Any Rows With Large Weight Reductions
#' @description Detects and removes rows where the mouse trial weight decreases dramatically across columns
#' @param data A data frame containing the columns to evaluate.
#' @param ... Columns containing weight measurements. At least two columns must be specified.
#' @param threshold A numeric value specifying the percentage reduction threshold. Defaults to 20.
#'
#' @return A list with the following components:
#' \item{Cleaned_Data}{A data frame with rows exceeding the threshold removed.}
#' \item{Dropped_Summary}{A data frame summarizing rows that were removed, including
#' the columns involved in the reduction and the calculated percentage reduction.}
#'
#' @export
#'
#' @examples
#' # Example dataset
#' data <- data.frame(
#'   Mouse_ID = 1:5,
#'   Weight_Day1 = c(50, 60, 55, 45, 40),
#'   Weight_Day2 = c(45, 50, 40, 40, 35),
#'   Weight_Day3 = c(40, 48, 38, 38, 30)
#' )
#'
#' # Drop rows with a weight reduction greater than 20% between days
#' result <- drop_large_weight_reductions(data, Weight_Day1, Weight_Day2, Weight_Day3, threshold = 20)
#'
#' # View cleaned data
#' result$Cleaned_Data
#'
#' # View dropped rows summary
#' result$Dropped_Summary

usethis::use_package("stringdist")
usethis::use_package("readxl")
usethis::use_package("stringr")

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
