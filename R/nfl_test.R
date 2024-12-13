#Functions for NFL data
#1. na_summary: returns table summary of all the na values in the dataset
#2. visualize_outliers: pair plots numeric columns and visualizes possible outliers in red
#3. true_yardline: corrects for the way the NFL counts yardline (calculates true distance from the team's endzone to current position)



#' Provides a quick overview of how missing values are distributed across the dataset.
#' @name `na_summary`
#' @param data A NFL Dataset
#' @return Number of NAs in each column, percentage of NAs, how many NAs are row unique, and mean and SD for numeric columns
#' @import dplyr
# Ensure data is a data frame

na_summary <- function(data) {
  # Ensure data is a data frame
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }

  # Initialize results as a list
  results <- list()

  # Loop through each column to calculate statistics
  for (col in names(data)) {
    col_data <- data[[col]]

    # Number of NAs in the column
    num_nas <- sum(is.na(col_data))

    # Percentage of NAs
    percentage_nas <- (num_nas / nrow(data)) * 100

    # Number of row-unique NAs
    row_unique_nas <- sum(rowSums(is.na(data)) == 1 & is.na(col_data))

    # Column mean and standard deviation
    if (is.numeric(col_data)) {
      col_mean <- as.character(mean(col_data, na.rm = TRUE))
      col_sd <- as.character(sd(col_data, na.rm = TRUE))
    } else {
      col_mean <- "not numeric"
      col_sd <- "not numeric"
    }

    # Append statistics for the column
    results[[col]] <- tibble(
      Column = col,
      Number_of_NAs = num_nas,
      Percentage_of_NAs = percentage_nas,
      Row_Unique_NAs = row_unique_nas,
      Column_Mean = col_mean,
      Column_SD = col_sd
    )
  }

  # Convert results list to a tibble
  results_tibble <- dplyr::bind_rows(results)

  # Arrange by highest number of NAs
  results_tibble <- results_tibble %>%
    dplyr::arrange(desc(Number_of_NAs))

  return(results_tibble)
}



#' Provides a visualization the numeric data
#' @name visualize_outliers
#' @param df2 A NFL Dataset
#' @param z_threshold Cutoff for outlier detection
#' @param ... Dataframe columns
#' @return returns a plot that shows the distribution of each of the columns, and computes outliers using z-score and plots in red
#' @import patchwork
#' @import ggplot2
# Ensure data is a data frame


visualize_outliers <- function(df2, ..., z_threshold = 3) {
  # Ensure the input is a data frame
  if (!is.data.frame(df2)) {
    return(paste(
      "Input df2 must be a data frame. The input type is:", class(df2)[1]
    ))
  }

  # Capture the column names passed as arguments
  columns <- enquos(...)

  # Check if at least two columns are provided
  if (length(columns) < 2) {
    stop("At least two columns must be specified.")
  }

  # Select the specified columns
  selected_data <- df2 %>%
    select(!!!columns)

  # Convert all columns to numeric
  numeric_data <- selected_data %>%
    mutate(across(everything(), ~ as.numeric(.), .names = "{col}"))

  # Check for non-convertible columns
  if (any(sapply(numeric_data, function(col) all(is.na(col))))) {
    stop("One or more columns could not be converted to numeric. Check for non-numeric values.")
  }

  # Add an outlier flag for each numeric column
  numeric_data <- numeric_data %>%
    mutate(across(
      everything(),
      ~ ifelse(
        abs((. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)) > z_threshold,
        "Outlier", "Normal"
      ),
      .names = "outlier_{col}"
    ))

  # Get column names for looping
  column_names <- colnames(selected_data)
  n <- length(column_names)

  # Initialize an empty matrix to store plots
  plots <- matrix(list(), n, n)

  # Generate plots
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) {
        # Diagonal: Histograms
        p <- ggplot(numeric_data, aes(x = .data[[column_names[i]]])) +
          geom_histogram(bins = 20, fill = "blue", alpha = 0.7) +
          theme_minimal() +
          labs(x = column_names[i], y = "Count") +
          theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
      } else {
        # Off-diagonal: Scatterplots with outlier coloring
        x_var <- column_names[j]
        y_var <- column_names[i]
        y_outlier_col <- paste0("outlier_", y_var)

        # Scatterplot with outliers in red
        p <- ggplot(numeric_data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
          geom_point(aes(color = .data[[y_outlier_col]]), alpha = 0.7) +
          scale_color_manual(values = c("Outlier" = "red", "Normal" = "black")) +
          theme_minimal() +
          labs(x = x_var, y = y_var, color = "Legend") +
          theme(legend.position = "none")
      }
      # Store plot in the matrix
      plots[i, j] <- list(p)
    }
  }

  # Combine plots into a grid using patchwork
  plot_grid <- wrap_plots(plots, byrow = FALSE)

  return(plot_grid)
}
visualize_outliers(df, "yardsToGo", "PassLength", "YardsAfterCatch")


#' Corrects for the way the NFL counts yardline by calculates true distance from the team's endzone to current position
#' @name true_yardline
#' @param data A dataframe containing possession yardline
#' @param possession_col Column marking which team has possession
#' @param yardline_side_col Column marking which team's yardline is the ball on
#' @param yardline_number_col Column marking the yardline number that the ball is on
#NFL data idiosyncrasy: the numbering only goes up to 50 yards and then switches when you get to opponents side.
#This computes the "true yardline"
true_yardline <- function(data, possession_col, yardline_side_col, yardline_number_col) {
  # Ensure the input is a data frame
  if (!is.data.frame(data)) {
    return(paste(
      "Input data must be a data frame. The input type is:", class(data)[1]
    ))
  }

  # Ensure required columns are in the data
  if (!all(c(possession_col, yardline_side_col, yardline_number_col) %in% colnames(data))) {
    missing_cols <- setdiff(c(possession_col, yardline_side_col, yardline_number_col), colnames(data))
    stop(paste("The following columns are missing from the data:", paste(missing_cols, collapse = ", ")))
  }

  # Calculate true yardline
  yardline <- data %>%
    mutate(
      yardline = ifelse(
        .data[[possession_col]] == .data[[yardline_side_col]],
        .data[[yardline_number_col]],        # Offensive team is on its own side
        100 - .data[[yardline_number_col]]  # Offensive team is on the opponent's side
      )
    ) %>%
    select(yardline) # Select only the "true_yardline" column

  return(as_tibble(yardline))
}
true_yardline(df, "possessionTeam", "yardlineSide", "yardlineNumber")
