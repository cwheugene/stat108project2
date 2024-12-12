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
