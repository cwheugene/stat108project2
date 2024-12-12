check_misspellings <- function(data, column_name = "treatment", frequency_threshold = 3, similarity_threshold = 0.8) {
  library(dplyr)
  library(stringdist)

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

  # Initialize a mapping of replacements
  replacements <- list()

  # Compare less common values to common values
  for (less_common in less_common_values) {
    for (common in common_values) {
      # Calculate string similarity
      similarity <- stringdist::stringsim(less_common, common, method = "jw")

      # Replace value if similarity exceeds threshold
      if (!is.na(similarity) && similarity > similarity_threshold) {
        replacements[[less_common]] <- common
        break
      }
    }
  }

  # Replace values in the dataset
  data[[column_name]] <- data[[column_name]] %>%
    sapply(function(value) {
      if (value %in% names(replacements)) {
        replacements[[value]]
      } else {
        value
      }
    })

  # Return the modified dataset
  return(data)
}
