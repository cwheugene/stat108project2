#' Check and Correct Misspellings in a Column
#'
#' This function identifies and corrects potential misspellings in a specified column
#' of a data frame by comparing less frequent values to more common ones. Values with
#' a string similarity score above the specified threshold are replaced with their
#' closest match from the most frequent values.
#'
#' @param data A data frame containing the column to check for misspellings.
#' @param column_name A character string specifying the name of the column to process.
#' Defaults to "treatment".
#' @param frequency_threshold A numeric value indicating the minimum frequency for a
#' value to be considered "common." Defaults to 3.
#' @param similarity_threshold A numeric value between 0 and 1 indicating the minimum
#' similarity score required for a replacement to occur. Defaults to 0.8.
#'
#' @return A data frame with corrected values in the specified column.
#'
#' @export
#'
#' @examples
#' # Example dataset
#' data <- data.frame(
#'   treatment = c("Placebo", "Placebo", "Plcebo", "Vacine", "Vaccine", "Vaccine"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Correct misspellings in the 'treatment' column
#' corrected_data <- check_misspellings(data, column_name = "treatment",
#'                                      frequency_threshold = 2, similarity_threshold = 0.8)
#'
#' # View the corrected data
#' print(corrected_data)

check_misspellings <- function(data, column_name = "treatment", frequency_threshold = 3, similarity_threshold = 0.8) {
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
