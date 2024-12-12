#' Fix Mismatched Gender Patterns in IDs
#'
#' This function identifies and corrects single mismatched gender entries in a specified column
#' containing IDs. It assumes that IDs follow the pattern `Prefix_Gender_Number`, where `Gender`
#' is represented by a single character (e.g., "M" or "F"). Mismatches are resolved by aligning
#' the gender with the previous or next entry in the sequence.
#'
#' @param data A data frame containing the column to process.
#' @param column_name A character string specifying the name of the column to fix.
#' Defaults to `"ID"`.
#'
#' @return A modified data frame with corrected IDs in the specified column.
#'
#' @export
#'
#' @examples
#' # Example dataset
#' data <- data.frame(
#'   ID = c("C57BL6J_M_1", "MC57BL6J_M_2", "C57BL6J_F_3", "C57BL6J_M_4"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Fix mismatched genders in the 'ID' column
#' corrected_data <- fix_gender_pattern(data, column_name = "ID")
#'
#' # View the corrected data
#' print(corrected_data)
library(dplyr)
fix_gender_pattern <- function(data, column_name = "ID") {
  # Ensure the specified column exists
  if (!column_name %in% colnames(data)) {
    stop("Specified column does not exist in the data.")
  }

  # Extract the column
  ids <- data[[column_name]]

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
  data[[column_name]] <- parsed_ids$Fixed_ID
  return(data)
}
