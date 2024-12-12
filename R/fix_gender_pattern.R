fix_gender_pattern <- function(data, column_name) {
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
