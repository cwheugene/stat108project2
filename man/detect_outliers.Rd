% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/detect_outliers.R
\name{detect_outliers}
\alias{detect_outliers}
\title{Detect and Handle Outliers in Numeric Data}
\usage{
detect_outliers(
  data,
  ...,
  method = "mean",
  threshold = 2.2,
  drop_outliers = FALSE
)
}
\arguments{
\item{data}{A data frame containing the columns to evaluate.}

\item{...}{One or more numeric columns to check for outliers.}

\item{method}{A character string specifying the method to use for detecting outliers.
Options are `"mean"` for the Z-score method or `"median"` for the IQR method.
Defaults to `"mean"`.}

\item{threshold}{A numeric value specifying the threshold for identifying outliers.
For the Z-score method, this represents the number of standard deviations from the mean
(default: 2.2). For the IQR method, this represents the multiplier for the interquartile range.}

\item{drop_outliers}{A logical value. If `TRUE`, outliers are replaced with `NA` in the
returned cleaned data frame. Defaults to `FALSE`.}
}
\value{
If `drop_outliers = FALSE`, returns a list of data frames, each containing
information about the detected outliers for a specified column:
\item{Row}{The row numbers of the outliers.}
\item{Value}{The original values of the outliers.}
\item{Z_Score}{(For the Z-score method) The Z-scores of the outliers.}
\item{Distance_to_Bound}{(For the IQR method) The distance of the outliers from the IQR bounds.}

If `drop_outliers = TRUE`, returns a list with:
\item{Results}{As described above.}
\item{Cleaned_Data}{A data frame with outliers replaced by `NA`.}
}
\description{
This function detects outliers in specified numeric columns of a data frame
using either the Z-score (mean-based) or IQR (median-based) method. Optionally,
outliers can be replaced with `NA`.
}
\examples{
# Example dataset
data <- data.frame(
  Weight_Day1 = c(50, 60, 55, 100, 45),
  Weight_Day2 = c(48, 58, 53, 110, 43),
  stringsAsFactors = FALSE
)

# Detect outliers using the Z-score method
outlier_results <- detect_outliers(data, Weight_Day1, Weight_Day2, method = "mean", threshold = 2.2)

# View results
outlier_results$Weight_Day1

# Detect outliers using the IQR method and drop them
cleaned_results <- detect_outliers(data, Weight_Day1, Weight_Day2, method = "median", threshold = 1.5, drop_outliers = TRUE)

# View cleaned data
cleaned_results$Cleaned_Data
}
