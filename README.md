
<!-- README.md is generated from README.Rmd. Please edit that file -->

# whiskerTrimmer (or some other name idk)

<!-- badges: start -->
<!-- badges: end -->

# Overview

The Mouse Vaccine Trial Package is designed to handle data cleaning and
preparation for studies involving longitudinal data from pre-clinical
vaccine trials. These studies often generate complex datasets that
include repeated measures, such as body weights or outcomes over time,
alongside metadata like treatment groups and subject IDs. This package
helps organise and standardise these data-sets into a single, clean
format for further analysis.

One common challenge in such studies is dealing with inconsistencies in
data entry, such as swapped columns, mismatched IDs, or unexpected
outliers. This package provides functions to address these issues
systematically. For example, it can rename columns based on position,
detect and handle outliers, and merge multiple datasets by a common
identifier. These tools allow users to focus on analyzing data rather
than troubleshooting errors.

Another benefit is its ability to handle missing or incomplete data. It
provides clear warnings about problematic rows and ensures that the
merged dataset retains only relevant and valid entries. By structuring
the data with a consistent format and resolving common errors, the
package prepares the data for statistical analysis or visualisation in R
or other tools. It also comes in-built with functions to directly
visualise data and find summary statistics, which provide an easy way of
making initial sense of experimental results.

The package is also versatile, working with data stored in Excel files.
It expects three sheets - “Birth,” “Body Weight,” and “Outcome” - and
uses a streamlined process to clean and merge these into a single,
analysis-ready dataset with standardised column names.

Overall, this package is a helpful tool for researchers managing mouse
vaccine trial data, providing a reliable way to transform raw datasets
into formats suitable for analysis. It is particularly useful when
dealing with large datasets or complex study designs, where manual
processing would be time-consuming and error-prone.

TL;DR: • Automated Data Wrangling: Consolidates raw data from multiple
sources into an analysis-ready format. • Quality Assurance: Flags
anomalies such as swapped values or outliers, ensuring data integrity. •
Visualisation and Summaries: Generates statistical summaries, tables,
and visualizations for interim reports. • Customisable: Handles unique
dataset structures, such as custom treatment groups or specific quirks
in your data. • Reproducibility: Automates cleaning and wrangling
processes for consistent results across data cuts.

# Installation

You can install the development version of the package like so:

1.  Install the devtools package

``` r
install.packages("devtools")
```

2.  Install the Mouse Vaccine Trial package from GitHub:

``` r
devtools::install_github("cwheugene/mouse_trial_package")
```

3.  Load the package in your R session:

``` r
library(mouse_trial_package)
```

4.  Verify installation by checking the documentation:

``` r
?mouse_trial_package
```

# Using the Package

## Step 1: Clean Your Data

Place your raw Excel file (e.g., mousedata.xlsx) in your working
directory or a designated data-raw folder. Use the clean_mouse_data()
function to process it:

The function expects three Excel sheets, with headings in the following
order: 1. Birth: ID, Sex, Num, Treatment 2. Body Weight: ID, Body Weight
1, Date Body Weight 1, Body Weight 2, Date Body Weight 2, Body Weight 3,
Date Body Weight 3 3. Outcome: ID, Outcome 1, Date Outcome 1, Outcome 2,
Date Outcome 2, Outcome 3, Date Outcome 3

The data can be cleaned and viewed with the following code:

``` r

# Set up the file path to the Excel document
file_path <- "data-raw/mousedata.xlsx"

# Clean the data with the function in the package
cleaned_data <- clean_mouse_data(file_path)

# View the cleaned data (in the form of a tibble)
View(cleaned_data)
```

The function automatically merges the three Excel sheets into a tidy
dataset ready for analysis.

## Step 2: Generate Statistical Summaries

# to be edited

Use built-in functions to summarise data: • Create a summary table of
weight changes by treatment group:

summary_table \<- summarize_weights(cleaned_data) print(summary_table)

    •   Flag and view significant weight changes:

flagged_data \<- flag_weight_loss(cleaned_data) View(flagged_data)

## Step 3: Create Visualistions

Visualize trends in the data, such as weight changes over time, using
built-in plotting functions:

Plot weight trends by treatment group

``` r
plot_weight_trends(cleaned_data)
```

# Support

If you encounter any issues or have questions, please submit a GitHub
issue in the repository or contact the maintainer at
<your_email@example.com>.

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.