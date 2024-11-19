# Install necessary packages (if not already installed)
if (!requireNamespace("zoo", quietly = TRUE)) install.packages("zoo")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")

library(zoo)
library(dplyr)

# Initialize a log file
log_file <- "data_cleaning_log.txt"
cat("Data Cleaning Log\n=================\n", file = log_file)

# Define the automated data cleaning function
clean_data <- function(data, 
                       na_method = "mean", 
                       handle_duplicates = TRUE, 
                       handle_outliers = TRUE, 
                       outlier_method = "IQR") {
  log_entry <- function(message) {
    cat(message, file = log_file, append = TRUE)
  }
  
  # Log initial dataset summary
  log_entry("Original Data Summary:\n")
  log_entry(capture.output(summary(data)))
  log_entry("\n")
  
  # 1. Handle Missing Values
  log_entry("Handling missing values...\n")
  data <- data %>%
    mutate(across(where(is.numeric), function(col) {
      missing_count <- sum(is.na(col))
      if (na_method == "mean") {
        col[is.na(col)] <- mean(col, na.rm = TRUE)
        log_entry(sprintf("Replaced %d missing values with column mean.\n", missing_count))
      } else if (na_method == "median") {
        col[is.na(col)] <- median(col, na.rm = TRUE)
        log_entry(sprintf("Replaced %d missing values with column median.\n", missing_count))
      } else if (na_method == "ffill") {
        col <- zoo::na.locf(col, na.rm = FALSE)
        log_entry(sprintf("Replaced %d missing values using forward fill.\n", missing_count))
      }
      return(col)
    }))
  
  # 2. Handle Duplicated Rows
  if (handle_duplicates) {
    log_entry("Checking for duplicates...\n")
    duplicate_count <- nrow(data) - nrow(distinct(data))
    if (duplicate_count > 0) {
      log_entry(sprintf("Removed %d duplicate rows.\n", duplicate_count))
      data <- data %>%
        distinct()
    } else {
      log_entry("No duplicates found.\n")
    }
  }
  
  # 3. Handle Outliers
  if (handle_outliers) {
    log_entry("Handling outliers...\n")
    data <- data %>%
      mutate(across(where(is.numeric), function(col) {
        if (outlier_method == "IQR") {
          q1 <- quantile(col, 0.25, na.rm = TRUE)
          q3 <- quantile(col, 0.75, na.rm = TRUE)
          iqr <- q3 - q1
          lower_bound <- q1 - 1.5 * iqr
          upper_bound <- q3 + 1.5 * iqr
          outlier_count <- sum(col < lower_bound | col > upper_bound, na.rm = TRUE)
          col[col < lower_bound | col > upper_bound] <- NA
          log_entry(sprintf("Replaced %d outliers with NA (IQR method).\n", outlier_count))
        } else if (outlier_method == "zscore") {
          z_scores <- scale(col, center = TRUE, scale = TRUE)
          outlier_count <- sum(abs(z_scores) > 3, na.rm = TRUE)
          col[abs(z_scores) > 3] <- NA
          log_entry(sprintf("Replaced %d outliers with NA (Z-score method).\n", outlier_count))
        }
        return(col)
      }))
    
    # Replace new NAs from outliers based on the specified method
    data <- data %>%
      mutate(across(where(is.numeric), function(col) {
        if (na_method == "mean") {
          col[is.na(col)] <- mean(col, na.rm = TRUE)
        } else if (na_method == "median") {
          col[is.na(col)] <- median(col, na.rm = TRUE)
        } else if (na_method == "ffill") {
          col <- zoo::na.locf(col, na.rm = FALSE)
        }
        return(col)
      }))
  }
  
  log_entry("Data cleaning completed.\n")
  
  # Log final dataset summary
  log_entry("Cleaned Data Summary:\n")
  log_entry(capture.output(summary(data)))
  log_entry("\n")
  
  return(data)
}

# Example Usage
# Create a sample dataset
set.seed(123)
example_data <- data.frame(
  id = c(1, 2, 3, 4, 5, 5), # Duplicate ID
  value1 = c(10, 15, NA, 100, 20, 20), # Missing and potential outlier
  value2 = c(NA, 30, 35, 200, 25, 25), # Missing and potential outlier
  value3 = c(5, NA, 10, 15, 10, 10) # Missing values
)

# View the original data
cat("Original Data:\n")
print(example_data)

# Apply the cleaning function
cleaned_data <- clean_data(example_data, 
                           na_method = "mean", 
                           handle_duplicates = TRUE, 
                           handle_outliers = TRUE, 
                           outlier_method = "IQR")

# View the cleaned data
cat("\nCleaned Data:\n")
print(cleaned_data)

# Inform user about the log file
cat("\nAll modifications have been logged to 'data_cleaning_log.txt'.\n")
