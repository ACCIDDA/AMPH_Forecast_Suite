#!/usr/bin/env Rscript
# Example script demonstrating fable data validation functions
# This script shows how to validate data before using it with fable models

library(AMPHForecastSuite)

cat("=== Fable Data Validation Examples ===\n\n")

# Example 1: Valid data
cat("Example 1: Validating valid data\n")
cat("-----------------------------------\n")
valid_data <- data.frame(
  date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 20),
  location = "US",
  value = rnorm(20, 100, 10)
)

cat("Data structure:\n")
print(head(valid_data))
cat("\n")

cat("Running validation...\n")
result <- validate_fable_input_data(valid_data, stop_on_error = FALSE, verbose = TRUE)
cat("\nValidation result:", result$valid, "\n\n")


# Example 2: Data with missing values
cat("\nExample 2: Validating data with missing values\n")
cat("-----------------------------------------------\n")
data_with_na <- data.frame(
  date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 20),
  location = c("US", NA, "US", "US", "US", NA, rep("US", 14)),
  value = rnorm(20, 100, 10)
)

cat("Running validation...\n")
result <- validate_fable_input_data(data_with_na, stop_on_error = FALSE, verbose = TRUE)
cat("\nValidation result:", result$valid)
cat("\nErrors found:", length(result$errors), "\n")
if (length(result$errors) > 0) {
  cat("Error messages:\n")
  for (err in result$errors) {
    cat("  -", err, "\n")
  }
}
cat("\n")


# Example 3: Data with irregular time intervals
cat("\nExample 3: Validating data with irregular intervals\n")
cat("----------------------------------------------------\n")
irregular_data <- data.frame(
  date = as.Date(c("2023-01-01", "2023-01-03", "2023-01-10", "2023-01-25", 
                   "2023-02-01", "2023-02-15")),
  location = "US",
  value = rnorm(6, 100, 10)
)

cat("Data structure:\n")
print(irregular_data)
cat("\n")

cat("Running validation...\n")
result <- validate_fable_input_data(irregular_data, stop_on_error = FALSE, verbose = TRUE)
cat("\nValidation result:", result$valid)
cat("\nErrors found:", length(result$errors), "\n")
if (length(result$errors) > 0) {
  cat("Error messages:\n")
  for (err in result$errors) {
    cat("  -", err, "\n")
  }
}
cat("\n")


# Example 4: Using individual validation functions
cat("\nExample 4: Using individual validation functions\n")
cat("-------------------------------------------------\n")
test_data <- data.frame(
  date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 10),
  location = "US",
  value = rnorm(10, 100, 10)
)

cat("1. Checking date column...\n")
date_result <- check_date_column(test_data, stop_on_error = FALSE)
cat("   Result:", date_result$valid, "\n")

cat("\n2. Checking numeric columns...\n")
numeric_result <- check_numeric_columns(test_data, stop_on_error = FALSE)
cat("   Result:", numeric_result$valid, "\n")

cat("\n3. Checking temporal regularity...\n")
regularity_result <- check_temporal_regularity(test_data, stop_on_error = FALSE)
cat("   Result:", regularity_result$valid, "\n")
if (regularity_result$valid && length(regularity_result$detected_intervals) > 0) {
  for (key in names(regularity_result$detected_intervals)) {
    interval_info <- regularity_result$detected_intervals[[key]]
    cat("   Detected interval for", key, ":", interval_info$interval_type, "\n")
  }
}

cat("\n4. Checking for missing values...\n")
missing_result <- check_missing_values(test_data, c("date", "location", "value"), 
                                      stop_on_error = FALSE)
cat("   Result:", missing_result$valid, "\n")

cat("\n5. Validating tsibble requirements...\n")
tsibble_result <- validate_tsibble_requirements(test_data, stop_on_error = FALSE)
cat("   Result:", tsibble_result$valid, "\n")


# Example 5: Multiple locations (keys)
cat("\n\nExample 5: Validating data with multiple locations\n")
cat("----------------------------------------------------\n")
multi_location_data <- data.frame(
  date = rep(seq.Date(as.Date("2023-01-01"), by = "week", length.out = 10), 3),
  location = rep(c("US", "CA", "NY"), each = 10),
  value = rnorm(30, 100, 10)
)

cat("Data summary:\n")
cat("  Total rows:", nrow(multi_location_data), "\n")
cat("  Unique locations:", length(unique(multi_location_data$location)), "\n")
cat("  Date range:", min(multi_location_data$date), "to", max(multi_location_data$date), "\n\n")

cat("Running validation...\n")
result <- validate_fable_input_data(multi_location_data, stop_on_error = FALSE, verbose = FALSE)
cat("Validation result:", result$valid, "\n")

cat("\nChecking temporal regularity for each location:\n")
regularity_result <- check_temporal_regularity(multi_location_data, stop_on_error = FALSE)
for (key in names(regularity_result$detected_intervals)) {
  interval_info <- regularity_result$detected_intervals[[key]]
  cat("  -", key, ":", interval_info$interval_type, "\n")
}


# Example 6: Custom column names
cat("\n\nExample 6: Validating data with custom column names\n")
cat("----------------------------------------------------\n")
custom_data <- data.frame(
  time = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 30),
  region = "Europe",
  cases = rpois(30, 100),
  deaths = rpois(30, 5)
)

cat("Data structure:\n")
print(head(custom_data))
cat("\n")

cat("Running validation with custom column names...\n")
result <- validate_fable_input_data(
  custom_data,
  date_col = "time",
  value_cols = c("cases", "deaths"),
  key_cols = "region",
  stop_on_error = FALSE,
  verbose = FALSE
)
cat("Validation result:", result$valid, "\n")
cat("Checks passed:", length(result$checks_passed), "\n")


cat("\n=== Validation Examples Complete ===\n")
