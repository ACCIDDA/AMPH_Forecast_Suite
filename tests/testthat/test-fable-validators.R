test_that("validate_fable_input_data works with valid data", {
  # Create valid test data
  hub_data <- data.frame(
    date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 10),
    location = "US",
    value = rnorm(10, 100, 10)
  )
  
  # Should pass without error
  expect_true(validate_fable_input_data(hub_data, verbose = FALSE))
  
  # Check non-stop mode returns list
  result <- validate_fable_input_data(hub_data, stop_on_error = FALSE, verbose = FALSE)
  expect_type(result, "list")
  expect_true(result$valid)
  expect_equal(length(result$errors), 0)
})

test_that("validate_fable_input_data catches non-data-frame input", {
  # Test with non-data-frame
  expect_error(
    validate_fable_input_data(c(1, 2, 3), verbose = FALSE),
    "Input must be a data frame"
  )
  
  # Test non-stop mode
  result <- validate_fable_input_data(c(1, 2, 3), stop_on_error = FALSE, verbose = FALSE)
  expect_false(result$valid)
  expect_true(any(grepl("data frame", result$errors)))
})

test_that("validate_fable_input_data catches empty data", {
  # Create empty data frame
  empty_data <- data.frame(date = as.Date(character(0)), location = character(0), value = numeric(0))
  
  expect_error(
    validate_fable_input_data(empty_data, verbose = FALSE),
    "empty"
  )
})

test_that("validate_fable_input_data catches missing columns", {
  # Create data without required columns
  incomplete_data <- data.frame(
    date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 10),
    value = rnorm(10)
    # Missing 'location' column
  )
  
  expect_error(
    validate_fable_input_data(incomplete_data, verbose = FALSE),
    "Missing required columns"
  )
})

test_that("check_date_column validates date format", {
  # Valid date column
  valid_data <- data.frame(
    date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 10),
    value = rnorm(10)
  )
  
  expect_true(check_date_column(valid_data, verbose = FALSE))
  
  # Invalid date column (character that can't be converted)
  invalid_data <- data.frame(
    date = c("not", "a", "date"),
    value = rnorm(3)
  )
  
  result <- check_date_column(invalid_data, stop_on_error = FALSE)
  expect_false(result$valid)
})

test_that("check_date_column catches NA values", {
  # Data with NA dates
  na_data <- data.frame(
    date = c(as.Date("2023-01-01"), NA, as.Date("2023-01-15")),
    value = rnorm(3)
  )
  
  result <- check_date_column(na_data, stop_on_error = FALSE)
  expect_false(result$valid)
  expect_true(any(grepl("NA values", result$errors)))
})

test_that("check_date_column handles convertible date formats", {
  # Character dates that can be converted
  char_date_data <- data.frame(
    date = c("2023-01-01", "2023-01-08", "2023-01-15"),
    value = rnorm(3)
  )
  
  result <- check_date_column(char_date_data, stop_on_error = FALSE)
  expect_true(result$valid)
  expect_true(any(grepl("can be converted", result$warnings)))
})

test_that("check_numeric_columns validates numeric data", {
  # Valid numeric column
  valid_data <- data.frame(
    date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 10),
    value = rnorm(10, 100, 10)
  )
  
  expect_true(check_numeric_columns(valid_data, verbose = FALSE))
  
  # Non-numeric column
  invalid_data <- data.frame(
    date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 10),
    value = letters[1:10]
  )
  
  result <- check_numeric_columns(invalid_data, stop_on_error = FALSE)
  expect_false(result$valid)
  expect_true(any(grepl("not numeric", result$errors)))
})

test_that("check_numeric_columns detects infinite values", {
  # Data with infinite values
  inf_data <- data.frame(
    date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 5),
    value = c(1, 2, Inf, 4, 5)
  )
  
  result <- check_numeric_columns(inf_data, stop_on_error = FALSE)
  expect_false(result$valid)
  expect_true(any(grepl("infinite", result$errors)))
})

test_that("check_numeric_columns warns about negative values", {
  # Data with negative values
  neg_data <- data.frame(
    date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 5),
    value = c(-1, 2, 3, 4, 5)
  )
  
  result <- check_numeric_columns(neg_data, stop_on_error = FALSE)
  expect_true(result$valid)  # Valid but with warning
  expect_true(any(grepl("negative", result$warnings)))
})

test_that("check_numeric_columns warns about zero variance", {
  # Data with all same values
  const_data <- data.frame(
    date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 5),
    value = rep(100, 5)
  )
  
  result <- check_numeric_columns(const_data, stop_on_error = FALSE)
  expect_true(result$valid)  # Valid but with warning
  expect_true(any(grepl("zero variance", result$warnings)))
})

test_that("check_missing_values detects NA values", {
  # Data without NAs
  clean_data <- data.frame(
    date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 10),
    location = "US",
    value = rnorm(10)
  )
  
  expect_true(check_missing_values(clean_data, c("date", "location", "value"), verbose = FALSE))
  
  # Data with NAs
  na_data <- data.frame(
    date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 10),
    location = c("US", NA, "US", "US", "US", NA, "US", "US", "US", "US"),
    value = rnorm(10)
  )
  
  result <- check_missing_values(na_data, c("date", "location", "value"), stop_on_error = FALSE)
  expect_false(result$valid)
  expect_true(any(grepl("missing values", result$errors)))
})

test_that("check_missing_values can allow NA in value columns", {
  # Data with NAs in value column
  na_value_data <- data.frame(
    date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 10),
    location = "US",
    value = c(1, 2, NA, 4, 5, 6, 7, 8, 9, 10)
  )
  
  # With allow_value_na = FALSE (default), should fail
  result1 <- check_missing_values(na_value_data, c("date", "location", "value"), 
                                 stop_on_error = FALSE, allow_value_na = FALSE)
  expect_false(result1$valid)
  
  # With allow_value_na = TRUE, should pass with warning
  result2 <- check_missing_values(na_value_data, c("date", "location", "value"), 
                                 stop_on_error = FALSE, allow_value_na = TRUE)
  expect_true(result2$valid)
  expect_true(any(grepl("missing values", result2$warnings)))
})

test_that("validate_tsibble_requirements detects duplicate key-index combinations", {
  # Valid data with unique combinations
  valid_data <- data.frame(
    date = rep(seq.Date(as.Date("2023-01-01"), by = "week", length.out = 5), 2),
    location = rep(c("US", "CA"), each = 5),
    value = rnorm(10)
  )
  
  expect_true(validate_tsibble_requirements(valid_data, verbose = FALSE))
  
  # Invalid data with duplicates
  dup_data <- data.frame(
    date = c(as.Date("2023-01-01"), as.Date("2023-01-01")),
    location = c("US", "US"),
    value = c(100, 200)
  )
  
  result <- validate_tsibble_requirements(dup_data, stop_on_error = FALSE)
  expect_false(result$valid)
  expect_true(any(grepl("duplicate", result$errors)))
})

test_that("validate_tsibble_requirements warns about unsorted index", {
  # Unsorted data
  unsorted_data <- data.frame(
    date = as.Date(c("2023-01-15", "2023-01-01", "2023-01-08")),
    location = "US",
    value = rnorm(3)
  )
  
  result <- validate_tsibble_requirements(unsorted_data, stop_on_error = FALSE)
  expect_true(result$valid)  # Valid but with warning
  expect_true(any(grepl("not sorted", result$warnings)))
})

test_that("check_temporal_regularity detects regular intervals", {
  # Weekly data
  weekly_data <- data.frame(
    date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 10),
    location = "US",
    value = rnorm(10)
  )
  
  result <- check_temporal_regularity(weekly_data, stop_on_error = FALSE)
  expect_true(result$valid)
  expect_equal(result$detected_intervals$US$interval_type, "weekly")
  
  # Daily data
  daily_data <- data.frame(
    date = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 10),
    location = "US",
    value = rnorm(10)
  )
  
  result <- check_temporal_regularity(daily_data, stop_on_error = FALSE)
  expect_true(result$valid)
  expect_equal(result$detected_intervals$US$interval_type, "daily")
})

test_that("check_temporal_regularity detects irregular intervals", {
  # Irregular data
  irregular_data <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-03", "2023-01-10", "2023-01-25")),
    location = "US",
    value = rnorm(4)
  )
  
  result <- check_temporal_regularity(irregular_data, stop_on_error = FALSE)
  expect_false(result$valid)
  expect_true(any(grepl("irregular", result$errors)))
})

test_that("check_temporal_regularity handles multiple keys", {
  # Multiple locations with regular data
  multi_key_data <- data.frame(
    date = rep(seq.Date(as.Date("2023-01-01"), by = "week", length.out = 5), 2),
    location = rep(c("US", "CA"), each = 5),
    value = rnorm(10)
  )
  
  result <- check_temporal_regularity(multi_key_data, stop_on_error = FALSE)
  expect_true(result$valid)
  expect_true("US" %in% names(result$detected_intervals))
  expect_true("CA" %in% names(result$detected_intervals))
  expect_equal(result$detected_intervals$US$interval_type, "weekly")
  expect_equal(result$detected_intervals$CA$interval_type, "weekly")
})

test_that("check_temporal_regularity handles monthly data with variation", {
  # Monthly data (with natural variation in days per month)
  monthly_dates <- seq.Date(as.Date("2023-01-01"), by = "month", length.out = 6)
  monthly_data <- data.frame(
    date = monthly_dates,
    location = "US",
    value = rnorm(6)
  )
  
  result <- check_temporal_regularity(monthly_data, stop_on_error = FALSE)
  # Should be valid with possible warnings about near-regularity
  expect_true(result$valid)
})

test_that("validate_fable_input_data provides comprehensive validation", {
  # Create data with multiple issues
  problematic_data <- data.frame(
    date = c(as.Date("2023-01-01"), as.Date("2023-01-01"), as.Date("2023-01-15")),
    location = c("US", "US", NA),
    value = c(100, 200, 300)
  )
  
  result <- validate_fable_input_data(problematic_data, stop_on_error = FALSE, verbose = FALSE)
  expect_false(result$valid)
  expect_true(length(result$errors) > 0)
})

test_that("validate_fable_input_data works with custom column names", {
  # Custom column names
  custom_data <- data.frame(
    time = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 10),
    region = "US",
    cases = rpois(10, 100)
  )
  
  result <- validate_fable_input_data(
    custom_data,
    date_col = "time",
    value_cols = "cases",
    key_cols = "region",
    stop_on_error = FALSE,
    verbose = FALSE
  )
  
  expect_true(result$valid)
})

test_that("validate_fable_input_data handles multiple value columns", {
  # Multiple value columns
  multi_value_data <- data.frame(
    date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 10),
    location = "US",
    cases = rpois(10, 100),
    deaths = rpois(10, 5)
  )
  
  result <- validate_fable_input_data(
    multi_value_data,
    value_cols = c("cases", "deaths"),
    stop_on_error = FALSE,
    verbose = FALSE
  )
  
  expect_true(result$valid)
})
