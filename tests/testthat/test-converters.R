test_that("convert_hub_to_tsibble works with basic data", {
  skip_if_not_installed("tsibble")
  
  # Create test data
  hub_data <- data.frame(
    date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 10),
    location = "US",
    value = rnorm(10, 100, 10)
  )
  
  # Convert to tsibble
  result <- convert_hub_to_tsibble(hub_data)
  
  # Check that result is a tsibble
  expect_s3_class(result, "tbl_ts")
  
  # Check that columns exist
  expect_true("date" %in% names(result))
  expect_true("location" %in% names(result))
  expect_true("value" %in% names(result))
})

test_that("convert_hub_to_epiestim formats data correctly", {
  skip_if_not_installed("EpiEstim")
  
  # Create test data
  hub_data <- data.frame(
    date = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 50),
    location = "US",
    value = rpois(50, 100)
  )
  
  # Convert to EpiEstim format
  result <- convert_hub_to_epiestim(hub_data, location_filter = "US")
  
  # Check that required columns exist
  expect_true("dates" %in% names(result))
  expect_true("I" %in% names(result))
  
  # Check that I is integer
  expect_type(result$I, "integer")
  
  # Check that dates are Date type
  expect_s3_class(result$dates, "Date")
})

test_that("convert_hub_to_epinow2 formats data correctly", {
  skip_if_not_installed("EpiNow2")
  
  # Create test data
  hub_data <- data.frame(
    date = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 50),
    location = "US",
    value = rpois(50, 100)
  )
  
  # Convert to EpiNow2 format
  result <- convert_hub_to_epinow2(hub_data, location_filter = "US")
  
  # Check that required columns exist
  expect_true("date" %in% names(result))
  expect_true("confirm" %in% names(result))
  
  # Check that confirm is integer
  expect_type(result$confirm, "integer")
  
  # Check that date is Date type
  expect_s3_class(result$date, "Date")
})

test_that("convert_tsibble_to_hub converts back to data frame", {
  skip_if_not_installed("tsibble")
  
  # Create test data and convert to tsibble
  hub_data <- data.frame(
    date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 10),
    location = "US",
    value = rnorm(10, 100, 10)
  )
  
  ts_data <- convert_hub_to_tsibble(hub_data)
  
  # Convert back
  result <- convert_tsibble_to_hub(ts_data)
  
  # Check that result is a data frame
  expect_s3_class(result, "data.frame")
  
  # Check that it has the expected number of rows
  expect_equal(nrow(result), nrow(hub_data))
})
