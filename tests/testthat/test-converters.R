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

# Additional tests for edge cases and error handling

test_that("convert_hub_to_tsibble handles empty data", {
  skip_if_not_installed("tsibble")
  
  # Create empty data frame with correct structure
  empty_data <- data.frame(
    date = as.Date(character()),
    location = character(),
    value = numeric()
  )
  
  # Should handle empty data gracefully
  result <- convert_hub_to_tsibble(empty_data)
  expect_s3_class(result, "tbl_ts")
  expect_equal(nrow(result), 0)
})

test_that("convert_hub_to_epiestim handles location filtering", {
  skip_if_not_installed("EpiEstim")
  
  # Create test data with multiple locations
  hub_data <- data.frame(
    date = rep(seq.Date(as.Date("2023-01-01"), by = "day", length.out = 25), 2),
    location = rep(c("US", "CA"), each = 25),
    value = rpois(50, 100)
  )
  
  # Filter for US only
  result_us <- convert_hub_to_epiestim(hub_data, location_filter = "US")
  expect_equal(nrow(result_us), 25)
  
  # Filter for CA only
  result_ca <- convert_hub_to_epiestim(hub_data, location_filter = "CA")
  expect_equal(nrow(result_ca), 25)
})

test_that("convert_hub_to_epinow2 handles location filtering", {
  skip_if_not_installed("EpiNow2")
  
  # Create test data with multiple locations
  hub_data <- data.frame(
    date = rep(seq.Date(as.Date("2023-01-01"), by = "day", length.out = 25), 2),
    location = rep(c("US", "CA"), each = 25),
    value = rpois(50, 100)
  )
  
  # Filter for US only
  result_us <- convert_hub_to_epinow2(hub_data, location_filter = "US")
  expect_equal(nrow(result_us), 25)
  
  # Filter for CA only
  result_ca <- convert_hub_to_epinow2(hub_data, location_filter = "CA")
  expect_equal(nrow(result_ca), 25)
})

test_that("convert_hub_to_epiestim ensures integer incidence", {
  skip_if_not_installed("EpiEstim")
  
  # Create test data with decimal values
  hub_data <- data.frame(
    date = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 30),
    location = "US",
    value = rnorm(30, 100, 10)  # Decimal values
  )
  
  result <- convert_hub_to_epiestim(hub_data, location_filter = "US")
  
  # Check that I is integer type
  expect_type(result$I, "integer")
  
  # Check that values are rounded properly
  expect_true(all(result$I == as.integer(result$I)))
})

test_that("convert_hub_to_epinow2 ensures integer confirm", {
  skip_if_not_installed("EpiNow2")
  
  # Create test data with decimal values
  hub_data <- data.frame(
    date = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 30),
    location = "US",
    value = rnorm(30, 100, 10)  # Decimal values
  )
  
  result <- convert_hub_to_epinow2(hub_data, location_filter = "US")
  
  # Check that confirm is integer type
  expect_type(result$confirm, "integer")
  
  # Check that values are rounded properly
  expect_true(all(result$confirm == as.integer(result$confirm)))
})

test_that("convert_hub_to_tsibble handles custom column names", {
  skip_if_not_installed("tsibble")
  
  # Create test data with non-default column names
  hub_data <- data.frame(
    my_date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 10),
    my_location = "US",
    my_value = rnorm(10, 100, 10)
  )
  
  result <- convert_hub_to_tsibble(
    hub_data,
    date_col = "my_date",
    value_col = "my_value",
    location_col = "my_location"
  )
  
  expect_s3_class(result, "tbl_ts")
  expect_true("my_date" %in% names(result))
  expect_true("my_value" %in% names(result))
})

test_that("convert_hub_to_epiestim handles custom column names", {
  skip_if_not_installed("EpiEstim")
  
  # Create test data with non-default column names
  hub_data <- data.frame(
    my_date = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 30),
    my_location = "US",
    my_value = rpois(30, 100)
  )
  
  result <- convert_hub_to_epiestim(
    hub_data,
    date_col = "my_date",
    incidence_col = "my_value",
    location_col = "my_location",
    location_filter = "US"
  )
  
  expect_true("dates" %in% names(result))
  expect_true("I" %in% names(result))
  expect_equal(nrow(result), 30)
})

test_that("convert_hub_to_epinow2 handles custom column names", {
  skip_if_not_installed("EpiNow2")
  
  # Create test data with non-default column names
  hub_data <- data.frame(
    my_date = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 30),
    my_location = "US",
    my_value = rpois(30, 100)
  )
  
  result <- convert_hub_to_epinow2(
    hub_data,
    date_col = "my_date",
    cases_col = "my_value",
    location_col = "my_location",
    location_filter = "US"
  )
  
  expect_true("date" %in% names(result))
  expect_true("confirm" %in% names(result))
  expect_equal(nrow(result), 30)
})

test_that("convert_hub_to_fable is alias for convert_hub_to_tsibble", {
  skip_if_not_installed("tsibble")
  
  hub_data <- data.frame(
    date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 10),
    location = "US",
    value = rnorm(10, 100, 10)
  )
  
  result_tsibble <- convert_hub_to_tsibble(hub_data)
  result_fable <- convert_hub_to_fable(hub_data)
  
  # Both should produce identical results
  expect_equal(result_tsibble, result_fable)
})

test_that("convert_fable_to_hub handles location parameter", {
  skip_if_not_installed("fable")
  skip_if_not_installed("tsibble")
  
  # Create a simple fable-like data frame
  fable_data <- data.frame(
    date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 4),
    value = rnorm(4, 100, 10),
    .mean = rnorm(4, 100, 10)
  )
  
  result <- convert_fable_to_hub(fable_data, location = "US", target = "inc")
  
  # Check that location and target columns are added
  expect_true("location" %in% names(result))
  expect_true("target" %in% names(result))
  expect_true(all(result$location == "US"))
  expect_true(all(result$target == "inc"))
})

test_that("convert_epiestim_to_hub requires estimate_R object", {
  skip_if_not_installed("EpiEstim")
  
  # Test with non-estimate_R object
  fake_object <- list(R = data.frame(a = 1, b = 2))
  
  expect_error(
    convert_epiestim_to_hub(fake_object),
    "must be an EpiEstim estimate_R result object"
  )
})

test_that("convert_epiestim_to_hub handles location and target parameters", {
  skip_if_not_installed("EpiEstim")
  
  # Create a mock EpiEstim result structure
  mock_result <- list(
    R = data.frame(
      t_end = 1:10,
      `Mean(R)` = rnorm(10, 1.5, 0.2),
      `Quantile.0.025(R)` = rnorm(10, 1.2, 0.1),
      `Quantile.0.975(R)` = rnorm(10, 1.8, 0.1),
      check.names = FALSE
    )
  )
  class(mock_result) <- "estimate_R"
  
  result <- convert_epiestim_to_hub(mock_result, location = "CA", target = "Rt")
  
  expect_true("location" %in% names(result))
  expect_true("target" %in% names(result))
  expect_equal(unique(result$location), "CA")
  expect_equal(unique(result$target), "Rt")
  expect_equal(nrow(result), 10)
})

test_that("convert_epinow2_to_hub warns on non-epinow object", {
  skip_if_not_installed("EpiNow2")
  
  # Create a fake object that's not an epinow result
  fake_result <- list(predictions = data.frame(date = Sys.Date(), median = 100))
  
  expect_warning(
    convert_epinow2_to_hub(fake_result),
    "may not be an EpiNow2 epinow result object"
  )
})

test_that("convert_hub_to_epiestim sorts data by date", {
  skip_if_not_installed("EpiEstim")
  
  # Create unsorted data
  hub_data <- data.frame(
    date = as.Date(c("2023-01-10", "2023-01-05", "2023-01-15", "2023-01-01")),
    location = "US",
    value = c(100, 80, 120, 90)
  )
  
  result <- convert_hub_to_epiestim(hub_data, location_filter = "US")
  
  # Check that dates are sorted
  expect_true(all(diff(result$dates) > 0))
})

test_that("convert_hub_to_epinow2 sorts data by date", {
  skip_if_not_installed("EpiNow2")
  
  # Create unsorted data
  hub_data <- data.frame(
    date = as.Date(c("2023-01-10", "2023-01-05", "2023-01-15", "2023-01-01")),
    location = "US",
    value = c(100, 80, 120, 90)
  )
  
  result <- convert_hub_to_epinow2(hub_data, location_filter = "US")
  
  # Check that dates are sorted
  expect_true(all(diff(result$date) > 0))
})
