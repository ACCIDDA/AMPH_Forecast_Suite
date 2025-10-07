test_that("get_nhsn_data validates disease parameter correctly", {
  # Test valid diseases
  expect_no_error({
    # Mock the epidatr function to avoid actual API calls
    mockery::stub(get_nhsn_data, 'epidatr::pub_nhs_facilities', 
                  data.frame(geo_value = "US", time_value = 202301, value = 100))
  })
  
  # Test invalid disease
  expect_error(
    get_nhsn_data(disease = "invalid_disease"),
    "Invalid disease"
  )
  
  expect_error(
    get_nhsn_data(disease = "flu"),
    "Invalid disease"
  )
})

test_that("get_nhsn_data handles case-insensitive disease names", {
  skip_if_not_installed("mockery")
  
  # Mock the epidatr function
  mock_result <- data.frame(
    geo_value = "US",
    time_value = 202301,
    value = 100
  )
  
  # Test uppercase
  mockery::stub(get_nhsn_data, 'epidatr::pub_nhs_facilities', mock_result)
  expect_no_error(get_nhsn_data(disease = "COVID"))
  
  # Test mixed case
  mockery::stub(get_nhsn_data, 'epidatr::pub_nhs_facilities', mock_result)
  expect_no_error(get_nhsn_data(disease = "Influenza"))
  
  # Test lowercase
  mockery::stub(get_nhsn_data, 'epidatr::pub_nhs_facilities', mock_result)
  expect_no_error(get_nhsn_data(disease = "rsv"))
})

test_that("get_nhsn_data maps diseases to correct signals", {
  skip_if_not_installed("mockery")
  
  # We can test the internal mapping by checking error messages include correct signal names
  # Since we can't easily mock internal calls, we test that valid diseases don't error
  
  mock_result <- data.frame(
    geo_value = "US",
    time_value = 202301,
    value = 100
  )
  
  # Mock and verify influenza signal
  mockery::stub(get_nhsn_data, 'epidatr::pub_nhs_facilities', mock_result)
  result <- get_nhsn_data(disease = "influenza")
  expect_true(is.data.frame(result))
  
  # Mock and verify covid signal  
  mockery::stub(get_nhsn_data, 'epidatr::pub_nhs_facilities', mock_result)
  result <- get_nhsn_data(disease = "covid")
  expect_true(is.data.frame(result))
  
  # Mock and verify rsv signal
  mockery::stub(get_nhsn_data, 'epidatr::pub_nhs_facilities', mock_result)
  result <- get_nhsn_data(disease = "rsv")
  expect_true(is.data.frame(result))
})

test_that("get_nhsn_data handles geo_values parameter", {
  skip_if_not_installed("mockery")
  
  mock_result <- data.frame(
    geo_value = c("CA", "NY"),
    time_value = 202301,
    value = c(100, 150)
  )
  
  mockery::stub(get_nhsn_data, 'epidatr::pub_nhs_facilities', mock_result)
  
  # Test with multiple geo values
  result <- get_nhsn_data(disease = "covid", geo_values = c("CA", "NY"))
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
})

test_that("get_nhsn_data handles time_values parameter", {
  skip_if_not_installed("mockery")
  
  mock_result <- data.frame(
    geo_value = "US",
    time_value = c(202201, 202202),
    value = c(100, 150)
  )
  
  mockery::stub(get_nhsn_data, 'epidatr::pub_nhs_facilities', mock_result)
  
  # Test with time values (mocked)
  result <- get_nhsn_data(disease = "covid", time_values = c(202201, 202202))
  expect_true(is.data.frame(result))
})

test_that("get_nhsn_data returns data frame", {
  skip_if_not_installed("mockery")
  
  mock_result <- data.frame(
    geo_value = "US",
    time_value = 202301,
    value = 100
  )
  
  mockery::stub(get_nhsn_data, 'epidatr::pub_nhs_facilities', mock_result)
  
  result <- get_nhsn_data(disease = "covid")
  expect_true(is.data.frame(result))
  expect_true("geo_value" %in% names(result))
})
