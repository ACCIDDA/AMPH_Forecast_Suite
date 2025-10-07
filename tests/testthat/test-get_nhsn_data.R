test_that("get_nhsn_data validates disease parameter correctly", {
  # Test invalid disease
  expect_error(
    get_nhsn_data(disease = "invalid_disease"),
    "Invalid disease"
  )
  
  expect_error(
    get_nhsn_data(disease = "flu"),
    "Invalid disease"
  )
  
  expect_error(
    get_nhsn_data(disease = "coronavirus"),
    "Invalid disease"
  )
})

test_that("get_nhsn_data accepts valid disease names", {
  skip_on_cran()
  skip_if_not_installed("epidatr")
  
  # Test that valid diseases don't error on validation
  # We can't test the actual API call without credentials/network
  # but we can test that the validation passes
  
  # These will fail at the API call stage, but should pass validation
  # We test by checking the error message doesn't contain "Invalid disease"
  
  tryCatch({
    get_nhsn_data(disease = "covid", geo_values = "US")
  }, error = function(e) {
    expect_false(grepl("Invalid disease", e$message))
  })
})

test_that("get_nhsn_data handles case-insensitive disease names", {
  # Test that case variations are accepted (they should pass validation)
  # These will error at API level but not at validation level
  
  expect_error(
    get_nhsn_data(disease = "COVID_WRONG"),  
    "Invalid disease"  # Should fail validation
  )
  
  # Test that lowercase works (by checking it doesn't produce validation error)
  tryCatch({
    get_nhsn_data(disease = "covid")
  }, error = function(e) {
    # Should not be a validation error
    expect_false(grepl("Invalid disease", e$message))
  })
  
  tryCatch({
    get_nhsn_data(disease = "COVID")
  }, error = function(e) {
    expect_false(grepl("Invalid disease", e$message))
  })
  
  tryCatch({
    get_nhsn_data(disease = "Influenza")
  }, error = function(e) {
    expect_false(grepl("Invalid disease", e$message))
  })
})

test_that("get_nhsn_data disease validation logic works", {
  # Test the validation logic directly
  # Valid diseases should be: influenza, covid, rsv (case-insensitive)
  
  valid_diseases <- c("influenza", "covid", "rsv")
  
  # Test all valid diseases (lowercase)
  for (disease in valid_diseases) {
    expect_error(
      get_nhsn_data(disease = disease),
      regexp = NA,  # Should not match any error pattern for "Invalid disease"
      info = paste("Disease", disease, "should be valid")
    )
  }
  
  # Test uppercase versions
  expect_error(get_nhsn_data(disease = "INFLUENZA"), regexp = NA)
  expect_error(get_nhsn_data(disease = "COVID"), regexp = NA)
  expect_error(get_nhsn_data(disease = "RSV"), regexp = NA)
  
  # Test mixed case
  expect_error(get_nhsn_data(disease = "Influenza"), regexp = NA)
  expect_error(get_nhsn_data(disease = "Covid"), regexp = NA)
  expect_error(get_nhsn_data(disease = "Rsv"), regexp = NA)
})

test_that("get_nhsn_data accepts geo_values parameter", {
  skip_on_cran()
  skip_if_not_installed("epidatr")
  
  # Test that the function accepts various geo_values formats
  # We're testing parameter acceptance, not API functionality
  
  expect_error(
    get_nhsn_data(disease = "covid", geo_values = "US"),
    regexp = "Invalid disease",
    invert = TRUE
  )
  
  expect_error(
    get_nhsn_data(disease = "influenza", geo_values = c("CA", "NY")),
    regexp = "Invalid disease",
    invert = TRUE
  )
})

test_that("get_nhsn_data accepts time_values parameter", {
  skip_on_cran()
  skip_if_not_installed("epidatr")
  
  # Test that the function accepts time_values parameter
  expect_error(
    get_nhsn_data(disease = "rsv", time_values = c(202201, 202202)),
    regexp = "Invalid disease",
    invert = TRUE
  )
})

test_that("get_nhsn_data has correct parameter names", {
  # Verify the function signature
  params <- names(formals(get_nhsn_data))
  
  expect_true("disease" %in% params)
  expect_true("geo_values" %in% params)
  expect_true("time_values" %in% params)
})

test_that("get_nhsn_data default parameters work", {
  # Check default values
  defaults <- formals(get_nhsn_data)
  
  expect_equal(defaults$geo_values, "US")
  expect_null(defaults$time_values)
})
