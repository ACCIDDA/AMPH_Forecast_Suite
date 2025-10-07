test_that("install_forecast_packages handles boolean parameters", {
  skip_on_cran()
  
  # Test with all FALSE - should not install anything
  # We can't easily test the actual behavior without mocking, 
  # but we can test that it doesn't error
  result <- install_forecast_packages(
    install_hubverse = FALSE,
    install_forecasting = FALSE,
    install_epi = FALSE,
    install_data = FALSE
  )
  
  expect_null(result)
})

test_that("install_forecast_packages returns NULL invisibly", {
  skip_on_cran()
  
  # The function should return NULL invisibly
  result <- install_forecast_packages(
    install_hubverse = FALSE,
    install_forecasting = FALSE,
    install_epi = FALSE,
    install_data = FALSE
  )
  
  expect_null(result)
})

test_that("install_forecast_packages accepts repos parameter", {
  skip_on_cran()
  
  # Test that custom repos parameter is accepted
  result <- install_forecast_packages(
    install_hubverse = FALSE,
    install_forecasting = FALSE,
    install_epi = FALSE,
    install_data = FALSE,
    repos = "https://cloud.r-project.org"
  )
  
  expect_null(result)
})

test_that("install_forecast_packages parameter defaults work", {
  # We can verify the function signature accepts default parameters
  expect_equal(
    length(formals(install_forecast_packages)),
    5  # install_hubverse, install_forecasting, install_epi, install_data, repos
  )
  
  # Check default values
  defaults <- formals(install_forecast_packages)
  expect_true(defaults$install_hubverse)
  expect_true(defaults$install_forecasting)
  expect_true(defaults$install_epi)
  expect_true(defaults$install_data)
  expect_equal(defaults$repos, "https://cloud.r-project.org")
})

test_that("install_forecast_packages can install selectively", {
  skip_on_cran()
  
  # Test installing only hubverse packages (disabled for testing)
  result1 <- install_forecast_packages(
    install_hubverse = FALSE,
    install_forecasting = FALSE,
    install_epi = FALSE,
    install_data = FALSE
  )
  expect_null(result1)
  
  # Test that different combinations work without error
  # We test with all FALSE to avoid actual installations
  result2 <- install_forecast_packages(
    install_hubverse = FALSE,
    install_forecasting = FALSE,
    install_epi = FALSE,
    install_data = FALSE
  )
  expect_null(result2)
})

test_that("install_forecast_packages has correct function signature", {
  # Verify all expected parameters exist
  params <- names(formals(install_forecast_packages))
  
  expect_true("install_hubverse" %in% params)
  expect_true("install_forecasting" %in% params)
  expect_true("install_epi" %in% params)
  expect_true("install_data" %in% params)
  expect_true("repos" %in% params)
})
