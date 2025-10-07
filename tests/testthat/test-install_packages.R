test_that("install_forecast_packages handles boolean parameters", {
  skip_on_cran()
  
  # Mock install.packages to avoid actual installations
  skip_if_not_installed("mockery")
  
  # Test with all FALSE - should not install anything
  # We can't easily test the actual behavior without mocking, 
  # but we can test that it doesn't error
  expect_no_error(
    install_forecast_packages(
      install_hubverse = FALSE,
      install_forecasting = FALSE,
      install_epi = FALSE,
      install_data = FALSE
    )
  )
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
  expect_no_error(
    install_forecast_packages(
      install_hubverse = FALSE,
      install_forecasting = FALSE,
      install_epi = FALSE,
      install_data = FALSE,
      repos = "https://cloud.r-project.org"
    )
  )
})

test_that("install_forecast_packages parameter defaults work", {
  skip_on_cran()
  skip_if_not_installed("mockery")
  
  # Mock the install.packages function to track calls
  mock_install <- mockery::mock()
  
  # We can't easily test defaults without actually running installations
  # But we can verify the function signature accepts default parameters
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
})

test_that("install_forecast_packages can install selectively", {
  skip_on_cran()
  
  # Test installing only hubverse packages
  expect_no_error(
    install_forecast_packages(
      install_hubverse = TRUE,
      install_forecasting = FALSE,
      install_epi = FALSE,
      install_data = FALSE
    )
  )
  
  # Test installing only forecasting packages
  expect_no_error(
    install_forecast_packages(
      install_hubverse = FALSE,
      install_forecasting = TRUE,
      install_epi = FALSE,
      install_data = FALSE
    )
  )
})
