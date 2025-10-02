# Example: Building a Forecasting Pipeline with AMPH Forecast Suite
# This script demonstrates how to use the package to build a complete
# infectious disease forecasting pipeline

library(AMPHForecastSuite)

# Step 1: Install required packages (run once)
# install_forecast_packages()

# Step 2: Load sample data (replace with your actual data)
# For this example, we'll create synthetic data
set.seed(123)
hub_data <- data.frame(
  date = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 100),
  location = "US",
  value = cumsum(rnorm(100, 10, 5)) + 100
)

# Make sure values are positive
hub_data$value <- pmax(hub_data$value, 1)

cat("Sample hub data:\n")
print(head(hub_data))

# Step 3: Convert to weekly data for fable forecasting
# Aggregate daily to weekly
hub_data_weekly <- data.frame(
  date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 14),
  location = "US",
  value = aggregate(hub_data$value[1:98], 
                   by = list(rep(1:14, each = 7)), 
                   FUN = sum)$x
)

cat("\nWeekly hub data:\n")
print(hub_data_weekly)

# Step 4: Convert to tsibble for fable
if (requireNamespace("tsibble", quietly = TRUE)) {
  ts_data <- convert_hub_to_tsibble(hub_data_weekly)
  cat("\nConverted to tsibble:\n")
  print(head(ts_data))
  
  # Step 5: Fit forecasting model with fable (if available)
  if (requireNamespace("fable", quietly = TRUE)) {
    library(fable)
    
    model <- ts_data %>% 
      model(
        arima = ARIMA(value),
        mean = MEAN(value)
      )
    
    cat("\nFitted models:\n")
    print(model)
    
    # Generate forecasts
    forecasts <- model %>% forecast(h = 4)
    cat("\nForecasts:\n")
    print(forecasts)
    
    # Convert back to hub format
    hub_forecasts <- convert_fable_to_hub(forecasts, location = "US", target = "inc")
    cat("\nHub format forecasts:\n")
    print(hub_forecasts)
  }
}

# Step 6: Estimate R with EpiEstim (if available)
if (requireNamespace("EpiEstim", quietly = TRUE)) {
  library(EpiEstim)
  
  # Convert to EpiEstim format (using daily data)
  epiestim_data <- convert_hub_to_epiestim(hub_data, location_filter = "US")
  cat("\nEpiEstim format data:\n")
  print(head(epiestim_data))
  
  # Estimate R (simplified parameters for demonstration)
  tryCatch({
    res <- estimate_R(
      epiestim_data, 
      method = "parametric_si",
      config = make_config(list(mean_si = 7, std_si = 4.5))
    )
    
    cat("\nR estimates:\n")
    print(head(res$R))
    
    # Convert to hub format
    hub_r_estimates <- convert_epiestim_to_hub(res, location = "US")
    cat("\nHub format R estimates:\n")
    print(head(hub_r_estimates))
  }, error = function(e) {
    cat("\nNote: EpiEstim estimation requires at least 2 time points and may fail with certain data patterns.\n")
    cat("Error:", e$message, "\n")
  })
}

# Step 7: Use EpiNow2 (if available)
if (requireNamespace("EpiNow2", quietly = TRUE)) {
  cat("\nEpiNow2 is available. Convert data:\n")
  epinow2_data <- convert_hub_to_epinow2(hub_data, location_filter = "US")
  print(head(epinow2_data))
  
  cat("\nNote: Running full EpiNow2 estimation requires specification of generation time\n")
  cat("and delay distributions. See EpiNow2 documentation for details.\n")
}

cat("\n=== Example complete ===\n")
cat("This example demonstrated:\n")
cat("1. Loading hub-format data\n")
cat("2. Converting to fable/tsibble format\n")
cat("3. Fitting forecasting models with fable\n")
cat("4. Converting to EpiEstim format\n")
cat("5. Estimating R with EpiEstim\n")
cat("6. Converting all results back to hub format\n")
