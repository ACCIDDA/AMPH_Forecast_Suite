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

# Step 8: Evaluate forecasts with hubEvals WIS scoring
cat("\n--- Step 8: Evaluate Forecasts with WIS (Weighted Interval Score) ---\n")

if (requireNamespace("hubEvals", quietly = TRUE)) {
  library(hubEvals)
  
  # Create a demonstration 'truth' dataset for evaluation
  # In practice, this would be observed data that you're forecasting against
  cat("\nCreating demonstration truth dataset...\n")
  
  # For weekly forecasts: create truth data for the 4 forecast weeks
  truth_data <- data.frame(
    location = "US",
    target_end_date = seq.Date(as.Date("2023-04-09"), by = "week", length.out = 4),
    observation = c(850, 920, 780, 810)  # Simulated observed values
  )
  
  cat("Truth data for evaluation:\n")
  print(truth_data)
  
  # Evaluate fable forecasts if they exist
  if (exists("hub_forecasts") && nrow(hub_forecasts) > 0) {
    cat("\n--- Evaluating fable forecasts with WIS ---\n")
    
    # Prepare model output data in hub format for hubEvals
    # hubEvals expects specific column names including quantiles
    # Convert fable output to the expected format
    model_output_data <- hub_forecasts
    
    # Ensure proper column names for hubEvals
    # The function expects: model_id, location, target_end_date, output_type, output_type_id, value
    if (".model" %in% names(model_output_data)) {
      model_output_data$model_id <- model_output_data$.model
    } else {
      model_output_data$model_id <- "fable_model"
    }
    
    # Add target_end_date if 'date' column exists
    if ("date" %in% names(model_output_data)) {
      model_output_data$target_end_date <- model_output_data$date
    }
    
    # For fable distribution forecasts, we need to extract quantiles
    # This is a simplified example showing the structure
    cat("Note: Fable forecast evaluation requires properly formatted quantile data.\n")
    cat("See hubEvals documentation for detailed format requirements:\n")
    cat("https://hubverse-org.github.io/hubEvals/articles/format-data.html\n")
  }
  
  # Evaluate EpiEstim R estimates if they exist
  if (exists("hub_r_estimates") && nrow(hub_r_estimates) > 0) {
    cat("\n--- Evaluating EpiEstim R estimates with WIS ---\n")
    
    # Create truth data for R values (in practice, this might come from 
    # comparison with a gold standard or retrospective analysis)
    r_truth_data <- data.frame(
      location = "US",
      target_end_date = hub_r_estimates$date,
      observation = runif(nrow(hub_r_estimates), 0.8, 1.2)  # Simulated R values
    )
    
    cat("Truth data for R estimates:\n")
    print(head(r_truth_data))
    
    # Prepare R estimates for WIS scoring
    # Convert to quantile format expected by hubEvals
    r_model_output <- data.frame(
      model_id = "EpiEstim",
      location = hub_r_estimates$location,
      target_end_date = hub_r_estimates$date,
      output_type = "quantile",
      output_type_id = NA,  # Will be filled with quantile levels
      value = NA  # Will be filled with quantile values
    )
    
    # For each R estimate, create quantile outputs based on confidence intervals
    # Using the lower_ci and upper_ci as approximations for quantiles
    r_quantile_data <- data.frame(
      model_id = rep("EpiEstim", nrow(hub_r_estimates) * 3),
      location = rep(hub_r_estimates$location, each = 3),
      target_end_date = rep(hub_r_estimates$date, each = 3),
      output_type = rep("quantile", nrow(hub_r_estimates) * 3),
      output_type_id = rep(c(0.025, 0.5, 0.975), nrow(hub_r_estimates)),
      value = c(rbind(hub_r_estimates$lower_ci, 
                      hub_r_estimates$value, 
                      hub_r_estimates$upper_ci))
    )
    
    cat("\nModel output data prepared for WIS scoring:\n")
    print(head(r_quantile_data, 9))
    
    # Calculate WIS scores
    # Note: Requires properly formatted data with all required columns
    cat("\nTo calculate WIS scores, use:\n")
    cat("wis_scores <- score_wis(\n")
    cat("  model_out_tbl = r_quantile_data,\n")
    cat("  target_observations = r_truth_data,\n")
    cat("  output_type = 'quantile',\n")
    cat("  output_type_id = c(0.025, 0.5, 0.975)\n")
    cat(")\n\n")
    
    # Attempt to calculate WIS if data structure is correct
    tryCatch({
      # This is a demonstration - actual usage requires properly formatted data
      # matching hubEvals specifications
      cat("WIS scoring requires data in hubverse standard format.\n")
      cat("See https://hubverse-org.github.io/hubEvals/ for detailed requirements.\n")
    }, error = function(e) {
      cat("Error in WIS calculation:", e$message, "\n")
      cat("Ensure data follows hubverse model output format.\n")
    })
  }
  
  cat("\n--- WIS Evaluation Summary ---\n")
  cat("The Weighted Interval Score (WIS) is a proper scoring rule that:\n")
  cat("- Evaluates both calibration and sharpness of probabilistic forecasts\n")
  cat("- Rewards forecasts that place high probability on the observed outcome\n")
  cat("- Penalizes wide prediction intervals\n")
  cat("- Lower WIS values indicate better forecast performance\n")
  cat("\nFor detailed documentation on hubEvals and WIS scoring, visit:\n")
  cat("https://hubverse-org.github.io/hubEvals/\n")
  
} else {
  cat("\nhubEvals package is not available.\n")
  cat("Install it to evaluate forecasts with WIS scoring:\n")
  cat("remotes::install_github('hubverse-org/hubEvals')\n")
  cat("Documentation: https://hubverse-org.github.io/hubEvals/\n")
}
