# AMPH Forecast Suite

Suite of tools and helper functions to build and run an infectious disease forecasting pipeline and hub.

## Overview

The AMPH Forecast Suite is an R package that provides a comprehensive toolkit for building infectious disease forecasting pipelines and hubs. It streamlines the process of working with multiple forecasting packages by:

- Installing and managing dependencies for key forecasting packages
- Providing helper functions to translate data between different package formats
- Facilitating integration with hubverse, fable, EpiEstim, EpiNow2, epidatr, and epiforecast packages

## Installation

You can install the development version from GitHub:

```r
# Install remotes if you haven't already
install.packages("remotes")

# Install AMPH Forecast Suite
remotes::install_github("ACCIDDA/AMPH_Forecast_Suite")
```

## Quick Start

### 1. Install Required Forecasting Packages

```r
library(AMPHForecastSuite)

# Install all forecasting packages
install_forecast_packages()

# Or install specific categories
install_forecast_packages(
  install_hubverse = TRUE,
  install_forecasting = TRUE,
  install_epi = TRUE,
  install_data = TRUE
)
```

### 2. Convert Data Between Formats

The package provides helper functions to convert data between hub format and various forecasting package formats:

#### Hub ↔ Fable/tsibble

```r
# Convert hub data to tsibble for fable modeling
hub_data <- data.frame(
  date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 20),
  location = "US",
  value = rnorm(20, 100, 10)
)

# Convert to tsibble format
ts_data <- convert_hub_to_tsibble(hub_data)

# Use with fable
library(fable)
library(tsibble)
model <- ts_data %>% 
  model(arima = ARIMA(value))

# Convert fable forecasts back to hub format
forecasts <- model %>% forecast(h = 4)
hub_forecasts <- convert_fable_to_hub(forecasts, location = "US")
```

### 3. Validate Data Before Modeling

Before converting data for fable models, you can validate that it meets the required format:

```r
# Validate data for fable modeling
hub_data <- data.frame(
  date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 20),
  location = "US",
  value = rnorm(20, 100, 10)
)

# Comprehensive validation - stops on error by default
validate_fable_input_data(hub_data)

# Get detailed validation results without stopping
validation_results <- validate_fable_input_data(
  hub_data,
  stop_on_error = FALSE,
  verbose = TRUE
)

# Check specific aspects
check_date_column(hub_data)
check_numeric_columns(hub_data)
check_temporal_regularity(hub_data)
check_missing_values(hub_data, c("date", "location", "value"))
validate_tsibble_requirements(hub_data)
```

#### Hub ↔ EpiEstim

```r
# Prepare data for EpiEstim
epiestim_data <- convert_hub_to_epiestim(
  hub_data, 
  location_filter = "US"
)

# Run EpiEstim (example)
library(EpiEstim)
res <- estimate_R(epiestim_data, method = "parametric_si",
                  config = make_config(list(mean_si = 7, std_si = 4.5)))

# Convert results back to hub format
hub_r_estimates <- convert_epiestim_to_hub(res, location = "US")
```

#### Hub ↔ EpiNow2

```r
# Prepare data for EpiNow2
epinow2_data <- convert_hub_to_epinow2(
  hub_data,
  location_filter = "US"
)

# Run EpiNow2 (example)
library(EpiNow2)
res <- epinow(
  reported_cases = epinow2_data,
  generation_time = generation_time_opts(Generation_Time_Opts(...)),
  delays = delay_opts(Delay_Opts(...))
)

# Convert results back to hub format
hub_forecasts <- convert_epinow2_to_hub(res, location = "US")
```

## Available Functions

### Package Installation
- `install_forecast_packages()`: Install all required forecasting packages

### Data Conversion Functions

#### Fable/tsibble Converters
- `convert_hub_to_tsibble()`: Convert hub format to tsibble
- `convert_hub_to_fable()`: Convert hub format for fable modeling
- `convert_fable_to_hub()`: Convert fable forecasts to hub format
- `convert_tsibble_to_hub()`: Convert tsibble back to hub format

#### EpiEstim Converters
- `convert_hub_to_epiestim()`: Convert hub format to EpiEstim format
- `convert_epiestim_to_hub()`: Convert EpiEstim results to hub format

#### EpiNow2 Converters
- `convert_hub_to_epinow2()`: Convert hub format to EpiNow2 format
- `convert_epinow2_to_hub()`: Convert EpiNow2 results to hub format

### Data Validation Functions

#### Fable Data Validators
- `validate_fable_input_data()`: Comprehensive validation of data for fable models
- `validate_tsibble_requirements()`: Check tsibble-specific requirements
- `check_date_column()`: Validate date/time column format
- `check_numeric_columns()`: Validate numeric value columns
- `check_missing_values()`: Check for missing values in critical columns
- `check_temporal_regularity()`: Check time series regularity

## Forecast Evaluation with hubEvals

The package supports integration with [hubEvals](https://hubverse-org.github.io/hubEvals/) for evaluating forecast performance using the Weighted Interval Score (WIS) and other proper scoring rules.

### WIS Evaluation Example

The Weighted Interval Score (WIS) is a proper scoring rule that evaluates both the calibration and sharpness of probabilistic forecasts. It rewards forecasts that place high probability on observed outcomes while penalizing overly wide prediction intervals.

```r
library(AMPHForecastSuite)
library(hubEvals)

# After generating forecasts with fable, EpiEstim, or EpiNow2
# and converting them to hub format...

# 1. Create or load truth/observation data
truth_data <- data.frame(
  location = "US",
  target_end_date = seq.Date(as.Date("2023-04-09"), by = "week", length.out = 4),
  observation = c(850, 920, 780, 810)
)

# 2. Prepare model output in hub quantile format
# Your hub_forecasts should have columns: model_id, location, 
# target_end_date, output_type, output_type_id, value
model_output <- hub_forecasts  # From convert_fable_to_hub() or similar

# 3. Calculate WIS scores
wis_scores <- score_wis(
  model_out_tbl = model_output,
  target_observations = truth_data,
  output_type = "quantile",
  output_type_id = c(0.025, 0.25, 0.5, 0.75, 0.975)
)

# View scores
print(wis_scores)
```

### Key Features of WIS Evaluation

- **Proper Scoring Rule**: WIS is a strictly proper scoring rule, meaning forecasters are incentivized to report their true probabilistic beliefs
- **Calibration & Sharpness**: Evaluates both how well prediction intervals contain observations and how precise those intervals are
- **Lower is Better**: Lower WIS values indicate better forecast performance
- **Quantile-Based**: Works with quantile forecasts commonly produced by epidemiological models

### Complete Pipeline Example

A full working example demonstrating WIS evaluation of forecasts from fable, EpiEstim, and EpiNow2 is available in:

```r
# View the complete example
system.file("examples", "forecasting_pipeline_example.R", 
            package = "AMPHForecastSuite")
```

### Additional Resources

- **hubEvals Documentation**: [https://hubverse-org.github.io/hubEvals/](https://hubverse-org.github.io/hubEvals/)
- **WIS Scoring Function**: [score_wis() reference](https://hubverse-org.github.io/hubEvals/reference/score_wis.html)
- **Data Format Requirements**: [Formatting data for hubEvals](https://hubverse-org.github.io/hubEvals/articles/format-data.html)

## Package Dependencies

The package helps you install and work with:

### Hubverse Packages
- hubData: Data management for forecast hubs
- hubUtils: Utility functions for hubs
- hubValidations: Validation tools for hub submissions
- hubVis: Visualization tools for hub data
- hubAdmin: Administrative tools for hubs

### Forecasting Packages
- fable: Forecasting models for tidy time series
- fabletools: Tools for working with fable models
- feasts: Feature extraction and statistics for time series
- tsibble: Tidy temporal data frames

### Epidemiological Packages
- EpiEstim: Estimate time-varying reproduction numbers
- EpiNow2: Estimate Rt and forecast with delays
- epipredict: Epidemiological forecasting tools

### Data Access
- epidatr: Access epidemiological data from various sources

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the GPL-3 License - see the [LICENSE](LICENSE) file for details.

## Citation

If you use this package in your research, please cite it as:

```
ACCIDDA (2024). AMPH Forecast Suite: Tools for Infectious Disease Forecasting.
R package version 0.1.0. https://github.com/ACCIDDA/AMPH_Forecast_Suite
```

## Support

For questions and issues, please open an issue on [GitHub](https://github.com/ACCIDDA/AMPH_Forecast_Suite/issues).
