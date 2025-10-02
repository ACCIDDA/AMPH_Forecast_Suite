# AMPH_Forecast_Suite
Suite of tools and helper functions to build and run a basic forecasting hub.

## Features

### Data Retrieval Functions

- **NHSN Weekly Hospitalization Data**: Retrieve weekly hospitalization counts for influenza, COVID-19, and RSV from the NHSN via the epidatr package.

## Installation

This suite requires R and the following packages:
- `epidatr` - for accessing epidemiological data

```r
# Install from CRAN
install.packages("epidatr")

# Or install the development version from GitHub
# install.packages("remotes")
# remotes::install_github("cmu-delphi/epidatr")
```

## NHSN Data Retrieval Function

### get_nhsn_data()

The `get_nhsn_data()` function retrieves weekly hospitalization count data from the National Healthcare Safety Network (NHSN) via the epidatr package. It supports data retrieval for three respiratory diseases:
- Influenza
- COVID-19
- RSV (Respiratory Syncytial Virus)

### Usage

```r
source("R/get_nhsn_data.R")

# Basic usage - get national COVID-19 data
covid_data <- get_nhsn_data(disease = "covid")

# Get influenza data for specific states
flu_data <- get_nhsn_data(
  disease = "influenza",
  geo_values = c("ca", "ny", "tx")
)

# Get RSV data for a specific time period
library(epidatr)
rsv_data <- get_nhsn_data(
  disease = "rsv",
  geo_values = "US",
  time_values = epirange(202201, 202252)
)
```

### Parameters

- **disease**: Character string specifying the disease. Must be one of "influenza", "covid", or "rsv" (case-insensitive).
- **geo_values**: Character vector of geographic locations. Default is "US" for national data. Can also use state abbreviations (e.g., "CA", "NY").
- **time_values**: Optional. Time period specification in epiweek format (YYYYWW). Use epidatr's `epirange()` helper function for date ranges. If NULL, retrieves all available data.
- **...**: Additional arguments passed to `epidatr::pub_nhs_facilities()`

### Return Value

Returns a data frame containing NHSN hospitalization data with columns including:
- `geo_value`: Geographic location identifier
- `time_value`: Date/epiweek of the observation
- Various count columns depending on the signal requested

### Examples

#### Example 1: National COVID-19 Data
```r
# Get all available COVID-19 hospitalization data for the US
covid_us <- get_nhsn_data(disease = "covid")
head(covid_us)
```

#### Example 2: Multi-State Influenza Data
```r
# Get influenza data for multiple states
states <- c("ca", "ny", "fl", "tx")
flu_states <- get_nhsn_data(
  disease = "influenza",
  geo_values = states
)
```

#### Example 3: RSV Data for Specific Time Period
```r
library(epidatr)

# Get RSV data for the 2022-2023 season
rsv_season <- get_nhsn_data(
  disease = "rsv",
  geo_values = "US",
  time_values = epirange(202240, 202320)
)
```

### Notes

- The function uses the `pub_nhs_facilities()` endpoint from epidatr
- Signal names are automatically mapped based on the disease parameter:
  - "influenza" → "confirmed_influenza_weekly"
  - "covid" → "confirmed_covid_weekly"
  - "rsv" → "confirmed_rsv_weekly"
- Geographic values should use lowercase state abbreviations or "US" for national data
- Time values use epiweek format (YYYYWW), where WW is the week number (01-53)

### Error Handling

The function validates the disease parameter and will raise an error if an invalid disease is specified:

```r
# This will raise an error
get_nhsn_data(disease = "measles")
# Error: Invalid disease: 'measles'. Must be one of: influenza, covid, rsv
```

### Related Resources

- [epidatr Package Documentation](https://cmu-delphi.github.io/epidatr/)
- [NHSN Data Information](https://www.cdc.gov/nhsn/)
- [Delphi Epidata API](https://cmu-delphi.github.io/delphi-epidata/)
=======
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
