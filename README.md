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
## Usage

### 1. Install Packages and Build Directory Structure

The package includes a function to install all required packages and set up a directory structure for your forecasting project.


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


## Summary

The AMPH Forecast Suite simplifies the process of:

1. Installing necessary packages
2. Setting up appropriate directory structure
3. Pulling and manipulating data to use with forecasting models
4. Ensembling, visualizing, and evaluating forecasts

For more information, see the package documentation and function help pages.





## Other key functionality

### 1. Forecasting workflow quick set up


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
