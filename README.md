# AMPH Forecast Suite

Suite of tools and helper functions to build and run an infectious disease forecasting pipeline and hub.

## Overview

The AMPH Forecast Suite is an R package that provides a comprehensive toolkit for building infectious disease forecasting pipelines and hubs. It streamlines the process of working with multiple forecasting packages by:

- Installing and managing dependencies for key forecasting packages
- Setting up appropriate directory structure
- Pulling and manipulating data to use with forecasting models
- Providing helper functions to translate data between different package formats
- Running forecasts and nowcast using multiple packages
- Ensembling, visualizing, and evaluating forecasts
- Producing outputs compatible with forecast hubs and evaluation tools

For more information, see the package documentation and function help pages.


## Installation

You can install the development version from GitHub:

```r
# Install remotes if you haven't already
install.packages("remotes")

# Install AMPH Forecast Suite
remotes::install_github("ACCIDDA/AMPH_Forecast_Suite")
```

### Install Packages and Build Directory Structure

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
  install_data = TRUE,
  install_nowcast = TRUE
)
```


## Packages for Infectious Disease Forecasting

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
- epiforecast: Tools for infectious disease forecasting
- tsibble: Tidy temporal data frames

### Epidemiological Packages
- EpiEstim: Estimate time-varying reproduction numbers
- EpiNow2: Estimate Rt and forecast with delays
- epipredict: Epidemiological forecasting tools

### Nowcasting packages
- epinowcast: Flexible Hierarchical Nowcasting
- baselinenowcast: Baseline nowcasting methods for handling delays in epidemiological data

### Data Access
- epidatr: Access epidemiological data from various sources



## Additional Resources

- **hubEvals Documentation**: [https://hubverse-org.github.io/hubEvals/](https://hubverse-org.github.io/hubEvals/)
- **WIS Scoring Function**: [score_wis() reference](https://hubverse-org.github.io/hubEvals/reference/score_wis.html)
- **Data Format Requirements**: [Formatting data for hubEvals](https://hubverse-org.github.io/hubEvals/articles/format-data.html)



## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the GPL-3 License - see the [LICENSE](LICENSE) file for details.

## Citation

If you use this package in your research, please cite it as:

```
ACCIDDA (2025). AMPH Forecast Suite: Tools for Infectious Disease Forecasting.
R package version 0.1.0. https://github.com/ACCIDDA/AMPH_Forecast_Suite
```

## Support

For questions and issues, please open an issue on [GitHub](https://github.com/ACCIDDA/AMPH_Forecast_Suite/issues).
