# AMPH_Forecast_Suite
Suite of tools and helper functions to build and run a basic forecasting hub.

## Features

### Data Retrieval Functions

- **NHSN Weekly Hospitalization Data**: Retrieve weekly hospitalization counts for influenza, COVID-19, and RSV from the NHSN via the epidatr package.

## Installation

This suite requires R and the following packages:
- `epidatr` - for accessing epidemiological data

```r
install.packages("epidatr")
```

## Usage

### Getting NHSN Hospitalization Data

```r
# Source the function
source("R/get_nhsn_data.R")

# Get COVID-19 hospitalization data
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

For more detailed documentation, see [R/README.md](R/README.md).
