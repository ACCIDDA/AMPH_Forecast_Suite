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
