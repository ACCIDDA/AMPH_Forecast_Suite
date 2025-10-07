# Fable Data Validation Functions - Implementation Summary

## Overview
Created comprehensive R functions to validate data format for fable models in the AMPH Forecast Suite package.

## Files Created

### 1. R/fable_validators.R (603 lines)
Main validation functions file containing:

#### Primary Function
- `validate_fable_input_data()` - Comprehensive validation wrapper that runs all checks

#### Individual Validation Functions
- `check_date_column()` - Validates date/time column format
- `check_numeric_columns()` - Validates numeric value columns
- `check_missing_values()` - Detects problematic missing values
- `validate_tsibble_requirements()` - Checks tsibble-specific requirements
- `check_temporal_regularity()` - Verifies time series regularity

### 2. tests/testthat/test-fable-validators.R (349 lines)
Comprehensive test suite with 23 test cases covering:
- Valid data scenarios
- Invalid data detection
- Error handling
- Edge cases
- Multiple keys/locations
- Custom column names
- Multiple value columns

### 3. inst/examples/fable_validation_example.R
Practical examples demonstrating:
- Basic validation
- Handling data with missing values
- Detecting irregular intervals
- Using individual validation functions
- Multiple locations
- Custom column names

### 4. vignettes/validating-fable-data.Rmd
Complete documentation vignette including:
- Introduction and setup
- Main validation function usage
- Individual function examples
- Common validation scenarios
- Best practices
- Integration with fable workflow

### 5. Updated Files
- **NAMESPACE**: Added exports for all 6 new validation functions
- **README.md**: Added validation functions section with usage examples

## Key Features

### Validation Checks Performed
1. **Data Structure**
   - Verifies input is a data frame
   - Checks for non-empty data
   - Validates required columns exist

2. **Date Column Validation**
   - Checks date type (Date, POSIXct, or convertible)
   - Detects NA values
   - Warns about duplicates

3. **Numeric Column Validation**
   - Verifies numeric type
   - Detects infinite values
   - Warns about negative values
   - Warns about zero variance

4. **Missing Values**
   - Detects NA values in critical columns
   - Optional allowance for value column NAs
   - Reports percentage of missing data

5. **Tsibble Requirements**
   - Checks for duplicate key-index combinations
   - Validates unique time-key pairs
   - Warns if index is unsorted

6. **Temporal Regularity**
   - Detects interval type (daily, weekly, monthly, yearly)
   - Verifies regular spacing
   - Handles minor variations (month length differences)
   - Checks each key separately

### Function Parameters
All functions support:
- **stop_on_error**: Control whether to stop or return results
- **verbose**: Toggle detailed messages
- **Custom column names**: Specify date, value, and key columns
- **Multiple value columns**: Validate multiple numeric columns at once

### Return Values
Functions return:
- **valid**: Boolean indicating if validation passed
- **errors**: Character vector of error messages
- **warnings**: Character vector of warning messages
- **checks_passed**: List of successful checks
- **detected_intervals**: Information about time intervals (temporal regularity)

## Usage Examples

### Quick Validation
```r
library(AMPHForecastSuite)

hub_data <- data.frame(
  date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 20),
  location = "US",
  value = rnorm(20, 100, 10)
)

# Validate and stop on error
validate_fable_input_data(hub_data)
```

### Detailed Validation
```r
# Get detailed results without stopping
result <- validate_fable_input_data(
  hub_data,
  stop_on_error = FALSE,
  verbose = TRUE
)

if (!result$valid) {
  cat("Errors found:\n")
  print(result$errors)
}
```

### Individual Checks
```r
# Check specific aspects
check_date_column(hub_data)
check_numeric_columns(hub_data)
check_temporal_regularity(hub_data)
```

### Custom Column Names
```r
custom_data <- data.frame(
  time = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 30),
  region = "Europe",
  cases = rpois(30, 100)
)

validate_fable_input_data(
  custom_data,
  date_col = "time",
  value_cols = "cases",
  key_cols = "region"
)
```

## Testing Coverage

All functions have comprehensive test coverage including:
- Valid data scenarios
- Error detection (invalid types, missing columns, etc.)
- Warning conditions (negative values, unsorted data, etc.)
- Edge cases (empty data, single observations, etc.)
- Multiple keys/locations
- Custom configurations

## Integration with Package

The validation functions integrate seamlessly with existing package functions:

```r
# 1. Validate
validate_fable_input_data(hub_data)

# 2. Convert (existing function)
ts_data <- convert_hub_to_tsibble(hub_data)

# 3. Model with fable
library(fable)
model <- ts_data %>% model(ARIMA(value))

# 4. Forecast
forecasts <- model %>% forecast(h = 4)

# 5. Convert back (existing function)
hub_forecasts <- convert_fable_to_hub(forecasts, location = "US")
```

## Benefits

1. **Early Error Detection**: Catch data issues before modeling
2. **Clear Error Messages**: Understand exactly what needs fixing
3. **Flexible Validation**: Use comprehensive or individual checks
4. **Customizable Behavior**: Stop on error or collect all issues
5. **Well Documented**: Examples, vignettes, and inline documentation
6. **Fully Tested**: Comprehensive test suite ensures reliability
7. **Integration Ready**: Works seamlessly with existing package functions

## Future Enhancements (Optional)

Potential future improvements could include:
- Auto-correction suggestions for common issues
- Integration with tidyverse pipelines
- Additional interval types (business days, etc.)
- Custom validation rules
- Validation reporting/visualization

## Conclusion

This implementation provides a robust, well-tested, and well-documented solution for validating data before use with fable models. The functions are flexible, easy to use, and integrate seamlessly with the existing AMPH Forecast Suite package.
