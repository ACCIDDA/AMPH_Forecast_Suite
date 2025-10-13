#' Convert Hub Format to Fable/tsibble Format
#'
#' Converts data from hub format to tsibble format for use with fable package.
#'
#' @param hub_data Data frame in hub format with columns for date, location, value, etc.
#' @param date_col Character. Name of the date column. Default is "date".
#' @param value_col Character. Name of the value column. Default is "value".
#' @param location_col Character. Name of the location column. Default is "location".
#' @param index Character. Name to use for the time index. Default is "date".
#' @param key Character vector. Key variables for tsibble. Default is "location".
#'
#' @return A tsibble object
#' @export
#'
#' @importFrom dplyr %>% select mutate rename
#' @importFrom lubridate as_date
#'
#' @examples
#' \dontrun{
#' hub_data <- data.frame(
#'   date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 10),
#'   location = "US",
#'   value = rnorm(10, 100, 10)
#' )
#' ts_data <- convert_hub_to_tsibble(hub_data)
#' }
convert_hub_to_tsibble <- function(hub_data,
                                  date_col = "date",
                                  value_col = "value",
                                  location_col = "location",
                                  index = "date",
                                  key = "location") {

  if (!requireNamespace("tsibble", quietly = TRUE)) {
    stop("Package 'tsibble' is required. Please install it first.")
  }

  # Ensure date column is Date type
  hub_data[[date_col]] <- lubridate::as_date(hub_data[[date_col]])

  # Convert to tsibble
  result <- tsibble::as_tsibble(hub_data,
                                index = date_col,
                                key = location_col)

  return(result)
}


#' Convert Hub Format to Fable Format
#'
#' Converts data from hub format to a format suitable for fable modeling.
#' This is essentially a wrapper around convert_hub_to_tsibble with additional
#' preparation for fable models.
#'
#' @param hub_data Data frame in hub format
#' @param ... Additional arguments passed to convert_hub_to_tsibble
#'
#' @return A tsibble object ready for fable modeling
#' @export
#'
#' @examples
#' \dontrun{
#' hub_data <- data.frame(
#'   date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 10),
#'   location = "US",
#'   value = rnorm(10, 100, 10)
#' )
#' fable_data <- convert_hub_to_fable(hub_data)
#' }
convert_hub_to_fable <- function(hub_data, ...) {
  convert_hub_to_tsibble(hub_data, ...)
}


#' Convert Fable Forecasts to Hub Format
#'
#' Converts fable forecast output to hub submission format.
#'
#' @param fable_forecast A fable forecast object
#' @param location Character. Location identifier. Default is NULL (extract from data).
#' @param target Character. Target name for the forecast. Default is "inc".
#'
#' @return A data frame in hub format
#' @export
#' @importFrom dplyr %>% mutate select rename
#' @importFrom tibble tibble
#'
#' @examples
#' \dontrun{
#' # Assuming you have a fable forecast object
#' # hub_forecast <- convert_fable_to_hub(fc, location = "US", target = "inc")
#' }
convert_fable_to_hub <- function(fable_forecast,
                                location = NULL,
                                target = "inc") {

  if (!requireNamespace("fable", quietly = TRUE)) {
    stop("Package 'fable' is required. Please install it first.")
  }

  # Extract forecast data
  # This is a simplified version - actual implementation depends on fable output structure
  result <- as.data.frame(fable_forecast)

  # Add hub-specific columns if they don't exist
  if (!is.null(location) && !"location" %in% names(result)) {
    result$location <- location
  }

  if (!"target" %in% names(result)) {
    result$target <- target
  }

  return(result)
}


#' Convert tsibble to Hub Format
#'
#' Converts a tsibble object back to standard hub format.
#'
#' @param tsibble_data A tsibble object
#'
#' @return A data frame in hub format
#' @export
#' @importFrom dplyr %>%
#'
#' @examples
#' \dontrun{
#' # Convert back from tsibble
#' # hub_data <- convert_tsibble_to_hub(ts_data)
#' }
convert_tsibble_to_hub <- function(tsibble_data) {

  # Convert to regular data frame
  result <- as.data.frame(tsibble_data)

  return(result)
}
