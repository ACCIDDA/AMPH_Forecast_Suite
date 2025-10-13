#' Get NHSN Weekly Hospitalization Data
#'
#' Pulls weekly hospitalization count data from the NHSN (National Healthcare
#' Safety Network) via the epidatr package for influenza, COVID-19, or RSV.
#'
#' @param disease Character string specifying the disease. Must be one of:
#'   "influenza", "covid", or "rsv" (case-insensitive).
#' @param geo_values Character vector of geographic locations (e.g., state
#'   abbreviations like "CA", "NY"). Default is "MD" for Maryland data.
#' @param forecast_date Character vector of the date of forecast in "YYYY-MM-DD". Should be a Sunday.
#' @param save_data Logical. If TRUE, saves the retrieved data to a CSV file in the working directory.
#'
#' @return A data frame containing NHSN hospitalization data with columns
#'   typically including geo_value, time_value, and hospitalization counts.
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom lubridate as_date
#' @importFrom epidatr pub_covidcast
#'
#' @examples
#' \dontrun{
#' # Get COVID-19 data for California and New York for specific weeks
#' covid_data <- get_nhsn_data(
#'   disease = "covid",
#'   geo_values = c("ca", "ny"),
#'   forecast_date = "2025-10-12"
#' )
#'
#' # Get influenza data for California and New York for specific weeks
#' flu_data <- get_nhsn_data(
#'   disease = "influenza",
#'   geo_values = c("ca", "ny"),
#'   forecast_date = "2025-10-12"
#' )
#'
#' # Get RSV data for MD
#' rsv_data <- get_nhsn_data(
#'   disease = "rsv",
#'   geo_values = "MD",
#'   forecast_date = "2025-10-12"
#' )
#' }
#'
#' @export

#' @importFrom epidatr pub_nhs_facilities
get_nhsn_data <- function(disease = "influenza",
                          geo_values = "MD",
                          forecast_date = "2025-10-12",
                          save_data = TRUE
                          ) {

  # Validate disease parameter
  disease <- tolower(disease)
  valid_diseases <- c("influenza", "covid", "rsv")

  geo_values <- tolower(geo_values)

  #check geo_values are valid state abbreviations or "US"
  valid_states <- tolower(state.abb)
  valid_locations <- c(valid_states, "us")
  used_locations <- geo_values[geo_values %in% valid_locations]
  if (length(used_locations) == 0) {
    stop(sprintf(
      "Invalid geo_values: '%s'. Must be state abbreviations or 'US'.",
      paste(geo_values, collapse = ", ")
    ))
  }

  if (!disease %in% valid_diseases) {
    stop(sprintf(
      "Invalid disease: '%s'. Must be one of: %s",
      disease,
      paste(valid_diseases, collapse = ", ")
    ))
  }

  # Map disease names to NHSN signal names
  # Based on epidatr NHSN signals for respiratory diseases
  signal_map <- list(
    "influenza" = "confirmed_admissions_flu_ew",
    "covid" = "confirmed_admissions_covid_ew",
    "rsv" = "confirmed_admissions_rsv_ew"
  )

  signal <- signal_map[[disease]]

  # Call epidatr to get the data
  epidata <- epidatr::pub_covidcast(
    source = "nhsn",
    signals = "*",
    geo_type = "state",
    time_type = "week",
    geo_values = geo_values,
    issues = "*"
  ) %>%
    mutate(origin_date = as.Date(time_value) - 1,
           issue_date = as.Date(issue) - 1)

  epidata_filter <- epidata %>%
    filter(signal == signal,
           geo_value %in% used_locations,
           time_value >= lubridate::as_date("2020-09-01"),
           time_value < lubridate::as_date(forecast_date)) %>%
    mutate(disease = disease, signal = signal) %>%
    select(geo_value, source, disease, signal, issue_date, time_value, value)

  # Save data to CSV if requested
  if (save_data) {

    #check that that target-data directory exists, if not create it
    if (!dir.exists("target-data")) {
      dir.create("target-data")
    }

    readr::write_csv(
      epidata_filter,
      file = file.path("target-data", paste0("target-hospital-admissions-", forecast_date, ".csv"))
    )
  }

  # Return the result
  return(result)
}
