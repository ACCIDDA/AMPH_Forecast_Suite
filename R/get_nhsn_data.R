#' #' Get NHSN Weekly Hospitalization Data
#' #'
#' #' Pulls weekly hospitalization count data from the NHSN (National Healthcare
#' #' Safety Network) via the epidatr package for influenza, COVID-19, or RSV.
#' #'
#' #' @param disease Character string specifying the disease. Must be one of:
#' #'   "influenza", "covid", or "rsv" (case-insensitive).
#' #' @param geo_values Character vector of geographic locations (e.g., "US", state
#' #'   abbreviations like "CA", "NY"). Default is "US" for national data.
#' #' @param time_values Character vector or range specifying the time period in
#' #'   epiweek format (YYYYWW) or using epidatr time helpers like
#' #'   epirange(202001, 202052). If NULL, retrieves all available data.
#' #' @param ... Additional arguments passed to epidatr::pub_nhs_facilities()
#' #'
#' #' @return A data frame containing NHSN hospitalization data with columns
#' #'   typically including geo_value, time_value, and hospitalization counts.
#' #'
#' #' @examples
#' #' \dontrun{
#' #' # Get national COVID-19 data for all available weeks
#' #' covid_data <- get_nhsn_data(disease = "covid")
#' #'
#' #' # Get influenza data for California and New York for specific weeks
#' #' flu_data <- get_nhsn_data(
#' #'   disease = "influenza",
#' #'   geo_values = c("ca", "ny"),
#' #'   time_values = epirange(202201, 202252)
#' #' )
#' #'
#' #' # Get RSV data for the US
#' #' rsv_data <- get_nhsn_data(disease = "rsv", geo_values = "US")
#' #' }
#' #'
#' #' @export
#'
#' #' @importFrom epidatr pub_nhs_facilities
#' get_nhsn_data <- function(disease,
#'                           geo_values = "US",
#'                           time_values = NULL,
#'                           ...) {
#'
#'   # Validate disease parameter
#'   disease <- tolower(disease)
#'   valid_diseases <- c("influenza", "covid", "rsv")
#'
#'   if (!disease %in% valid_diseases) {
#'     stop(sprintf(
#'       "Invalid disease: '%s'. Must be one of: %s",
#'       disease,
#'       paste(valid_diseases, collapse = ", ")
#'     ))
#'   }
#'
#'   # Map disease names to NHSN signal names
#'   # Based on epidatr NHSN signals for respiratory diseases
#'   signal_map <- list(
#'     "influenza" = "confirmed_influenza_weekly",
#'     "covid" = "confirmed_covid_weekly",
#'     "rsv" = "confirmed_rsv_weekly"
#'   )
#'
#'   signal <- signal_map[[disease]]
#'
#'   # Call epidatr to get the data
#'   if (is.null(time_values)) {
#'     # Get all available data
#'     result <- epidatr::pub_nhs_facilities(
#'       locations = geo_values,
#'       signals = signal,
#'       ...
#'     )
#'   } else {
#'     # Get data for specific time range
#'     result <- epidatr::pub_nhs_facilities(
#'       locations = geo_values,
#'       signals = signal,
#'       dates = time_values,
#'       ...
#'     )
#'   }
#'
#'   # Return the result
#'   return(result)
#' }
