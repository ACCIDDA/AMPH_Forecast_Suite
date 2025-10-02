#' Convert Hub Format to EpiNow2 Format
#'
#' Converts data from hub format to the format required by EpiNow2 package
#' for estimating reproduction numbers and generating forecasts.
#'
#' @param hub_data Data frame in hub format with date and case columns
#' @param date_col Character. Name of the date column. Default is "date".
#' @param cases_col Character. Name of the cases column. Default is "value".
#' @param location_col Character. Name of the location column. Default is "location".
#' @param location_filter Character. Location to filter for (if multiple locations). Default is NULL.
#'
#' @return A data frame formatted for EpiNow2 with columns: date, confirm
#' @export
#' @importFrom dplyr %>% filter select mutate arrange rename
#' @importFrom lubridate as_date
#'
#' @examples
#' \dontrun{
#' hub_data <- data.frame(
#'   date = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 50),
#'   location = "US",
#'   value = rpois(50, 100)
#' )
#' epinow2_data <- convert_hub_to_epinow2(hub_data, location_filter = "US")
#' }
convert_hub_to_epinow2 <- function(hub_data,
                                  date_col = "date",
                                  cases_col = "value",
                                  location_col = "location",
                                  location_filter = NULL) {
  
  if (!requireNamespace("EpiNow2", quietly = TRUE)) {
    warning("Package 'EpiNow2' is not installed. Install it for full functionality.")
  }
  
  # Filter by location if specified
  if (!is.null(location_filter)) {
    hub_data <- hub_data %>%
      dplyr::filter(!!rlang::sym(location_col) == location_filter)
  }
  
  # Create EpiNow2 format
  result <- hub_data %>%
    dplyr::select(date = !!rlang::sym(date_col),
                  confirm = !!rlang::sym(cases_col)) %>%
    dplyr::arrange(date)
  
  # Ensure date is Date type
  result$date <- lubridate::as_date(result$date)
  
  # EpiNow2 expects integer case counts
  result$confirm <- as.integer(round(result$confirm))
  
  return(result)
}


#' Convert EpiNow2 Output to Hub Format
#'
#' Converts EpiNow2 estimates and forecasts to hub submission format.
#'
#' @param epinow2_result EpiNow2 result object (output from epinow)
#' @param location Character. Location identifier. Default is "unknown".
#' @param target Character. Target variable name. Default is "inc".
#' @param extract_type Character. Type of data to extract: "forecasts", "estimates", or "both". Default is "forecasts".
#'
#' @return A data frame in hub format
#' @export
#' @importFrom dplyr %>% mutate select bind_rows
#' @importFrom tibble tibble
#'
#' @examples
#' \dontrun{
#' # Assuming you have EpiNow2 results
#' # hub_forecasts <- convert_epinow2_to_hub(epinow2_res, location = "US")
#' }
convert_epinow2_to_hub <- function(epinow2_result,
                                  location = "unknown",
                                  target = "inc",
                                  extract_type = "forecasts") {
  
  if (!inherits(epinow2_result, "epinow")) {
    warning("Input may not be an EpiNow2 epinow result object")
  }
  
  result_list <- list()
  
  # Extract forecasts
  if (extract_type %in% c("forecasts", "both")) {
    if ("predictions" %in% names(epinow2_result)) {
      forecasts <- epinow2_result$predictions
      
      forecast_df <- tibble::tibble(
        date = forecasts$date,
        location = location,
        target = paste0(target, "_forecast"),
        value = forecasts$median,
        lower_ci = forecasts$lower_90,
        upper_ci = forecasts$upper_90
      )
      
      result_list$forecasts <- forecast_df
    }
  }
  
  # Extract estimates
  if (extract_type %in% c("estimates", "both")) {
    if ("estimates" %in% names(epinow2_result)) {
      estimates <- epinow2_result$estimates$summarised
      
      # Extract R estimates
      r_estimates <- estimates[estimates$variable == "R", ]
      
      estimate_df <- tibble::tibble(
        date = r_estimates$date,
        location = location,
        target = "R_eff",
        value = r_estimates$median,
        lower_ci = r_estimates$lower_90,
        upper_ci = r_estimates$upper_90
      )
      
      result_list$estimates <- estimate_df
    }
  }
  
  # Combine results
  if (length(result_list) > 0) {
    result <- dplyr::bind_rows(result_list)
  } else {
    result <- tibble::tibble()
    warning("No data extracted from EpiNow2 result")
  }
  
  return(result)
}
