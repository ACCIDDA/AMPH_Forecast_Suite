#' Convert Hub Format to EpiEstim Format
#'
#' Converts data from hub format to the format required by EpiEstim package
#' for estimating time-varying reproduction numbers.
#'
#' @param hub_data Data frame in hub format with date and incidence columns
#' @param date_col Character. Name of the date column. Default is "date".
#' @param incidence_col Character. Name of the incidence column. Default is "value".
#' @param location_col Character. Name of the location column. Default is "location".
#' @param location_filter Character. Location to filter for (if multiple locations). Default is NULL.
#'
#' @return A data frame formatted for EpiEstim with columns: dates, I (incidence)
#' @export
#' @importFrom dplyr %>% filter select mutate arrange
#' @importFrom lubridate ymd
#'
#' @examples
#' \dontrun{
#' hub_data <- data.frame(
#'   date = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 50),
#'   location = "US",
#'   value = rpois(50, 100)
#' )
#' epiestim_data <- convert_hub_to_epiestim(hub_data, location_filter = "US")
#' }
convert_hub_to_epiestim <- function(hub_data,
                                   date_col = "date",
                                   incidence_col = "value",
                                   location_col = "location",
                                   location_filter = NULL) {
  
  if (!requireNamespace("EpiEstim", quietly = TRUE)) {
    warning("Package 'EpiEstim' is not installed. Install it for full functionality.")
  }
  
  # Filter by location if specified
  if (!is.null(location_filter)) {
    hub_data <- hub_data %>%
      dplyr::filter(!!rlang::sym(location_col) == location_filter)
  }
  
  # Create EpiEstim format
  result <- hub_data %>%
    dplyr::select(dates = !!rlang::sym(date_col),
                  I = !!rlang::sym(incidence_col)) %>%
    dplyr::arrange(dates)
  
  # Ensure dates are Date type
  result$dates <- lubridate::as_date(result$dates)
  
  # EpiEstim expects integer incidence
  result$I <- as.integer(round(result$I))
  
  return(result)
}


#' Convert EpiEstim Output to Hub Format
#'
#' Converts EpiEstim reproduction number estimates to hub format.
#'
#' @param epiestim_result EpiEstim result object (output from estimate_R)
#' @param location Character. Location identifier. Default is "unknown".
#' @param target Character. Target variable name. Default is "R_eff".
#'
#' @return A data frame in hub format with R estimates
#' @export
#' @importFrom dplyr %>% mutate select
#' @importFrom tibble tibble
#'
#' @examples
#' \dontrun{
#' # Assuming you have EpiEstim results
#' # hub_r_estimates <- convert_epiestim_to_hub(epiestim_res, location = "US")
#' }
convert_epiestim_to_hub <- function(epiestim_result,
                                   location = "unknown",
                                   target = "R_eff") {
  
  if (!inherits(epiestim_result, "estimate_R")) {
    stop("Input must be an EpiEstim estimate_R result object")
  }
  
  # Extract R estimates
  r_data <- epiestim_result$R
  
  # Convert to hub format
  result <- tibble::tibble(
    date = r_data$t_end,  # Using end date of time window
    location = location,
    target = target,
    value = r_data$`Mean(R)`,
    lower_ci = r_data$`Quantile.0.025(R)`,
    upper_ci = r_data$`Quantile.0.975(R)`
  )
  
  return(result)
}
