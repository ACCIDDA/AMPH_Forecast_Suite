#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom lubridate as_date
#' @importFrom lubridate ymd
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom purrr map_dfr
## usethis namespace: end
NULL

#' AMPH Forecast Suite
#'
#' A comprehensive suite of tools and helper functions to build and run
#' an infectious disease forecasting hub. This package facilitates the
#' installation and integration of key forecasting packages including
#' hubverse packages, fable, EpiEstim, EpiNow2, epidatr, and epiforecast.
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{\link{install_forecast_packages}}: Install all required forecasting packages
#'   \item \code{\link{convert_hub_to_fable}}: Convert hub data to fable format
#'   \item \code{\link{convert_fable_to_hub}}: Convert fable forecasts to hub format
#'   \item \code{\link{convert_hub_to_epiestim}}: Convert hub data to EpiEstim format
#'   \item \code{\link{convert_epiestim_to_hub}}: Convert EpiEstim results to hub format
#'   \item \code{\link{convert_hub_to_epinow2}}: Convert hub data to EpiNow2 format
#'   \item \code{\link{convert_epinow2_to_hub}}: Convert EpiNow2 results to hub format
#'   \item \code{\link{get_nhsn_data}}: Retrieve data from NHSN
#' }
#'
#' @docType package
#' @name AMPHForecastSuite-package
#' @aliases AMPHForecastSuite
NULL
