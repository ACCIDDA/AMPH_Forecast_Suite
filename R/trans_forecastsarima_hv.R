#' Transform forecast package Output to appropriate hubVerse format
#'
#' Converts the output from a forecasting model from the `forecast` package
#' to the required hub format for submission to a forecasting hub.
#'
#' @param fc_output Data frame. Output from a `forecast` package forecasting model.
#' @param model_name Character. Name of the forecasting model. Default is "AMPH-epipredict-arx".
#' @param target Character. Target variable name. Default is "wk inc flu hosp".
#' @param reference_date Date. The date of the forecast reference. This should be in "YYYY-MM-DD" format.
#' @param horizon_time_steps Integer vector. Time steps for the forecast horizon. Default is 0:3 (0 to 3 weeks ahead).
#' @param geo_ids Character vector. Geographic location identifiers. Default is "md".
#'
#' @return tibble. A data frame in hub format.
#' @export
#'
#' @examples
#' \dontrun{
#' library(AMPHForecastSuite)
#' setup_file_structure(project_dir = "My_Forecast_Project")
#' # Assuming you have ARX forecast output
#' hub_forecast <- trans_forecastpkg_hv(fc_output = fc_sarima,
#'                    model_name = "AMPH-sarima",
#'                    target = "wk inc flu hosp",
#'                    reference_date = forecast_date + 6,
#'                    horizon_time_steps = 0:3,
#'                    geo_ids = "md")
#'                    }
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr `%>%`
#' @importFrom epipredict pivot_quantiles_longer
#' @importFrom purrr map list_rbind
#' @importFrom lubridate as_date
#' @importFrom tidyr pivot_longer separate
#' @importFrom tibble as_tibble
#'
#'
#'
trans_forecastpkg_hv <- function(fc_output = fc_sarima,
                                model_name = "AMPH-sarima",
                                target = "wk inc flu hosp",
                                reference_date,
                                horizon_time_steps = 0:3,
                                geo_ids = "md") {

  forecast_res <- fc_output %>%
    tibble::as_tibble() %>%
    dplyr::mutate(horizon = horizon_time_steps,
                  location = geo_ids,
                  model = model_name,
                  target = target) %>%
    tidyr::pivot_longer(-c(horizon, location, model, target)) %>%
    dplyr::mutate(name = case_when(
      name == "Point Forecast" ~ "median",
      TRUE ~ name)) %>%
    tidyr::separate(name, into = c("interval", "confidence"),
                    sep = " ",
                    extra = "merge",  # merges extra pieces into last column
                    fill = "right"    # fills missing pieces with NA
    ) %>%
    dplyr::mutate(output_type_id = case_when(
      interval == "Lo" ~ (50 - as.numeric(confidence)/2)/100,
      interval == "Hi" ~ (50 + as.numeric(confidence)/2)/100,
      interval == "median" ~ 50/100),
      value = as.numeric(value),
      output_type = "quantile",
      reference_date = lubridate::as_date(reference_date)) %>%
    dplyr::mutate(target_end_date = lubridate::as_date(reference_date) + (horizon * 7)) %>%
    dplyr::select(target_end_date, reference_date, target, horizon, location, model,
                  output_type, output_type_id, value) %>%
    dplyr::arrange(location, model, target, horizon, output_type_id)

  return(forecast_res)
}


