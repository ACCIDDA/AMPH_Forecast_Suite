#' Transform Epipredict ARX Output to appropriate hubVerse format
#'
#' Converts the output from an ARX forecasting model (e.g., arx_forecast)
#' to the required hub format for submission to a forecasting hub.
#'
#' @param fc_output List. Output from the ARX forecasting model (e.g., arx_forecast).
#' @param model_name Character. Name of the forecasting model. Default is "AMPH-epipredict-arx".
#' @param target Character. Target variable name. Default is "wk inc flu hosp".
#' @param reference_date Date. The date of the forecast reference. This should be in "YYYY-MM-DD" format.
#'
#' @return tibble. A data frame in hub format.
#' @export
#'
#' @examples
#' \dontrun{
#' library(AMPHForecastSuite)
#' setup_file_structure(project_dir = "My_Forecast_Project")
#' # Assuming you have ARX forecast output
#' hub_forecast <- trans_epipredarx_hv(fc_output = arx_forecast,
#'                     model_name = "AMPH-epipredict-arx",
#'                     target = "wk inc flu hosp",
#'                     reference_date = forecast_date + 6,
#'                     horizon_time_steps = 0:3)
#' }
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr `%>%`
#' @importFrom epipredict pivot_quantiles_longer
#' @importFrom purrr map list_rbind
#' @importFrom lubridate as_date
#'
trans_epipredarx_hv <- function(fc_output = arx_forecast,
                                model_name = "AMPH-epipredict-arx",
                                target = "wk inc flu hosp",
                                reference_date,
                                horizon_time_steps = 0:3) {

  forecast_res <- fc_output %>%
    purrr::map(~ `$`(., "predictions")) %>%
    purrr::list_rbind() %>%
    dplyr::mutate(horizon = horizon_time_steps,
                  location = geo_value,
                  model = model_name,
                  target = target)

  forecast_res_long <- forecast_res %>%
    dplyr::select(-.pred) %>%
    epipredict::pivot_quantiles_longer(.pred_distn) %>%
    dplyr::mutate(output_type = "quantile",
                  reference_date = lubridate::as_date(reference_date),
                  target_end_date = lubridate::as_date(target_date) + 6) %>%
    dplyr::select(target_end_date, reference_date, target, horizon, location, model,
                  output_type, output_type_id = .pred_distn_quantile_level, value = .pred_distn_value)

  return(forecast_res_long)
}


