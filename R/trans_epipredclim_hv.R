#' Transform Epipredict Climate Output to appropriate hubVerse format
#'
#' Converts the output from a climatological_forecaster model
#' to the required hub format for submission to a forecasting hub.
#'
#' @param fc_output List. The output from the climatological_forecaster model, typically containing a 'predictions' data frame.
#' @param model_name Character. Name of the forecasting model. Default is "AMPH-epipredict-climate".
#' @param target Character. Target variable name. Default is "wk inc flu hosp".
#' @param reference_date Date. The date of the forecast reference. This should be in "YYYY-MM-DD" format.
#' @param horizon_time_steps Integer vector. Time steps for the forecast horizon. Default is 0:3 (0 to 3 weeks ahead).
#'
#' @return tibble. A data frame in hub format.
#' @export
#'
#' @examples
#' \dontrun{
#' library(AMPHForecastSuite)
#' setup_file_structure(project_dir = "My_Forecast_Project")
#' # Assuming you have climate_forecast output from climatological_forecaster hub_format
#' hub_forecast <- trans_epipredclim_hv(fc_output = climate_forecast,
#'                                     model_name = "AMPH-epipredict-climate",
#'                                     target = "wk inc flu hosp",
#'                                     reference_date = forecast_date + 6,
#'                                     horizon_time_steps = 0:3)
#' }
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr `%>%`
#' @importFrom epipredict pivot_quantiles_longer
#' @importFrom purrr map list_rbind
#' @importFrom lubridate as_date
#'
#'
trans_epipredclim_hv <- function(fc_output = climate_forecast,
                                model_name = "AMPH-epipredict-climate",
                                target = "wk inc flu hosp",
                                reference_date,
                                horizon_time_steps = 0:3) {

  forecast_res <- fc_output$predictions %>%
    dplyr::mutate(horizon = horizon_time_steps,
           location = geo_value,
           model = model_name,
           target = target)

  forecast_res_long <- forecast_res %>%
    dplyr::select(-.pred) %>%
    epipredict::pivot_quantiles_longer(.pred_distn) %>%
    dplyr::mutate(output_type = "quantile",
                  reference_date = lubridate::as_date(reference_date),
                  target_end_date = lubridate::as_date(target_date)) %>%
    dplyr::select(target_end_date, reference_date, target, horizon, location, model,
                  output_type, output_type_id = .pred_distn_quantile_level, value = .pred_distn_value)

  return(forecast_res_long)
}


