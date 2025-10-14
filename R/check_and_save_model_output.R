#' Check and Save Model Output for Forecasting Hub
#'
#' This function helps save the output of a forecasting model into a structured
#' directory format suitable for submission to a forecasting hub. It creates
#' necessary directories if they do not exist and saves the forecast output
#' as a CSV file named with the forecast date and model name.
#'
#' @param model_name Character. Name of the forecasting model (e.g., "AMPH-SARIMA").
#' @param fc_output Data frame. Forecast output to be saved.
#' @param forecast_date Character. Date of the forecast in "YYYY-MM-DD" format.
#'
#' @return NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' library(AMPHForecastSuite)
#' setup_file_structure(project_dir = "My_Forecast_Project")
#' }
#'
save_model_output <- function(model_name = "AMPH-SARIMA",
                              fc_output,
                              reference_date) {

  if (!is.character(model_name) || length(model_name) != 1) {
    stop("model_name must be a string.")
  }

  if (!is.data.frame(fc_output)) {
    stop("fc_output must be a data frame.")
  }

  # # Check model output using hubValidations package
  # if (requireNamespace("hubValidations", quietly = TRUE)) {
  #   validation_result <- hubValidations::validate_model_output(fc_output)
  #   if (!validation_result$valid) {
  #     warning("Model output validation failed: ", paste(validation_result$errors, collapse = "; "))
  #   } else {
  #     message("Model output validation passed.")
  #   }
  # } else {
  #   stop("Package 'hubValidations' is not installed. Please install `hubValidations` package before proceeding.")
  # }

  # Create output directory for the model
  output_dir <- file.path("model-output", model_name)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  fc_filename <- file.path(output_dir,
                           sprintf("%s-%s.csv", reference_date, model_name))
  # Save it
  readr::write_csv(fc_output, fc_filename)
  message("Model output saved to ", fc_filename)
  invisible(NULL)
}


