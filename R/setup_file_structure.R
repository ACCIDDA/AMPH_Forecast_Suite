#' Set Up File Structure for a Forecasting Project
#'
#' This function helps install all the necessary packages for building an
#' infectious disease forecasting pipeline and hub, including hubverse packages,
#' fable, EpiEstim, EpiNow2, epidatr, and epiforecast packages.
#'
#' @param project_dir Character. Directory to set up the project structure. Default is "AMPH_forecast_project".
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
setup_file_structure <- function(project_dir = "AMPH_forecast_project") {
  if (!is.character(project_dir) || length(project_dir) != 1) {
    stop("project_dir must be a single character string.")
  }

  if (project_dir == "") {
    message("Using current working directory as project directory.")
    project_dir <- getwd()
  }

  dir.create(project_dir, recursive = TRUE, showWarnings = FALSE)
  setwd(project_dir)
  dir.create("model-output")
  dir.create("target-data")

  message("Directory setup complete!")
  invisible(NULL)
}

