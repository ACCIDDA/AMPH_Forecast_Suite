



# Set parameters

# forecast_date = "2024-12-01" #"2025-10-12" # Sunday
forecast_date_series <- lubridate::as_date("2024-12-01") + lubridate::weeks(-2:10) #"2025-10-12" # Sunday
forecast_date <- forecast_date_series[1]

state_name <- "Maryland"
geo_ids <- "md"
location_id <- "24"
forecast_disease <- "influenza"
target <- "wk inc flu hosp"
forecast_horizon_wks <- 0:3


library(AMPHForecastSuite)
library(tidyverse)
library(jsonlite)
library(epidatr)
library(epiprocess)
library(forecast)
library(jsonlite)
library(epipredict)


# 1) Set up project file structure
setup_file_structure(project_dir = "")



# Loop over forecast dates
for (forecast_date in forecast_date_series) {

  message("Running forecast for date: ", forecast_date)


  # PULL DATA ---------------------------------------------------------------

  # Pull NHSN hospitalization data
  target_data <- get_nhsn_data(
    disease = forecast_disease, #"influenza"  or "rsv" or "covid"
    geo_values = geo_ids,
    forecast_date = forecast_date,
    save_data = TRUE
  )

  ## Save it
  write_csv(target_data, file = file.path("target-data", paste0("target-hospital-admissions-", forecast_date, ".csv")))




  # FORECASTING -------------------------------------------------------------


  # Load data saved from a specific forecast date:
  target_data_path <- file.path("target-data", paste0("target-hospital-admissions-", forecast_date, ".csv"))

  target_data <- readr::read_csv(file = target_data_path) %>%
    mutate(location = as.character(location)) %>%
    dplyr::filter(!is.na(observation)) %>%        # keep rows with observed values
    dplyr::arrange(dplyr::across(dplyr::any_of(c("target_end_date","date"))))

  # get location id to add to forecast output
  location_dat <- target_data %>%
    dplyr::select(location, abbreviation, location_name) %>%
    distinct()
  location <- as.character(location_dat$location)


  ## Define Reference date
  #  -- The `reference_date` is the Saturday for the week of the forecast date. This is the date that will be used in the forecast submission file.
  reference_date <- get_reference_date(forecast_date)

  message("Reference date: ", reference_date)
  message("Forecast date: ", forecast_date)




  # ~ SARIMA (`forecast` package) ---------------------------------------------


  model_name <- "AMPH-sarima"
  fc_sarima <- forecast::auto.arima(y = target_data$observation,
                                    seasonal = T,
                                    lambda = "auto") %>%
    forecast::forecast(h = length(forecast_horizon_wks),
                       level = c(.1, .2, .3, .4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.98))

  # Transform to hubVerse format
  data_fc_sarima <- trans_forecastpkg_hv(fc_output = fc_sarima,
                                         model_name = model_name,
                                         target = target,
                                         reference_date = reference_date,
                                         horizon_time_steps = forecast_horizon_wks,
                                         geo_ids = location)

  ## Save data file
  save_model_output(model_name = model_name,
                    fc_output = data_fc_sarima,
                    reference_date)





  # ~ Neural network model (`forecast` package) ------------------------------

  model_name <- "AMPH-neuralnetwork"
  fc_nnet <- forecast::nnetar(target_data$observation,
                              lambda = "auto") %>%
    forecast::forecast(PI = TRUE,
                       h = length(forecast_horizon_wks),
                       level = c(.1, .2, .3, .4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.98))
  # Transform to hubVerse format
  data_fc_nnet <- trans_forecastpkg_hv(fc_output = fc_nnet,
                                       model_name = model_name,
                                       target = target,
                                       reference_date = reference_date,
                                       horizon_time_steps = forecast_horizon_wks,
                                       geo_ids = location)

  ## Save data file
  save_model_output(model_name = model_name,
                    fc_output = data_fc_nnet,
                    reference_date)




  # ~ Autoregressive Forecaster (`epipredict` package)  ---------------------

  model_name <- "AMPH-epipredict-arx"
  # Set up data for epipredict
  target_data_arx <- target_data %>%
    rename(geo_value = location,
           time_value = target_end_date,
           value = observation) %>%
    tsibble::as_tsibble(index = time_value, key = c(geo_value)) %>%
    arrange(geo_value, time_value) %>%
    epiprocess::as_epi_df()
  # Run model
  arx_forecast <- lapply(
    seq(7, 28, 7),
    function(days_ahead) {
      epipredict::arx_forecaster(
        epi_data = target_data_arx,
        outcome = "value",
        trainer = parsnip::linear_reg(),
        predictors = "value",
        args_list = arx_args_list(
          lags = list(c(0, 7, 14)),
          ahead = days_ahead,
          quantile_levels = c(0.01, 0.025, seq(.05, .95, .05), 0.975, 0.99),
          nonneg = TRUE
        )
      )
    }
  )
  # Transform to hubVerse format
  data_fc_arx_epipred <- trans_epipredarx_hv(fc_output = arx_forecast,
                                             model_name = model_name,
                                             target = target,
                                             reference_date = reference_date,
                                             horizon_time_steps = forecast_horizon_wks)
  ## Save data file
  save_model_output(model_name = model_name,
                    fc_output = data_fc_arx_epipred,
                    reference_date)



  # ~ Climatological Forecaster (`epipredict` package) ----------------------

  model_name <- "AMPH-epipredict-climate"

  # Set up data for epipredict
  target_data_clim <- target_data %>%
    rename(geo_value = location,
           time_value = target_end_date,
           value = observation) %>%
    tsibble::as_tsibble(index = time_value, key = c(geo_value)) %>%
    arrange(geo_value, time_value) %>%
    epiprocess::as_epi_df()

  # Run the model
  climate_forecast <- epipredict::climatological_forecaster(
    target_data_clim,
    outcome = "value",
    args_list = climate_args_list(
      forecast_horizon = forecast_horizon_wks,
      time_type = "week",
      quantile_levels = c(0.01, 0.025, seq(.05, .95, .05), 0.975, 0.99),
      center_method = "mean",
      quantile_by_key = "geo_value",
      forecast_date = reference_date,
      nonneg = TRUE
    )
  )
  hub_forecast <- trans_epipredclim_hv(fc_output = climate_forecast,
                                       model_name = model_name,
                                       target = target,
                                       reference_date = reference_date,
                                       horizon_time_steps = forecast_horizon_wks)

  ## Save data file
  save_model_output(model_name = model_name,
                    fc_output = data_fc_arx_epipred,
                    reference_date)





  # PULL FLUSIGHT -----------------------------------------------------------



  # 2) Get FluSight data repo
  repo_dir <- clone_hub_repos(disease = forecast_disease,
                              clone_dir = getwd())

  # We will copy forecasts from a set of models from FluSight. These include:
  # - FluSight-baseline
  # - MOBS-GLEAM_FLUH
  # - FluSight-ensemble

  # models from flusight
  models_to_copy <- c(
    "FluSight-baseline",
    "MOBS-GLEAM_FLUH",
    "FluSight-ensemble")

  #models from AMPH
  models_created_in_AMPH <- list.dirs("model-output",
                                      full.names = FALSE,
                                      recursive = FALSE)
  # copy Forecast Hub forecasts to model-output folder
  copy_fch_outputs(repo_dir,
                   reference_date,
                   models_to_copy)


  # 4) Load model output (hub forecasts & your forecasts)

  output_path <- file.path("model-output")

  # Retrieve parquet/csv model output files and keep those matching the reference date
  file_paths <- list.files(output_path, pattern = "\\.(parquet|csv)$",
                           full.names = TRUE, recursive = TRUE)
  file_paths <- file_paths[grepl(reference_date, file_paths)]



  # Read & bind; keep quantile forecasts; add model_id from folder name

  projection_data_all <- file_paths %>%
    purrr::map_dfr(function(.x) {
      df <- read_model_file(.x)

      # standardize expected columns just in case
      if (!"output_type" %in% names(df))   stop("Missing 'output_type' in: ", .x)
      if (!"output_type_id" %in% names(df)) stop("Missing 'output_type_id' in: ", .x)

      df %>%
        dplyr::filter(.data$output_type == "quantile") %>%
        dplyr::mutate(
          output_type_id = suppressWarnings(as.numeric(.data$output_type_id)),
          model_id = basename(dirname(.x)),
          location = as.character(location)
        )
    }) %>%
    dplyr::filter(location %in% location_id)

  prep_proj_data <- projection_data_all %>%
    dplyr::mutate(
      target_end_date = dplyr::coalesce(target_end_date, reference_date + 7 * as.integer(horizon))
    ) %>%
    dplyr::select(-tidyselect::any_of(c("model", "origin_date")))

  # Convert to hubverse model_out_tbl format
  projection_data_tbl <- hubUtils::as_model_out_tbl(prep_proj_data) %>%
    dplyr::filter(model_id %in% c(
      models_created_in_AMPH,
      models_to_copy
    ))


  # Read and join location metadata (for names/abbreviations)

  # loc_data <- readr::read_csv(file.path(dir_path, "auxiliary-data", "locations.csv"),
  #                             show_col_types = FALSE)
  data(loc_data, package = "AMPHForecastSuite")

  projection_data_tbl2 <- projection_data_tbl %>%
    dplyr::select(-starts_with("location_name")) %>%
    dplyr::left_join(
      loc_data %>%
        dplyr::select(location, location_name),
      by = "location"
    )


  # 5) Pick location, start date, and uncertainty bands

  # Location can be "US" or a full state name (must match location_name in target_data)
  loc <- state_name
  start_date <- lubridate::as_date(reference_date) - lubridate::weeks(12)
  # Middle 80% interval:
  uncertainty <- c(0.1, 0.9)




  # BUILD ENSEMBLE ----------------------------------------------------------
  # - Build a simple equal-weight ensemble

  # Filter to the location of interest and the chosen forecast round
  round_dat <- projection_data_tbl2 %>%
    dplyr::filter(.data$location_name == loc,
                  target == target,
                  output_type == "quantile",
                  horizon >= 0) %>%
    dplyr::collect()

  # Generate a simple (equal-weight) ensemble across contributing models
  round_ens <- hubEnsembles::simple_ensemble(
    round_dat %>%
      dplyr::filter(model_id %in% c("AMPH-SARIMA","AMPH-neuralnetwork", "MOBS-GLEAM_FLUH"))) %>%
    # dplyr::filter(!(model_id %in% c("FluSight-baseline", "AMPH-SARIMA","AMPH-neuralnetwork",
    #                                 "FluSight-ensemble",
    #                                 "AMPH-epipredict-climate")))) %>%
    mutate(model_id = "AMPH-ensemble")

  # Save ensemble output
  save_model_output(model_name = "AMPH-ensemble",
                    fc_output = round_ens,
                    reference_date)

}



# 7) Prepare data for visualization
# use the latest refenece date to pull data again

# pull updated target data
new_target_data_date <- lubridate::as_date(reference_date) + lubridate::weeks(8)
target_data_plot <- get_nhsn_data(
  disease = forecast_disease,
  geo_values = geo_ids,
  forecast_date = new_target_data_date,
  save_data = TRUE
)










# target_data_plot <- readr::read_csv(
#   file.path("target-data", paste0("target-hospital-admissions-", new_target_data_date, ".csv")),
#   show_col_types = FALSE)
#
# # Forecasts to tidy plot
# proj_data <- hubUtils::as_model_out_tbl(plot_df) %>%
#   dplyr::rename(target_date = target_end_date) %>%
#   dplyr::mutate(output_type_id = suppressWarnings(as.numeric(output_type_id))) %>%
#   dplyr::arrange(model_id, horizon, target_date, output_type_id) %>%
#   dplyr::distinct(model_id, horizon, target_date, output_type_id, .keep_all = TRUE)
#
# # Observed data for the same location and time window
# target_data_plot <- target_data_plot %>%
#   dplyr::filter(target_end_date > start_date) %>%
#   dplyr::rename(date = target_end_date)
#




# # 10) Score forecasts (WIS, coverage)
#
# Evaluation of forecasts is critical for assessing model performance and for building trust in forecasts. Proper scoring rules, such as the Weighted Interval Score (WIS), provide a way to evaluate the accuracy and calibration of probabilistic forecasts. Scoring requires observed data. We will use the `scoringutils` package to compute WIS for our forecasts.
#
# ```{r scoring-prep}
#
# scoring_target_data <- readr::read_csv(
#   file.path("target-data", paste0("target-hospital-admissions-", new_target_data_date, ".csv")),
#   show_col_types = FALSE)
#
# scoring_target_data <- scoring_target_data %>%
#   filter(location %in% location,
#          issue_date >= target_end_date + 28,
#          target_end_date > "2022-09-01") %>%
#   select(geo_value = location, target_end_date, value = observation) %>%
#   drop_na(value) %>%
#   epiprocess::as_epi_df(time_value = target_end_date)
# ```
#
# ```{r scoring, eval=TRUE}
# # Join forecasts with observations at (target_date, location)
# # and conform to scoringutils "forecast" structure.
#
# scoring_df <- dplyr::left_join(
#   proj_data,
#   scoring_target_data %>%
#     dplyr::rename(observation = value,
#                   target_date = time_value) %>%
#     mutate(location_name = "Maryland") %>%
#     select(-geo_value),
#   by = c("target_date", "location_name"),
#   relationship = "many-to-one"
# ) %>%
#   dplyr::rename(
#     model = model_id,
#     predicted = value,
#     observed = observation,
#     quantile_level = output_type_id
#   )
#
# # Convert to a scoringutils forecast object
# forecast <- scoringutils::as_forecast_quantile(
#   scoring_df,
#   observed       = "observed",
#   predicted      = "predicted",
#   quantile_level = "quantile_level",
#   # be explicit so extra cols don't confuse the unit of a single forecast
#   forecast_unit  = c("model", "location_name", "target_date")
# )
#
# # Score (WIS, coverage, etc.)
# scores <- scoringutils::score(forecast)
# ```
#
#
# ```{r, eval=TRUE}
# knitr::kable(scoringutils::summarise_scores(scores, by = "model"))
#
# ```












