

# "Usage: Rscript scripts/external_to_projections.R --output-path <dir> --pathogen <flu|rsv|covid>",
# "                --data-path <forecast.csv> --target-data-path <targets.(csv|parquet)>",
# "                --locations-data-path <locations.csv> [--overwrite] [--log-level <level>]",
# "",
# "Arguments:",
# "  --output-path          Directory where JSON files will be written.",
# "  --pathogen             Pathogen to process (flu, rsv, covid).",
# "  --data-path            Hubverse forecast export in CSV format.",
# "  --target-data-path     Hubverse target data (CSV or Parquet).",
# "  --locations-data-path  Location metadata CSV.",
# "  --overwrite            Overwrite existing files if present.",
# "  --log-level            Logging verbosity (DEBUG, INFO, WARNING, ERROR).",
# "  --help                 Show this message and exit.",
# sep = "\n"

# covid_hub_abs_path = '/Users/emprzy/Documents/work/covid19-forecast-hub'
# covid_locations_data = pd.read_csv(Path(covid_hub_abs_path) / 'auxiliary-data/locations.csv')
# covid_target_data = pd.read_parquet('/Users/emprzy/Documents/work/covid19-forecast-hub/target-data/time-series.parquet')
# covid_hub_conn = connect_hub(covid_hub_abs_path)
# cdf = covid_hub_conn.get_dataset().to_table().to_pandas()
#



library(AMPHForecastSuite)

# combine model outputs
combine_model_outout <- function(comb_file_path = "respiLens-output/AMPH_forecasts_comb.csv") {
  files_to_process <- list.files("model-output/", full.names = TRUE, recursive = TRUE)
  comb_forecasts <- lapply(files_to_process, function(f) {
    model_id <- strsplit(f, "/")[[1]]
    model_id <- model_id[length(model_id)]
    model_id <- gsub("^\\d{4}-\\d{2}-\\d{2}-|\\.csv$", "", model_id)
    AMPHForecastSuite::read_model_file(f) %>% dplyr::mutate(model_id = model_id) %>%
      dplyr::mutate(location = as.character(location),
                    output_type_id = as.character(output_type_id))
    }) %>%
    dplyr::bind_rows()
  readr::write_csv(comb_forecasts, comb_file_path)
  return(comb_forecasts)
}

# Convert CSV to Parquet
csv_to_parquet <- function(file_path) {
  arrow::write_parquet(readr::read_csv(file_path), gsub(".csv", ".parquet", file_path))
}

# Identify latest target data file
identify_latest_targetdata <- function(target_data_dir = "target-data") {
  target_data_files <- list.files(target_data_dir, full.names = TRUE)
  # look for date pattern in filename YYYY-MM-DD
  target_data_dates <- sapply(target_data_files, function(f) {
    sub(".*?(\\d{4}-\\d{2}-\\d{2}).*", "\\1", f)
  }) %>% lubridate::as_date()
  latest_file <- target_data_files[which.max(target_data_dates)]
  return(latest_file)
}




setup_args_and_data_respilens <- function(
    output_path = "respiLens-output",
    disease = "flu", # flu, rsv, covid
    data_path = "respiLens-output/AMPH_forecasts_comb.csv",
    target_data_path = "target-data/targets.csv",
    locations_data_path = "data/locations/locations.csv",
    overwrite = TRUE,
    log_level = "INFO"){

  # check pathogen options
  disease <- tolower(disease)

  # fix flu
  disease <- ifelse(disease == "influenza", "flu", disease)
  # fix covid
  disease <- ifelse(disease == "covid-19", "covid", disease)
  disease <- ifelse(disease == "covid19", "covid", disease)

  valid_diseases <- c("flu", "rsv", "covid")
  if (!disease %in% valid_diseases) {
    stop(sprintf(
      "ERROR: Invalid disease: '%s'. Must be one of: %s",
      disease,
      paste(valid_diseases, collapse = ", ")
    ))
  }

  # consolidate output data
  dir.create(output_path, showWarnings = FALSE, recursive = TRUE)
  comb_fc <- combine_model_outout(comb_file_path = data_path)

  # remove unneeded columns
  comb_fc <- comb_fc %>%
    dplyr::select(-tidyselect::matches("^model$"), -tidyselect::contains("location_name"))

  # setup target data
  target_data <- readr::read_csv(target_data_path) %>% dplyr::filter(!is.na(observation))
  target_data_path <- gsub(".csv", "_respiLens.csv", target_data_path)
  target_data <- target_data %>%
    dplyr::rename(as_of = issue_date) %>%
    dplyr::mutate(location = as.character(location))
  readr::write_csv(target_data, target_data_path)
  # csv_to_parquet(target_data_path) # Not necessary right now

  # prepare arguments list
  args <- list(
    output_path = output_path,
    pathogen = disease,
    data_path = data_path,
    target_data_path = target_data_path,
    locations_data_path = locations_data_path,
    overwrite = overwrite,
    log_level = log_level
  )

  # Assign simulated arguments to the global environment

  # Example usage in CLI:
  # emprzy ~/Documents/work/RespiLens-staging Rscript scripts/external_to_projections.R
  # --output-path /Users/emprzy/Documents/work/miscellaneous/output
  # --data-path /Users/emprzy/Documents/work/miscellaneous/RSV.csv
  # --target-data-path /Users/emprzy/Documents/work/rsv-forecast-hub/target-data/time-series.parquet
  # --locations-data-path /Users/emprzy/Documents/work/rsv-forecast-hub/auxiliary-data/locations.csv
  # --overwrite
  # --pathogen rsv

  assign("commandArgs", function(trailingOnly = TRUE) {
    return(c("--output-path", args$output_path,
             "--data-path", args$data_path,
             "--target-data-path", args$target_data_path,
             "--locations-data-path", args$locations_data_path,
             {if (args$overwrite) {"--overwrite"}},
             "--pathogen", args$pathogen,
             "--log-level", args$log_level))
  }, envir = .GlobalEnv)

  return(args)
}




# get latest target data file
latest_target_file <- identify_latest_targetdata(target_data_dir = "target-data")

# setup location data
data("loc_data")
loc_data_file <- "auxiliary-data/locations.csv"
dir.create(dirname(loc_data_file), showWarnings = FALSE, recursive = TRUE)
readr::write_csv(loc_data, loc_data_file)

# prepare arguments and data for respilens
args <- setup_args_and_data_respilens(
    output_path = "respiLens-output",
    disease = "flu", # flu, rsv, covid
    data_path = "respiLens-output/AMPH_forecasts_comb.csv",
    target_data_path = latest_target_file,
    locations_data_path = loc_data_file,
    overwrite = TRUE,
    log_level = "INFO")

# Run the respiLens data generator
run_main()








