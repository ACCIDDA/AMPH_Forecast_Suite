#!/usr/bin/env Rscript

# Convert Hubverse exports into RespiLens projection JSON files (R implementation)

suppressWarnings({
  options(stringsAsFactors = FALSE)
})

log_levels <- c("DEBUG", "INFO", "WARNING", "ERROR")

#' Usage text display
#'
#' @returns Description of Rscript call
#' @export
#'
#' @examples
usage_text <- function() {
  paste(
    "Usage: Rscript scripts/external_to_projections.R --output-path <dir> --pathogen <flu|rsv|covid>",
    "                --data-path <forecast.csv> --target-data-path <targets.(csv|parquet)>",
    "                --locations-data-path <locations.csv> [--overwrite] [--log-level <level>]",
    "",
    "Arguments:",
    "  --output-path          Directory where JSON files will be written.",
    "  --pathogen             Pathogen to process (flu, rsv, covid).",
    "  --data-path            Hubverse forecast export in CSV format.",
    "  --target-data-path     Hubverse target data (CSV or Parquet).",
    "  --locations-data-path  Location metadata CSV.",
    "  --overwrite            Overwrite existing files if present.",
    "  --log-level            Logging verbosity (DEBUG, INFO, WARNING, ERROR).",
    "  --help                 Show this message and exit.",
    sep = "\n"
  )
}

#' Parse the CLI arguments
#'
#' @returns Parsed list of CLI arguments
#' @export
#'
#' @examples
parse_cli_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if ("--help" %in% args || "-h" %in% args) {
    cat(usage_text(), "\n")
    quit(save = "no", status = 0, runLast = FALSE)
  }
  parsed <- list(
    overwrite = FALSE,
    log_level = "INFO"
  )
  i <- 1
  while (i <= length(args)) {
    key <- args[[i]]
    if (!startsWith(key, "--")) {
      stop(sprintf("Unexpected argument '%s'. Arguments must be provided as --key value pairs.", key), call. = FALSE)
    }
    if (key == "--overwrite") {
      parsed$overwrite <- TRUE
      i <- i + 1
      next
    }
    if (i == length(args)) {
      stop(sprintf("No value supplied for argument '%s'.", key), call. = FALSE)
    }
    value <- args[[i + 1]]
    name <- sub("^--", "", key)
    name <- gsub("-", "_", name)
    parsed[[name]] <- value
    i <- i + 2
  }

  required <- c("output_path", "pathogen", "data_path", "target_data_path", "locations_data_path")
  missing <- required[!required %in% names(parsed)]
  if (length(missing) > 0) {
    stop(sprintf("Missing required arguments: %s", paste(sprintf("--%s", gsub("_", "-", missing)), collapse = ", ")), call. = FALSE)
  }

  parsed$pathogen <- tolower(parsed$pathogen)
  parsed$log_level <- toupper(parsed$log_level)
  if (!parsed$log_level %in% log_levels) {
    stop(sprintf("Invalid --log-level '%s'. Choose one of: %s", parsed$log_level, paste(log_levels, collapse = ", ")), call. = FALSE)
  }
  parsed
}




#' Check for Required Package
#'
#' @param pkg Package to check for
#'
#' @returns
#' @export
#'
#' @examples
require_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required. Install it with install.packages('%s').", pkg, pkg), call. = FALSE)
  }
}

#' Log message with level and context
#'
#' @param level
#' @param message
#' @param ctx
#'
#' @returns
#' @export
#'
#' @examples
log_message <- function(level, message, ctx) {
  current_level <- match(ctx$args$log_level, log_levels)
  level_index <- match(level, log_levels)
  if (!is.na(level_index) && level_index >= current_level) {
    cat(sprintf("[%s] %s\n", level, message))
  }
}

#' Title
#'
#' @returns
#' @export
#'
#' @examples
get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  script_path <- sub(file_arg, "", cmd_args[grep(file_arg, cmd_args)])
  if (length(script_path) == 0) {
    return(getwd())
  }
  normalizePath(dirname(script_path))
}

check_required_columns <- function(df, required, source_label) {
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(sprintf("The file '%s' is missing required columns: %s", source_label, paste(missing, collapse = ", ")), call. = FALSE)
  }
}

clean_nan_values <- function(df) {
  replace_nan <- function(column) {
    if (is.numeric(column)) {
      column[is.nan(column)] <- NA_real_
    }
    column
  }
  df[] <- lapply(df, replace_nan)
  df
}

unique_in_order <- function(x) {
  x[!duplicated(x)]
}

hubverse_df_preprocessor <- function(df) {
  df <- df[!is.na(df$horizon), , drop = FALSE]
  df$horizon <- as.integer(df$horizon)
  categorical_ids <- c("decrease", "increase", "large_decrease", "large_increase", "stable")
  numeric_ids <- c(0.025, 0.25, 0.5, 0.75, 0.975)

  df <- df[df$output_type != "sample", , drop = FALSE]
  keep_rows <- logical(nrow(df))
  for (i in seq_len(nrow(df))) {
    row <- df[i, ]
    if (row$output_type == "quantile") {
      suppressWarnings({
        quant <- as.numeric(row$output_type_id)
      })
      keep_rows[i] <- !is.na(quant) && quant %in% numeric_ids
      if (keep_rows[i]) {
        df$output_type_id[i] <- quant
      }
    } else if (row$output_type == "pmf") {
      keep_rows[i] <- as.character(row$output_type_id) %in% categorical_ids
    } else {
      keep_rows[i] <- FALSE
    }
  }
  df <- df[keep_rows, , drop = FALSE]
  df
}

#' Load Clean Forecast Data
#'
#' @param path Path to forecast data CSV file
#'
#' @returns clean forecast data frame
#' @export
#'
load_forecast_data <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Forecast data path does not exist: %s", path), call. = FALSE)
  }
  df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                 colClasses = c(location = "character"))
  clean_nan_values(df)
}

#' Title
#'
#' @param path
#'
#' @importsFrom arrow read_parquet
#'
#'
load_target_data <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Target data path does not exist: %s", path), call. = FALSE)
  }
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("parquet", "pq")) {
    require_package("arrow")
    df <- arrow::read_parquet(path)
    df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
    if ("location" %in% names(df)) {
      df$location <- as.character(df$location)
    }
  } else if (ext %in% c("csv")) {
    df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                   colClasses = c(location = "character"))
  } else {
    stop(sprintf("Unsupported target data file extension '%s'. Expected csv or parquet.", ext), call. = FALSE)
  }
  return(clean_nan_values(df))
}

load_locations_data <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Locations data path does not exist: %s", path), call. = FALSE)
  }
  df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                 colClasses = c(location = "character"))
  clean_nan_values(df)
}

#' Title
#'
#' @param forecast_df
#' @param locations_df
#' @param forecast_path
#' @param locations_path
#'
#'
validate_location_coverage <- function(forecast_df, locations_df, forecast_path, locations_path) {
  forecast_locations <- unique_in_order(as.character(forecast_df$location))
  known_locations <- unique_in_order(as.character(locations_df$location))
  missing <- setdiff(forecast_locations, known_locations)
  if (length(missing) > 0) {
    stop(sprintf(
      "Locations present in forecast data but missing from location metadata: %s. Forecast file: %s, locations file: %s",
      paste(missing, collapse = ", "),
      forecast_path,
      locations_path
    ), call. = FALSE)
  }
}

#' Title
#'
#' @param locations_df
#' @param location
#'
#'
get_location_row <- function(locations_df, location) {
  row <- locations_df[locations_df$location == location, , drop = FALSE]
  if (nrow(row) == 0) {
    stop(sprintf("Could not find location '%s' in location metadata.", location), call. = FALSE)
  }
  return(row[1, , drop = FALSE])
}








#' Title
#'
#' @param target_data
#' @param location
#' @param config
#'
#' @returns
#'
#' @examples
#' \dontrun{
#' ground_truth_df <- prepare_ground_truth_df(target_data, location, config)
#' ground_truth <- format_ground_truth_output(ground_truth_df, config)
#' }
#'
prepare_ground_truth_df <- function(target_data, location, config) {
  df <- target_data[target_data$location == location, , drop = FALSE]
  if (nrow(df) == 0) {
    return(df)
  }
  date_col <- config$ground_truth_date_column
  if (!date_col %in% names(df)) {
    return(data.frame())
  }
  df$as_of <- as.character(df$as_of)
  df[[date_col]] <- as.Date(df[[date_col]])
  df <- df[!is.na(df[[date_col]]), , drop = FALSE]

  df <- df[order(df$as_of), , drop = FALSE]
  df <- df[!duplicated(df[, c(date_col, "target")], fromLast = TRUE), , drop = FALSE]
  if (!is.null(config$ground_truth_min_date)) {
    df <- df[df[[date_col]] >= config$ground_truth_min_date, , drop = FALSE]
  }
  df <- df[order(df[[date_col]]), , drop = FALSE]
  df
}





#' Title
#'
#' @param ground_truth_df  Data.frame. Ground truth data for the location
#' @param config
#'
#' @returns
#' @export
#'
#' @examples
format_ground_truth_output <- function(ground_truth_df, config) {
  if (nrow(ground_truth_df) == 0) {
    return(list(dates = list()))
  }
  date_col <- config$ground_truth_date_column
  df_to_pivot <- ground_truth_df[, c(date_col, "target", "observation")]
  pivot_df <- reshape(
    df_to_pivot,
    idvar = date_col,
    timevar = "target",
    direction = "wide"
  )
  names(pivot_df) <- sub("observation\\.", "", names(pivot_df))
  pivot_df <- pivot_df[order(pivot_df[[date_col]]), , drop = FALSE]
  result <- list(
    dates = format(pivot_df[[date_col]], "%Y-%m-%d")
  )
  target_cols <- names(pivot_df)[names(pivot_df) != date_col]
  for (target_name in target_cols) {
    result[[target_name]] <- unname(pivot_df[[target_name]])
  }

  return(result)
}












#' Title
#'
#' @param df Data.frame. HubVerse formatted data of forecasts
#' @param locations_df Location metadata data frame
#' @param config Configuration file for the dataset
#'
#'
#' @returns
#' @export
#'
#' @examples
build_metadata_key <- function(df, locations_df, config) {
  location <- as.character(df$location[[1]])
  loc_row <- get_location_row(locations_df, location)
  population_value <- loc_row$population[[1]]
  if (is.null(population_value) || is.na(population_value)) {
    population_value <- NA_real_
  } else {
    population_value <- as.numeric(population_value)
  }
  list(
    location = location,
    abbreviation = as.character(loc_row$abbreviation[[1]]),
    location_name = as.character(loc_row$location_name[[1]]),
    population = population_value,
    dataset = config$dataset_label,
    series_type = config$series_type,
    hubverse_keys = list(
      models = unname(unique_in_order(as.character(df$model_id))),
      targets = unname(unique_in_order(as.character(df$target))),
      horizons = unname(unique_in_order(as.character(df$horizon))),
      output_types = unname(unique_in_order(as.character(df$output_type)))
    )
  )
}

#' Title
#'
#' @param df Data.frame. HubVerse formatted data of forecasts
#' @param config Configuration list
#'
#'
#' @returns
#' @export
#'
#' @examples
build_forecasts_key <- function(df, config) {
  df$reference_date <- as.character(df$reference_date)
  df$target <- as.character(df$target)
  df$model_id <- as.character(df$model_id)
  df$output_type <- as.character(df$output_type)
  df$horizon <- as.character(df$horizon)
  df$target_end_date <- as.character(df$target_end_date)

  result <- list()
  reference_dates <- unique_in_order(df$reference_date)
  for (ref_date in reference_dates) {
    ref_df <- df[df$reference_date == ref_date, , drop = FALSE]
    targets <- unique_in_order(ref_df$target)
    reference_entry <- list()
    for (target in targets) {
      target_df <- ref_df[ref_df$target == target, , drop = FALSE]
      models <- unique_in_order(target_df$model_id)
      target_entry <- list()
      for (model in models) {
        model_df <- target_df[target_df$model_id == model, , drop = FALSE]
        output_types <- unique_in_order(model_df$output_type)
        model_entry <- NULL
        for (output_type in output_types) {
          if (output_type %in% config$drop_output_types) {
            next
          }
          output_df <- model_df[model_df$output_type == output_type, , drop = FALSE]
          horizons <- unique_in_order(output_df$horizon)
          predictions <- list()
          for (hz in horizons) {
            hz_df <- output_df[output_df$horizon == hz, , drop = FALSE]
            if (nrow(hz_df) == 0) {
              next
            }
            entry <- list(
              date = as.character(hz_df$target_end_date[[1]])
            )
            if (output_type == "quantile") {
              entry$quantiles <- unname(as.numeric(hz_df$output_type_id))
              entry$values <- unname(as.numeric(hz_df$value))
            } else if (output_type == "pmf") {
              entry$categories <- unname(as.character(hz_df$output_type_id))
              entry$probabilities <- unname(as.numeric(hz_df$value))
            } else {
              stop(sprintf("Unsupported output_type '%s'.", output_type), call. = FALSE)
            }
            predictions[[as.character(hz)]] <- entry
          }
          if (length(predictions) > 0) {
            model_entry <- list(
              type = output_type,
              predictions = predictions
            )
            target_entry[[model]] <- model_entry
          }
        }
      }
      if (length(target_entry) > 0) {
        reference_entry[[target]] <- target_entry
      }
    }
    if (length(reference_entry) > 0) {
      result[[ref_date]] <- reference_entry
    }
  }
  return(result)
}

#' Build metadata file
#'
#' @param data_df Forecast data frame
#' @param locations_df  Location metadata data frame
#'
#' @returns
#'
#' @examples
build_metadata_file <- function(data_df, locations_df) {
  location_entries <- lapply(seq_len(nrow(locations_df)), function(i) {
    row <- locations_df[i, , drop = FALSE]
    population_value <- row$population[[1]]
    if (is.null(population_value) || is.na(population_value)) {
      population_value <- NA_real_
    } else {
      population_value <- as.numeric(population_value)
    }
    list(
      location = as.character(row$location[[1]]),
      abbreviation = as.character(row$abbreviation[[1]]),
      location_name = as.character(row$location_name[[1]]),
      population = population_value
    )
  })

  list(
    last_updated = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    models = unname(unique_in_order(as.character(data_df$model_id))),
    locations = location_entries
  )
}

#' Validate JSON payload against schema
#'
#' @param payload JSON payload to validate
#' @param schema_path Path to JSON schema file
#'
#' @importsFrom jsonvalidate json_validate
#' @importsFrom jsonlite toJSON
#'
#' @returns invisible TRUE if valid, otherwise stops with error
#'
#' @examples
#' \dontrun{
#' validate_payload(item$payload, schema_path)
#' }
#'
validate_payload <- function(payload, schema_path) {
  if (!requireNamespace("jsonvalidate", quietly = TRUE)) {
    warning("Package 'jsonvalidate' not installed; skipping schema validation.")
    return(invisible(TRUE))
  }
  json_text <- jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = FALSE, na = "null")
  is_valid <- jsonvalidate::json_validate(json_text, schema = schema_path, engine = "ajv", verbose = TRUE)
  if (!isTRUE(is_valid)) {
    stop("Schema validation failed for generated payload.", call. = FALSE)
  }
}
write.table(json_text, "tmp.txt")


#' Save JSON payload to file
#'
#' @param payload JSON payload to validate
#' @param path
#' @param overwrite Logical. Whether to overwrite existing files.
#'
#' @importsFrom jsonlite write_json
#'
#' @returns
#'
#' @examples
save_json_payload <- function(payload, path, overwrite) {
  if (file.exists(path) && !overwrite) {
    stop(sprintf("File already exists at %s. Re-run with --overwrite to replace it.", path), call. = FALSE)
  }
  jsonlite::write_json(payload, path, auto_unbox = TRUE, pretty = TRUE, na = "null")
}

#' Process Hubverse forecast export into RespiLens projection JSON files
#'
#' @param data_df Forecast data frame
#' @param locations_df Location metadata data frame
#' @param target_df Target data frame
#' @param config Configuration file for the dataset
#'
#'
#' @returns
#' @export
#'
#' @examples
process_dataset <- function(data_df, locations_df, target_df, config) {
  outputs <- list()
  locations <- unique_in_order(as.character(data_df$location))
  for (location in locations) {
    loc_df <- data_df[data_df$location == location, , drop = FALSE]
    ground_truth_df <- prepare_ground_truth_df(target_df, location, config)
    metadata <- build_metadata_key(loc_df, locations_df, config)
    ground_truth <- format_ground_truth_output(ground_truth_df, config)
    forecasts <- build_forecasts_key(loc_df, config)

    loc_row <- get_location_row(locations_df, location)
    abbreviation <- as.character(loc_row$abbreviation[[1]])
    file_name <- sprintf("%s_%s.json", abbreviation, config$file_suffix)

    outputs[[length(outputs) + 1]] <- list(
      file_name = file_name,
      payload = list(
        metadata = metadata,
        ground_truth = ground_truth,
        forecasts = forecasts
      )
    )
  }
  outputs
}





copy_schema_from_github <- function(dest_dir) {

  dest_dir <- file.path(dest_dir, "schemas")
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Download the file from GitHub
  schema_github_url <- "https://github.com/ACCIDDA/RespiLens-staging/blob/main/scripts/schemas/RespiLens_projections.schema.json"
  download.file(schema_github_url,
                destfile = file.path(dest_dir, "RespiLens_projections.schema.json"),
                mode = "wb")
}








#' Title
#'
#' @returns
#' @export
#'
#' @importsFrom jsonlite toJSON write_json
#'
#'
#' @examples
#' \dontrun{
#' # Assuming forecast_df, locations_df, target_df, and config are defined
#' outputs <- process_dataset(forecast_df, locations_df, target_df, config)
#' for (item in outputs) {
#'  validate_payload(item$payload, schema_path)
main <- function() {
  args <- parse_cli_args()
  context <- list(args = args)

  dataset_configs <- list(
    flu = list(
      file_suffix = "flu",
      dataset_label = "flusight forecasts",
      ground_truth_value_key = "wk inc flu hosp",
      ground_truth_date_column = "target_end_date",
      ground_truth_target = NULL,
      ground_truth_min_date = as.Date("2023-10-01"),
      series_type = "projection",
      drop_output_types = c("sample")
    ),
    rsvforecasthub = list(
      file_suffix = "rsv",
      dataset_label = "rsv forecast hub",
      ground_truth_value_key = "wk inc rsv hosp",
      ground_truth_date_column = "date",
      ground_truth_target = "wk inc rsv hosp",
      ground_truth_min_date = as.Date("2023-10-01"),
      series_type = "projection",
      drop_output_types = c("sample")
    ),
    covid19forecasthub = list(
      file_suffix = "covid19",
      dataset_label = "covid19 forecast hub",
      ground_truth_value_key = "wk inc covid hosp",
      ground_truth_date_column = "date",
      ground_truth_target = "wk inc covid hosp",
      ground_truth_min_date = as.Date("2023-10-01"),
      series_type = "projection",
      drop_output_types = c("sample")
    )
  )

  pathogen_aliases <- c(rsv = "rsvforecasthub", covid = "covid19forecasthub", covid19 = "covid19forecasthub")
  canonical_pathogen <- if (args$pathogen %in% names(dataset_configs)) args$pathogen else pathogen_aliases[[args$pathogen]]
  if (is.na(canonical_pathogen)) {
    stop(sprintf("Unsupported pathogen '%s'. Supported options are: %s",
                 args$pathogen,
                 paste(names(dataset_configs), collapse = ", ")), call. = FALSE)
  }

  config <- dataset_configs[[canonical_pathogen]]
  log_message("INFO", sprintf("Starting %s projections processing...", toupper(canonical_pathogen)), context)

  forecast_required_columns <- c(
    "location", "reference_date", "target", "model_id", "horizon",
    "output_type", "output_type_id", "value", "target_end_date"
  )
  target_required_columns <- list(
    flu = c("as_of", "target_end_date", "location", "observation", "target"),
    rsvforecasthub = c("as_of", "date", "location", "observation", "target"),
    covid19forecasthub = c("as_of", "date", "location", "observation", "target")
  )
  location_required_columns <- c("location", "abbreviation", "location_name", "population")

  forecast_df <- load_forecast_data(args$data_path)
  check_required_columns(forecast_df, forecast_required_columns, args$data_path)
  # remove unneeded columns
  forecast_df <- forecast_df %>%
    dplyr::select(dplyr::all_of(forecast_required_columns))
  forecast_df <- hubverse_df_preprocessor(forecast_df)


  target_df <- load_target_data(args$target_data_path)
  if (("issue_date" %in% colnames(target_df) & !("as_of" %in% colnames(target_df)))){
    target_df$as_of <- target_df$issue_date
  }
  check_required_columns(target_df, target_required_columns[[canonical_pathogen]], args$target_data_path)
  # remove unneeded columns
  target_df <- target_df %>%
    dplyr::select(dplyr::all_of(target_required_columns[[canonical_pathogen]])) %>%
    dplyr::filter(!is.na(observation))

  locations_df <- load_locations_data(args$locations_data_path)
  check_required_columns(locations_df, location_required_columns, args$locations_data_path)
  # remove unneeded columns
  locations_df <- locations_df %>%
    dplyr::select(dplyr::all_of(location_required_columns))


  forecast_df$location <- as.character(forecast_df$location)
  target_df$location <- as.character(target_df$location)
  locations_df$location <- as.character(locations_df$location)
  locations_df$abbreviation <- as.character(locations_df$abbreviation)
  locations_df$location_name <- as.character(locations_df$location_name)

  forecast_df$value <- as.numeric(forecast_df$value)
  target_df$observation <- as.numeric(target_df$observation)

  validate_location_coverage(forecast_df, locations_df, args$data_path, args$locations_data_path)


  # Get RespiLens Schema

  #respilens_dir <- get_script_dir()
  respilens_dir <- args$output_path
  schema_path <- file.path(respilens_dir, "schemas", "RespiLens_projections.schema.json")
  if (!file.exists(schema_path)) {
    message(sprintf("Could not find schema file at %s.\n\nCopying from RespiLens GitHub.", schema_path))
    copy_schema_from_github(respilens_dir)
  }

  outputs <- process_dataset(forecast_df, locations_df, target_df, config)
  path_mapping <- list(flu = "flusight", rsvforecasthub = "rsvforecasthub", covid19forecasthub = "covid19forecasthub")
  target_subdir <- path_mapping[[canonical_pathogen]]
  output_dir <- file.path(args$output_path, target_subdir)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  #describe whats in outputs



  for (item in outputs) {
    validate_payload(payload = item$payload, schema_path = schema_path)
    target_path <- file.path(output_dir, item$file_name)
    save_json_payload(item$payload, target_path, args$overwrite)
  }
  log_message("INFO", sprintf("Successfully saved JSON output."), context)

  metadata_payload <- build_metadata_file(forecast_df, locations_df)
  metadata_path <- file.path(output_dir, "metadata.json")
  save_json_payload(metadata_payload, metadata_path, TRUE)
  log_message("INFO", sprintf(paste0("Successfully saved RespiLens formatted metadata at ", metadata_path)), context)
  log_message("INFO", "Processing complete.", context)
  invisible(0)
}

#' Title
#'
#' @returns
#' @export
#'
#' @examples
run_main <- function() {
  tryCatch(
    {
      status <- main()
      quit(save = "no", status = status, runLast = FALSE)
    },
    error = function(e) {
      message(sprintf("ERROR: %s", e$message))
      quit(save = "no", status = 1, runLast = FALSE)
    }
  )
}

# if (!interactive()) {
#   run_main()
# }
