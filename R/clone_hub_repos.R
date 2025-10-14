#' Clone github forecast repo
#'
#' Clones influenza, covid, or rsv forecast hub repos to use for learning, visualization, etc.
#' Clones to the current working directory.
#'
#' @param disease Character. One of "influenza", "covid", or "rsv". Default is "influenza".
#' @param old_rsv_repo Logical. If TRUE, uses the old RSV forecast hub repository. Default is TRUE. If FALSE, uses the new CDC RSV forecast hub repository.
#'  This parameter is only relevant if disease is "rsv". The RSV forecast hub moved from Hopkins to CDC starting in the 2025-26 season.
#' @param clone_dir Character. Directory to clone the repository into. Default is the current working directory.
#'
#' @returns Character. The path to the cloned repository.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Clone the influenza forecast hub repository
#' flu_repo <- clone_hub_repos(disease = "influenza")
#' # Clone the COVID-19 forecast hub repository
#' covid_repo <- clone_hub_repos(disease = "covid")
#' # Clone the old RSV forecast hub repository
#' old_rsv_repo <- clone_hub_repos(disease = "rsv", old_rsv_repo = TRUE)
#' # Clone the new CDC RSV forecast hub repository
#' new_rsv_repo <- clone_hub_repos(disease = "rsv", old_rsv_repo = FALSE)
#' }
#'
clone_hub_repos <- function(disease = "influenza",
                            old_rsv_repo = TRUE,
                            clone_dir = NULL) {

  disease <- tolower(disease)
  disease <- ifelse(disease == "flu", "influenza",
                    ifelse(disease == "covid19", "covid", disease))

  repos <- list(
    influenza = "https://github.com/cdcepi/FluSight-forecast-hub.git",
    covid = "https://github.com/CDCgov/covid19-forecast-hub.git",
    rsv = c("https://github.com/HopkinsIDD/rsv-forecast-hub.git", "https://github.com/CDCgov/rsv-forecast-hub.git"))

  repo_dirs <- list(
    influenza = "FluSight-forecast-hub",
    covid = "COVID19-forecast-hub",
    rsv = c("RSV-forecast-hub-old", "rsv-forecast-hub")
  )

  repo_path <- repos[[disease]]
  repo_dir <- repo_dirs[[disease]]

  if (disease == "rsv" & old_rsv_repo) {
    repo_path <- repo_path[1]
    repo_dir <- repo_dir[1]
  } else if (disease == "rsv" & !old_rsv_repo) {
    repo_path <- repo_path[2]
    repo_dir <- repo_dir[2]
  }

  if (!(is.null(clone_dir) | is.na(clone_dir) | clone_dir == "")) {
    repo_dir <- file.path(clone_dir, repo_dir)
  }

  # Ensure Git is available
  has_git <- tryCatch(
    system2("git", "--version", stdout = TRUE, stderr = TRUE),
    error = function(e) NA
  )
  if (any(is.na(has_git))) stop("Git does not appear to be installed or on PATH.")


  # Clone into working directory if not present
  if (!dir.exists(repo_dir)) {
    message("Cloning repository...")
    status <- system2("git", c("clone", repo_path),
                      stdout = TRUE, stderr = TRUE)
    cat(paste(status, collapse = "\n"), "\n")
  } else {
    message("Repository exists. Pulling latest changes...")

    # Check it's actually a git repo
    is_repo <- tryCatch(
      system2("git", c("-C", paste0('\"',repo_dir,'\"'), "rev-parse", "--is-inside-work-tree"),
              stdout = TRUE, stderr = TRUE),
      error = function(e) "false"
    )

    if (identical(trimws(is_repo), "true")) {
      # Warn if there are local changes
      dirty <- trimws(paste(system2("git", c("-C", paste0('\"',repo_dir,'\"'), "status", "--porcelain"),
                                    stdout = TRUE), collapse = "\n"))
      if (nchar(dirty) > 0) {
        message("⚠️ Local changes detected in ", repo_dir,
                ". Pulling with --ff-only (won't overwrite local work).")
      }

      # Fetch and pull (fast-forward only)
      fetch_out <- system2("git", c("-C", paste0('\"',repo_dir,'\"'), "fetch", "--prune"),
                           stdout = TRUE, stderr = TRUE)
      pull_out  <- system2("git", c("-C", paste0('\"',repo_dir,'\"'), "pull", "--ff-only", "--quiet"),
                           stdout = TRUE, stderr = TRUE)

      # Show any messages from Git
      if (length(fetch_out)) cat(paste(fetch_out, collapse = "\n"), "\n")
      if (length(pull_out))  cat(paste(pull_out,  collapse = "\n"), "\n")
      message("Pull complete.")
    } else {
      stop(sprintf("Path '%s' exists but is not a Git repository.", repo_dir))
    }
  }

  # Normalize path for downstream code
  repo_dir <- normalizePath(repo_dir)
  message("Using repo_dir: ", repo_dir)

  return(repo_dir)
}




#' Copy Forecast Hub Outputs to model-output
#'
#' @param repo_dir Path to the cloned forecast hub repository.
#' @param forecast_date Forecast date in "YYYY-MM-DD" format. Can be a vector of dates.
#' @param models_to_copy Character vector. Names of models to copy outputs for.
#'
#' @importFrom jsonlite read_json
#' @importFrom lubridate is.Date
#'
#' @details
#' This function copies forecast output files from a specified forecasting hub repository
#' to a local "model-output" directory for a given forecast date. It allows users
#' to specify which models' outputs to copy.
#'
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming you have cloned the influenza forecast hub repository
#' flu_repo <- clone_hub_repos(disease = "influenza")
#' # Copy forecast outputs for a specific date
#' copy_fch_outputs(repo_dir = flu_repo, forecast_date = "2023-10-02",
#'                  models_to_copy = c("FluSight-baseline", "MOBS-GLEAM_FLUH", "FluSight-ensemble"))
#' }
#'
copy_fch_outputs <- function(repo_dir,
                             forecast_date,
                             models_to_copy = c("FluSight-baseline",
                                                "MOBS-GLEAM_FLUH",
                                                "FluSight-ensemble")) {

  dir_path <- normalizePath(repo_dir)

  if (!dir.exists(dir_path)) {
    stop(paste0(dir_path, " does not exist."))
  }

  forecast_date <- as.Date(forecast_date)
  if (!lubridate::is.Date(forecast_date) || length(forecast_date) < 1 ||
      is.na(as.Date(forecast_date, format = "%Y-%m-%d"))) {
    stop("forecast_date must be a string in 'YYYY-MM-DD' format.")
  }

  if (!is.character(models_to_copy) || length(models_to_copy) <= 1) {
    stop("models_to_copy must be a character vector of model names.")
  }

  # Read submission (reference) dates from hub-config/tasks.json
  tasks_path <- file.path(dir_path, "hub-config", "tasks.json")
  tasks <- jsonlite::read_json(tasks_path)

  # Extract reference dates from the first round / first model task
  dates_archive <- unlist(tasks$rounds[[1]]$model_tasks[[1]]$task_ids$reference_date$optional)
  dates_archive <- as.Date(dates_archive)
  dates_archive <- dates_archive[dates_archive <= Sys.Date()]

  # Set reference date of interest
  curr_origin_date <- forecast_date

  if (!any(curr_origin_date %in% dates_archive)) {
    stop("Chosen curr_origin_date is not a valid submission date in the Hub.")
  }

  # Copy files for the chosen date into model-output/
  dest_dir <- normalizePath("model-output")

  files_to_copy <- sapply(curr_origin_date, function(x) {
    list.files(
      file.path(repo_dir, "model-output"),
      pattern = paste0("^", format(x, "%Y-%m-%d"), "-.*\\.(csv|parquet)$"),
      full.names = TRUE, recursive = TRUE)
  })
  files_to_copy <- unlist(files_to_copy)

  # copy files for chosen models
  sapply(models_to_copy,
         function(x){
           dir.create(file.path("model-output", x), showWarnings = FALSE, recursive = TRUE)
           files_ <- files_to_copy[grepl(paste0("/",x,"/"), files_to_copy)]
           file.copy(
             from = files_,
             to = file.path(dest_dir, x, basename(files_)),
             overwrite = TRUE)
         })
  message("Copied files for date ", paste(curr_origin_date, collapse = ", "), " to ", dest_dir)
  invisible(NULL)
}







