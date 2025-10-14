#' Get the hubVerse reference date.
#'
#' Return the date of the **Saturday that follows** the forecast date.
#' The `reference_date` is defined for forecast hubs as the Saturday Following the forecast date.
#'
#' @param forecast_date A vector of dates to process. Accepts objects coercible to `Date`:
#'   `Date`, `POSIXct`, or character strings in unambiguous ISO-like formats
#'   (e.g., `"2025-10-13"`). See Details for time-zone notes with `POSIXct`.
#'
#' @return A `Date` vector of the same length as `forecast_date`, giving the Saturday on or
#'   after `forecast_date`. `NA` inputs yield `NA` outputs.
#'
#' The function is fully vectorized.
#'
#' @examples
#' # Basic usage
#' get_reference_date("2025-10-13")
#' #> "2025-10-18"
#'
#' # Input is Saturday: strictly after by default
#' get_reference_date(as.Date("2025-10-18"))
#' #> "2025-10-25"
#'
#' # Allow returning the same day if it is Saturday
#' get_reference_date("2025-10-18")
#' #> "2025-10-18"
#'
#'
#' @seealso
#' \code{\link[lubridate]{wday}}, \code{\link[lubridate]{as_date}}
#'
#' @export
#'
get_reference_date <- function(forecast_date) {
  d <- as.Date(forecast_date)                         # For POSIXct, uses system tz unless specified
  dow <- as.integer(format(d, "%w"))      # 0 = Sun, 1 = Mon, ..., 6 = Sat
  ahead <- (6 - dow) %% 7                 # days until Saturday (0..6)
  d + ahead
}










#' Read model file helper
#'
#' helper that picks the right reader
#' supports .parquet, .parquet.gz, .csv, .csv.gz
#'
#' @param path character(1) path to file
#'
#' @importFrom arrow read_parquet
#' @importFrom readr read_csv
#'
#' @returns NULL
#' @export
#'
#' @examples
#' \dontrun{
#' read_model_file("path/to/file.parquet")
#' read_model_file("path/to/file.csv.gz")
#' }
#'
read_model_file <- function(path) {
  if (grepl("\\.parquet(\\.gz)?$", path, ignore.case = TRUE)) {
    # use arrow for parquet (handles both .parquet and .parquet.gz)
    arrow::read_parquet(path)
  } else if (grepl("\\.csv(\\.gz)?$", path, ignore.case = TRUE)) {
    readr::read_csv(path, show_col_types = FALSE)
  } else {
    stop("Unrecognized file type: ", path)
  }
}


