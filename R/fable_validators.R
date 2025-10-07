#' Validate Data for Fable Model Input
#'
#' Comprehensive validation function that checks if data is suitable for fable modeling.
#' This function performs multiple checks including structure, date format, missing values,
#' and temporal regularity.
#'
#' @param data Data frame to validate
#' @param date_col Character. Name of the date/time column. Default is "date".
#' @param value_cols Character vector. Names of numeric value columns to check. Default is "value".
#' @param key_cols Character vector. Names of key columns (grouping variables). Default is "location".
#' @param stop_on_error Logical. If TRUE, stops with error on validation failure. If FALSE, returns validation results. Default is TRUE.
#' @param verbose Logical. If TRUE, prints detailed validation messages. Default is TRUE.
#'
#' @return If stop_on_error is FALSE, returns a list with validation results. Otherwise returns TRUE invisibly on success.
#' @export
#'
#' @examples
#' \dontrun{
#' hub_data <- data.frame(
#'   date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 10),
#'   location = "US",
#'   value = rnorm(10, 100, 10)
#' )
#' validate_fable_input_data(hub_data)
#' }
validate_fable_input_data <- function(data,
                                      date_col = "date",
                                      value_cols = "value",
                                      key_cols = "location",
                                      stop_on_error = TRUE,
                                      verbose = TRUE) {
  
  validation_results <- list(
    valid = TRUE,
    errors = character(0),
    warnings = character(0),
    checks_passed = character(0)
  )
  
  # Helper function to add error
  add_error <- function(msg) {
    validation_results$valid <<- FALSE
    validation_results$errors <<- c(validation_results$errors, msg)
    if (verbose) message("ERROR: ", msg)
  }
  
  # Helper function to add warning
  add_warning <- function(msg) {
    validation_results$warnings <<- c(validation_results$warnings, msg)
    if (verbose) message("WARNING: ", msg)
  }
  
  # Helper function to add success
  add_success <- function(msg) {
    validation_results$checks_passed <<- c(validation_results$checks_passed, msg)
    if (verbose) message("OK: ", msg)
  }
  
  # Check 1: Data is a data frame
  if (!is.data.frame(data)) {
    add_error("Input must be a data frame")
    if (stop_on_error) stop("Validation failed: Input must be a data frame")
    return(validation_results)
  }
  add_success("Input is a data frame")
  
  # Check 2: Data is not empty
  if (nrow(data) == 0) {
    add_error("Data frame is empty (0 rows)")
    if (stop_on_error) stop("Validation failed: Data frame is empty")
    return(validation_results)
  }
  add_success(paste("Data frame has", nrow(data), "rows"))
  
  # Check 3: Required columns exist
  all_required_cols <- c(date_col, value_cols, key_cols)
  missing_cols <- setdiff(all_required_cols, names(data))
  if (length(missing_cols) > 0) {
    add_error(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
    if (stop_on_error) stop("Validation failed: Missing required columns: ", paste(missing_cols, collapse = ", "))
    return(validation_results)
  }
  add_success("All required columns present")
  
  # Check 4: Validate date column
  date_check <- check_date_column(data, date_col, stop_on_error = FALSE)
  if (!date_check$valid) {
    for (err in date_check$errors) add_error(err)
    for (warn in date_check$warnings) add_warning(warn)
    if (stop_on_error) stop("Validation failed: Date column issues")
    return(validation_results)
  }
  add_success("Date column is valid")
  
  # Check 5: Validate numeric columns
  numeric_check <- check_numeric_columns(data, value_cols, stop_on_error = FALSE)
  if (!numeric_check$valid) {
    for (err in numeric_check$errors) add_error(err)
    for (warn in numeric_check$warnings) add_warning(warn)
    if (stop_on_error) stop("Validation failed: Numeric column issues")
    return(validation_results)
  }
  add_success("Numeric columns are valid")
  
  # Check 6: Check for missing values
  missing_check <- check_missing_values(data, c(date_col, value_cols, key_cols), stop_on_error = FALSE)
  if (!missing_check$valid) {
    for (err in missing_check$errors) add_error(err)
    for (warn in missing_check$warnings) add_warning(warn)
  }
  if (missing_check$valid) {
    add_success("No problematic missing values detected")
  }
  
  # Check 7: Validate tsibble requirements
  tsibble_check <- validate_tsibble_requirements(data, date_col, key_cols, stop_on_error = FALSE)
  if (!tsibble_check$valid) {
    for (err in tsibble_check$errors) add_error(err)
    for (warn in tsibble_check$warnings) add_warning(warn)
  }
  if (tsibble_check$valid) {
    add_success("Tsibble requirements met")
  }
  
  # Check 8: Check temporal regularity for each key
  regularity_check <- check_temporal_regularity(data, date_col, key_cols, stop_on_error = FALSE)
  if (!regularity_check$valid) {
    for (err in regularity_check$errors) add_error(err)
    for (warn in regularity_check$warnings) add_warning(warn)
  } else {
    add_success("Temporal regularity check passed")
  }
  
  # Final result
  if (validation_results$valid) {
    if (verbose) message("\n=== All validation checks passed ===")
    if (stop_on_error) {
      return(invisible(TRUE))
    }
  } else {
    if (verbose) {
      message("\n=== Validation failed ===")
      message(paste("Total errors:", length(validation_results$errors)))
      message(paste("Total warnings:", length(validation_results$warnings)))
    }
    if (stop_on_error) {
      stop("Data validation failed. See errors above.")
    }
  }
  
  return(validation_results)
}


#' Check Date Column Format
#'
#' Validates that the date column is in the correct format for fable/tsibble.
#'
#' @param data Data frame to check
#' @param date_col Character. Name of the date column. Default is "date".
#' @param stop_on_error Logical. If TRUE, stops on error. Default is TRUE.
#'
#' @return List with validation results
#' @export
#'
#' @examples
#' \dontrun{
#' hub_data <- data.frame(
#'   date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 10),
#'   value = rnorm(10)
#' )
#' check_date_column(hub_data)
#' }
check_date_column <- function(data, date_col = "date", stop_on_error = TRUE) {
  
  result <- list(
    valid = TRUE,
    errors = character(0),
    warnings = character(0)
  )
  
  # Check column exists
  if (!date_col %in% names(data)) {
    result$valid <- FALSE
    result$errors <- c(result$errors, paste("Date column", date_col, "not found in data"))
    if (stop_on_error) stop("Date column ", date_col, " not found in data")
    return(result)
  }
  
  date_values <- data[[date_col]]
  
  # Check for NA values
  if (any(is.na(date_values))) {
    na_count <- sum(is.na(date_values))
    result$valid <- FALSE
    result$errors <- c(result$errors, paste("Date column contains", na_count, "NA values"))
    if (stop_on_error) stop("Date column contains ", na_count, " NA values")
  }
  
  # Check if it's a Date, POSIXct, or convertible to Date
  if (!inherits(date_values, c("Date", "POSIXct", "POSIXlt"))) {
    # Try to convert to Date
    tryCatch({
      test_conversion <- lubridate::as_date(date_values)
      if (any(is.na(test_conversion[!is.na(date_values)]))) {
        result$valid <- FALSE
        result$errors <- c(result$errors, "Date column cannot be converted to Date type")
      } else {
        result$warnings <- c(result$warnings, "Date column is not Date type but can be converted")
      }
    }, error = function(e) {
      result$valid <<- FALSE
      result$errors <<- c(result$errors, paste("Date column conversion failed:", e$message))
      if (stop_on_error) stop("Date column conversion failed: ", e$message)
    })
  }
  
  # Check for duplicates (within keys, this will be checked separately)
  if (length(unique(date_values)) < length(date_values)) {
    result$warnings <- c(result$warnings, "Date column has duplicate values (may be valid if multiple keys exist)")
  }
  
  if (result$valid && stop_on_error) {
    return(invisible(TRUE))
  }
  
  return(result)
}


#' Check Numeric Columns
#'
#' Validates that value columns are numeric and suitable for modeling.
#'
#' @param data Data frame to check
#' @param value_cols Character vector. Names of numeric columns to check. Default is "value".
#' @param stop_on_error Logical. If TRUE, stops on error. Default is TRUE.
#'
#' @return List with validation results
#' @export
#'
#' @examples
#' \dontrun{
#' hub_data <- data.frame(
#'   date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 10),
#'   value = rnorm(10)
#' )
#' check_numeric_columns(hub_data)
#' }
check_numeric_columns <- function(data, value_cols = "value", stop_on_error = TRUE) {
  
  result <- list(
    valid = TRUE,
    errors = character(0),
    warnings = character(0)
  )
  
  for (col in value_cols) {
    # Check column exists
    if (!col %in% names(data)) {
      result$valid <- FALSE
      result$errors <- c(result$errors, paste("Value column", col, "not found in data"))
      next
    }
    
    values <- data[[col]]
    
    # Check if numeric
    if (!is.numeric(values)) {
      result$valid <- FALSE
      result$errors <- c(result$errors, paste("Column", col, "is not numeric (type:", class(values)[1], ")"))
      next
    }
    
    # Check for infinite values
    if (any(is.infinite(values))) {
      inf_count <- sum(is.infinite(values))
      result$valid <- FALSE
      result$errors <- c(result$errors, paste("Column", col, "contains", inf_count, "infinite values"))
    }
    
    # Warn about negative values (may be valid depending on context)
    if (any(values < 0, na.rm = TRUE)) {
      neg_count <- sum(values < 0, na.rm = TRUE)
      result$warnings <- c(result$warnings, paste("Column", col, "contains", neg_count, "negative values"))
    }
    
    # Check for all zeros
    if (all(values == 0, na.rm = TRUE)) {
      result$warnings <- c(result$warnings, paste("Column", col, "contains only zeros"))
    }
    
    # Check variance
    if (length(values) > 1 && !all(is.na(values))) {
      if (stats::var(values, na.rm = TRUE) == 0) {
        result$warnings <- c(result$warnings, paste("Column", col, "has zero variance (all values are identical)"))
      }
    }
  }
  
  if (!result$valid && stop_on_error) {
    stop("Numeric column validation failed: ", paste(result$errors, collapse = "; "))
  }
  
  if (result$valid && stop_on_error) {
    return(invisible(TRUE))
  }
  
  return(result)
}


#' Check for Missing Values
#'
#' Checks for missing values in critical columns.
#'
#' @param data Data frame to check
#' @param columns Character vector. Columns to check for missing values.
#' @param stop_on_error Logical. If TRUE, stops on error. Default is TRUE.
#' @param allow_value_na Logical. If TRUE, allows NA in value columns. Default is FALSE.
#'
#' @return List with validation results
#' @export
#'
#' @examples
#' \dontrun{
#' hub_data <- data.frame(
#'   date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 10),
#'   location = "US",
#'   value = rnorm(10)
#' )
#' check_missing_values(hub_data, c("date", "location", "value"))
#' }
check_missing_values <- function(data, columns, stop_on_error = TRUE, allow_value_na = FALSE) {
  
  result <- list(
    valid = TRUE,
    errors = character(0),
    warnings = character(0)
  )
  
  for (col in columns) {
    if (!col %in% names(data)) {
      next  # Column existence checked elsewhere
    }
    
    na_count <- sum(is.na(data[[col]]))
    
    if (na_count > 0) {
      na_pct <- round(100 * na_count / nrow(data), 2)
      msg <- paste("Column", col, "has", na_count, "missing values (", na_pct, "%)")
      
      # For value columns, might be acceptable
      if (allow_value_na && grepl("value|count|case", col, ignore.case = TRUE)) {
        result$warnings <- c(result$warnings, msg)
      } else {
        result$valid <- FALSE
        result$errors <- c(result$errors, msg)
      }
    }
  }
  
  if (!result$valid && stop_on_error) {
    stop("Missing value check failed: ", paste(result$errors, collapse = "; "))
  }
  
  if (result$valid && stop_on_error) {
    return(invisible(TRUE))
  }
  
  return(result)
}


#' Validate Tsibble Requirements
#'
#' Checks if data meets tsibble-specific requirements for conversion.
#'
#' @param data Data frame to check
#' @param index_col Character. Name of the time index column. Default is "date".
#' @param key_cols Character vector. Names of key columns. Default is "location".
#' @param stop_on_error Logical. If TRUE, stops on error. Default is TRUE.
#'
#' @return List with validation results
#' @export
#'
#' @examples
#' \dontrun{
#' hub_data <- data.frame(
#'   date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 10),
#'   location = "US",
#'   value = rnorm(10)
#' )
#' validate_tsibble_requirements(hub_data)
#' }
validate_tsibble_requirements <- function(data,
                                         index_col = "date",
                                         key_cols = "location",
                                         stop_on_error = TRUE) {
  
  result <- list(
    valid = TRUE,
    errors = character(0),
    warnings = character(0)
  )
  
  # Check for duplicate key-index combinations
  if (all(c(index_col, key_cols) %in% names(data))) {
    # Create a combined key
    if (length(key_cols) == 1) {
      key_index_combo <- paste(data[[index_col]], data[[key_cols]], sep = "_")
    } else {
      key_vals <- do.call(paste, c(data[key_cols], sep = "_"))
      key_index_combo <- paste(data[[index_col]], key_vals, sep = "_")
    }
    
    if (any(duplicated(key_index_combo))) {
      dup_count <- sum(duplicated(key_index_combo))
      result$valid <- FALSE
      result$errors <- c(result$errors, 
                        paste("Found", dup_count, "duplicate combinations of index and key variables.",
                              "Tsibble requires unique combinations."))
    }
  }
  
  # Check if index is ordered within each key
  if (all(c(index_col, key_cols) %in% names(data))) {
    # Split by keys and check ordering
    split_data <- split(data, data[key_cols])
    
    for (key_val in names(split_data)) {
      subset_data <- split_data[[key_val]]
      index_vals <- subset_data[[index_col]]
      
      # Convert to Date if not already
      if (!inherits(index_vals, c("Date", "POSIXct", "POSIXlt"))) {
        tryCatch({
          index_vals <- lubridate::as_date(index_vals)
        }, error = function(e) {
          # Skip ordering check if conversion fails
          return()
        })
      }
      
      if (!identical(index_vals, sort(index_vals))) {
        result$warnings <- c(result$warnings, 
                           paste("Index column is not sorted for key:", key_val))
      }
    }
  }
  
  if (!result$valid && stop_on_error) {
    stop("Tsibble requirement validation failed: ", paste(result$errors, collapse = "; "))
  }
  
  if (result$valid && stop_on_error) {
    return(invisible(TRUE))
  }
  
  return(result)
}


#' Check Temporal Regularity
#'
#' Checks if the time series has regular intervals (daily, weekly, monthly, etc.)
#' for each key variable combination.
#'
#' @param data Data frame to check
#' @param date_col Character. Name of the date column. Default is "date".
#' @param key_cols Character vector. Names of key columns. Default is "location".
#' @param stop_on_error Logical. If TRUE, stops on error. Default is TRUE.
#'
#' @return List with validation results including detected interval
#' @export
#'
#' @examples
#' \dontrun{
#' hub_data <- data.frame(
#'   date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 10),
#'   location = "US",
#'   value = rnorm(10)
#' )
#' check_temporal_regularity(hub_data)
#' }
check_temporal_regularity <- function(data,
                                     date_col = "date",
                                     key_cols = "location",
                                     stop_on_error = TRUE) {
  
  result <- list(
    valid = TRUE,
    errors = character(0),
    warnings = character(0),
    detected_intervals = list()
  )
  
  if (!all(c(date_col, key_cols) %in% names(data))) {
    result$valid <- FALSE
    result$errors <- c(result$errors, "Required columns not found")
    if (stop_on_error) stop("Required columns not found for temporal regularity check")
    return(result)
  }
  
  # Split by keys
  split_data <- split(data, data[key_cols])
  
  for (key_val in names(split_data)) {
    subset_data <- split_data[[key_val]]
    
    # Get and sort dates
    dates <- subset_data[[date_col]]
    
    # Convert to Date if needed
    if (!inherits(dates, c("Date", "POSIXct", "POSIXlt"))) {
      tryCatch({
        dates <- lubridate::as_date(dates)
      }, error = function(e) {
        result$valid <<- FALSE
        result$errors <<- c(result$errors, paste("Cannot convert dates for key:", key_val))
        return()
      })
    }
    
    # Remove NA dates
    dates <- dates[!is.na(dates)]
    
    if (length(dates) < 2) {
      result$warnings <- c(result$warnings, 
                          paste("Key", key_val, "has fewer than 2 observations"))
      next
    }
    
    # Sort dates
    dates <- sort(dates)
    
    # Calculate intervals
    intervals <- as.numeric(diff(dates))
    
    # Check if intervals are regular
    unique_intervals <- unique(intervals)
    
    if (length(unique_intervals) == 1) {
      # Perfect regularity
      interval_days <- unique_intervals[1]
      interval_type <- if (interval_days == 1) "daily"
      else if (interval_days == 7) "weekly"
      else if (interval_days %in% 28:31) "monthly"
      else if (interval_days %in% 365:366) "yearly"
      else paste(interval_days, "days")
      
      result$detected_intervals[[key_val]] <- list(
        regular = TRUE,
        interval_days = interval_days,
        interval_type = interval_type
      )
      
    } else if (length(unique_intervals) <= 3 && 
               (max(unique_intervals) - min(unique_intervals)) <= 3) {
      # Nearly regular (accounting for month/year variations)
      avg_interval <- mean(intervals)
      interval_type <- if (avg_interval < 2) "daily"
      else if (avg_interval >= 6 && avg_interval <= 8) "weekly"
      else if (avg_interval >= 28 && avg_interval <= 31) "monthly"
      else if (avg_interval >= 365 && avg_interval <= 366) "yearly"
      else paste(round(avg_interval), "days (approx)")
      
      result$detected_intervals[[key_val]] <- list(
        regular = TRUE,
        interval_days = avg_interval,
        interval_type = interval_type,
        note = "Nearly regular with minor variations"
      )
      
      result$warnings <- c(result$warnings,
                          paste("Key", key_val, "has nearly regular intervals with minor variations"))
      
    } else {
      # Irregular
      result$valid <- FALSE
      result$errors <- c(result$errors,
                        paste("Key", key_val, "has irregular time intervals.",
                              "Range:", min(intervals), "to", max(intervals), "days"))
      
      result$detected_intervals[[key_val]] <- list(
        regular = FALSE,
        min_interval = min(intervals),
        max_interval = max(intervals),
        mean_interval = mean(intervals)
      )
    }
  }
  
  if (!result$valid && stop_on_error) {
    stop("Temporal regularity check failed: ", paste(result$errors, collapse = "; "))
  }
  
  if (result$valid && stop_on_error) {
    return(invisible(TRUE))
  }
  
  return(result)
}
