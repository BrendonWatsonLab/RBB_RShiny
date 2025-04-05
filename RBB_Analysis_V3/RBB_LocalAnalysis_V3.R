# ============================================================================
# Script: RBB_LocalAnalysis_V3.R #
# Author: Noah Muscat, Simeone Marino, Gemini AI Assistant
# Date: 2025-04-04
# R Version: 4.x recommended
# Description:
#   Processes raw behavioral data from RBB experiments incrementally.
#   Reads new raw CSV files, calculates wheel movement and beambreak metrics,
#   and **appends** results to daily consolidated output files per rat per data type
#   (WheelMovement, File3, File6, File7, File8).
#
#   **INCREMENTAL PROCESSING:** Uses a log file to track processed *raw* files.
#   **OUTPUT:** Daily consolidated CSV files (e.g., RBBXX_File6_YYYY-MM-DD.csv).
#   **MIRRORED OUTPUT STRUCTURE:** Saves consolidated files within the
#   corresponding experiment_XX/cohort_XX structure in main_output_dir.
#
# Inputs:
#   - root_data_dir: Path to raw data (experiment_XX/cohort_XX/RBB...).
#   - main_output_dir: Path where consolidated output structure will be saved.
#   - processed_log_file: Path to log file tracking processed raw files.
#
# Dependencies: data.table, dplyr, tidyr, lubridate, stringr
# ============================================================================

# --- User-Defined Parameters ---

# Path to the root directory containing RAW experiment_XX/cohort_XX folders
# ** Use absolute path for cron compatibility **
root_data_dir <- "/nfs/turbo/umms-brendonw/RBB_Data" # <<< SET RAW INPUT ROOT PATH HERE

# Path to the *main* directory where all CONSOLIDATED output files will be saved
# ** Use absolute path for cron compatibility **
# !! IMPORTANT: Ensure this directory exists or the script has permission to create it. !!
main_output_dir <- "/nfs/turbo/umms-brendonw/RBB_Data_Cleaned" # <<< SET CONSOLIDATED OUTPUT PATH HERE

# Path for the log file tracking processed RAW files.
# ** Use absolute path for cron compatibility **
processed_log_file <- file.path(main_output_dir, "processed_files.log")

# --- Reprocessing Override Switch ---
# Set to TRUE to ignore the processed log file and re-analyze all found RAW CSV files.
OVERRIDE_LOG <- FALSE # <<< SET TO TRUE TO FORCE REPROCESSING

# Wheel movement calculation parameters (V) - Verify value is correct for setup
WHEEL_VOLTAGE_MAX <- 5.00
WHEEL_VOLTAGE_THRESHOLD <- 0.25

# Timezone for converting POSIX timestamps. Adjust if necessary.
TIMEZONE <- "UTC"

# Fixed descriptive labels for the 8 digital channels (0-7)
CHANNEL_LABELS <- c(
  "Water_1_Beambreak",  # Channel 0
  "Water_2_Beambreak",  # Channel 1
  "Food_1_Beambreak",   # Channel 2
  "Food_2_Beambreak",   # Channel 3
  "Water_1_Dispense",   # Channel 4
  "Water_2_Dispense",   # Channel 5
  "Food_1_Dispense",    # Channel 6
  "Food_2_Dispense"     # Channel 7
)

# --- Load Required Libraries ---

message("Loading required packages...")
# Removed tidyr, keep bit64 just in case fread needs it implicitly
required_packages <- c("data.table", "dplyr", "lubridate", "stringr", "bit64")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing package:", pkg))
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}
message("Packages loaded successfully.")

# --- Helper Functions ---

#' Convert integer to binary vector
number2binary <- function(number, noBits) {
  # Uses bitwise operations which are generally fast
  binary_vector <- as.numeric(intToBits(number))[1:noBits]
  # The intToBits result needs reversing for standard high-to-low bit order
  return(rev(binary_vector))
}


#' Calculate single step wheel movement (handles wrap-around & threshold)
calculate_single_step_movement <- function(v1, v2, v_max = WHEEL_VOLTAGE_MAX, v_threshold = WHEEL_VOLTAGE_THRESHOLD) {
  if(is.na(v1) || is.na(v2)) { return(0) }
  dist_simple <- abs(v1 - v2)
  # Check for potential NA in max/min result if v_max is weird, default to simple diff
  dist_wrap <- tryCatch(v_max - max(v1, v2) + min(v1, v2), error = function(e) dist_simple)
  if(is.na(dist_wrap)) dist_wrap <- dist_simple # Fallback if calculation failed
  
  dist_actual <- min(dist_simple, dist_wrap, na.rm = TRUE)
  movement <- ifelse(dist_actual <= v_threshold, 0, dist_actual)
  return(movement)
}

#' Calculate wheel movement vector
calculate_wheel_movement_wrap <- function(wheel_voltages, voltage_max = WHEEL_VOLTAGE_MAX, voltage_threshold = WHEEL_VOLTAGE_THRESHOLD) {
  if (!is.numeric(wheel_voltages)) { stop("Input 'wheel_voltages' must be a numeric vector.") }
  n <- length(wheel_voltages)
  if (n < 2) { return(rep(0, n)) }
  movement_values <- numeric(n)
  movement_values[1] <- 0
  # Vectorized approach for calculating differences
  v1 <- wheel_voltages[-n] # All except last
  v2 <- wheel_voltages[-1] # All except first
  
  # Apply step function row-wise or element-wise using mapply
  step_movements <- mapply(calculate_single_step_movement, v1, v2, 
                           MoreArgs = list(v_max = voltage_max, v_threshold = voltage_threshold))
  
  movement_values[2:n] <- step_movements
  
  return(movement_values)
}

#' Create binned summary data (Counts and Durations) using data.table.
#' (Final version from previous steps)
create_binned_data <- function(event_data, bin_width) {
  
  # --- Input Validation & Setup ---
  if (!is.data.frame(event_data) || !all(c("StartTime", "Label", "Duration") %in% names(event_data))) { stop("Input 'event_data' must be a data frame with 'StartTime', 'Label', and 'Duration' columns.") }
  if (!lubridate::is.POSIXct(event_data$StartTime)) { stop("'StartTime' column must be POSIXct.") }
  if (!is.numeric(event_data$Duration)) { stop("'Duration' column must be numeric (in seconds).") }
  
  all_labels <- unique(CHANNEL_LABELS)
  expected_count_cols <- paste("Count", all_labels, sep = "_")
  expected_duration_cols <- paste("TotalDuration", all_labels, sep = "_")
  expected_value_cols <- c(expected_count_cols, expected_duration_cols)
  final_ordered_cols <- c("BinStartTime", expected_value_cols)
  
  # Handle empty input
  if (nrow(event_data) == 0) {
    # message(paste("Input data for binning ('", bin_width, "') is empty. Returning empty data.table.", sep=""))
    empty_dt <- data.table::data.table(BinStartTime = as.POSIXct(character()))
    for (col in expected_value_cols) { empty_dt[, (col) := if(startsWith(col, "Count_")) integer(0) else numeric(0)] }
    try(data.table::setcolorder(empty_dt, neworder = final_ordered_cols), silent=TRUE)
    return(empty_dt)
  }
  
  message(paste("  Binning data by:", bin_width, "using data.table"))
  
  # --- Convert to data.table and Aggregate ---
  event_dt <- data.table::as.data.table(event_data)
  event_dt[, BinStartTime := lubridate::floor_date(StartTime, unit = bin_width)]
  binned_dt <- event_dt[, .( Count = .N, TotalDuration = sum(Duration, na.rm = TRUE) ), by = .(BinStartTime, Label)]
  
  # --- Reshape to Wide Format using dcast ---
  binned_wide_dt <- tryCatch({
    data.table::dcast(binned_dt, BinStartTime ~ Label, value.var = c("Count", "TotalDuration"), fill = 0, sep = "_")
  }, error = function(e){
    message("!!! ERROR during data.table::dcast: ", e$message); return(data.table::data.table())
  })
  
  # --- Ensure All Expected Columns Exist ---
  current_columns <- names(binned_wide_dt)
  missing_cols <- setdiff(expected_value_cols, current_columns)
  
  if (length(missing_cols) > 0) {
    # message(paste("    Adding missing value columns (filled with 0):", paste(missing_cols, collapse=", ")))
    suppressWarnings({
      for (col in missing_cols) {
        if (startsWith(col, "Count_")) { binned_wide_dt[, (col) := 0L] }
        else { binned_wide_dt[, (col) := 0.0] }
      }
    })
  }
  
  # --- Final Structuring and Ordering ---
  binned_final <- NULL
  if("BinStartTime" %in% names(binned_wide_dt) && nrow(binned_wide_dt) > 0) {
    final_select_cols <- intersect(final_ordered_cols, names(binned_wide_dt))
    extra_cols <- setdiff(names(binned_wide_dt), final_select_cols)
    if(length(extra_cols) > 0) {
      # message(paste("    Removing extra columns:", paste(extra_cols, collapse=", ")))
      binned_wide_dt[, (extra_cols) := NULL]
    }
    data.table::setcolorder(binned_wide_dt, neworder = final_select_cols)
    data.table::setorder(binned_wide_dt, BinStartTime)
    binned_final <- binned_wide_dt
  } else {
    message("    Result after dcast/adding columns is empty or missing BinStartTime. Creating empty structure.")
    empty_dt <- data.table::data.table(BinStartTime = as.POSIXct(character()))
    for (col in expected_value_cols) { empty_dt[, (col) := if(startsWith(col, "Count_")) integer(0) else numeric(0)] }
    try(data.table::setcolorder(empty_dt, neworder = final_ordered_cols), silent=TRUE)
    binned_final <- empty_dt
  }
  
  return(binned_final)
} # End of function


# --- Main Processing Logic ---

message("Starting RBB data processing...")
start_time <- Sys.time()

# --- Read Log of Previously Processed Files (or Override) ---
processed_files_list <- character(0)
if (OVERRIDE_LOG) {
  message("OVERRIDE_LOG is TRUE. Ignoring processed files log and reprocessing all found files.")
} else if (file.exists(processed_log_file)) {
  message(paste("Reading processed files log:", processed_log_file))
  tryCatch({
    processed_files_list <- unique(readLines(processed_log_file, warn = FALSE))
    processed_files_list <- processed_files_list[processed_files_list != ""] # Remove empty lines
    message(paste("  Found", length(processed_files_list), "files previously processed."))
  }, error = function(e) {
    warning(paste("Could not read processed files log:", processed_log_file, "\nError:", e$message, "\nProcessing all found files."))
    processed_files_list <- character(0)
  })
} else {
  message("Processed files log not found. Will process all found files.")
}

# --- Find Input Files ---
message(paste("Searching for input CSV files in:", root_data_dir))
normalized_root <- normalizePath(root_data_dir, winslash = "/", mustWork = FALSE)
# Pattern to match the raw input files, adjust if necessary
# Example: RBB01_20250404_100000.csv
raw_file_pattern <- "RBB\\d{2}_\\d{8}_\\d{6}\\.csv$"
# Find files recursively, matching the pattern in the basename
all_files <- list.files(path = normalized_root, pattern = raw_file_pattern, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)

# Further filter based on expected path structure
input_files <- grep("experiment_[0-9]+/cohort_[0-9]+/.+\\.csv$", all_files, value = TRUE, ignore.case = TRUE)

if (length(input_files) == 0) {
  message("No input CSV files found matching the expected pattern and path structure.") # Changed from stop() to message()
} else {
  message(paste("Found", length(input_files), "total input CSV files potentially needing processing."))
}

# --- Process Files Incrementally ---
message("Processing files...")
files_processed_this_run <- 0
files_skipped_count <- 0
files_error_count <- 0

if (length(input_files) > 0) { # Only loop if files were found
  for (input_csv_path in input_files) {
    
    base_filename <- basename(input_csv_path)
    
    # Check if file was already processed
    if (!OVERRIDE_LOG && (input_csv_path %in% processed_files_list)) {
      files_skipped_count <- files_skipped_count + 1
      next # Skip
    }
    
    if (OVERRIDE_LOG && (input_csv_path %in% processed_files_list)) {
      message(paste("\nReprocessing previously processed file:", base_filename))
    } else {
      message(paste("\nProcessing NEW file:", base_filename))
    }
    
    # Wrap individual file processing in tryCatch
    tryCatch({
      
      # Determine output subdirectory
      file_dir <- dirname(input_csv_path)
      relative_path <- sub(paste0("^", normalized_root, "/?"), "", file_dir)
      target_sub_dir <- file.path(main_output_dir, relative_path)
      
      # Create output subdirectory if needed
      if (!dir.exists(target_sub_dir)) {
        message(paste("  Creating output subdirectory:", target_sub_dir))
        dir.create(target_sub_dir, recursive = TRUE, showWarnings = FALSE)
      }
      
      # Get base info from filename (for Rat ID parsing)
      base_output_name <- tools::file_path_sans_ext(base_filename)
      rat_id_match <- stringr::str_match(base_output_name, "^(RBB\\d{2})_")
      if (is.na(rat_id_match[1,2])) { stop("Failed to parse Rat ID for saving.") }
      rat_id <- rat_id_match[1,2]
      
      # --- Read Input CSV Data ---
      message("  Reading data...")
      raw_data <- data.table::fread( input_csv_path, showProgress = FALSE, colClasses = c(POSIX = "numeric") )
      
      required_cols <- c("POSIX", "Digital Pins", "Wheel Analog")
      if (!all(required_cols %in% names(raw_data))) { stop(paste("Input file missing required columns:", paste(setdiff(required_cols, names(raw_data)), collapse=", "))) }
      if (nrow(raw_data) == 0) { message("  Input file is empty. Skipping processing steps."); next } # Skip if empty
      if (!is.numeric(raw_data$POSIX)) { stop("fread failed to read POSIX as numeric") }
      if (anyNA(raw_data$POSIX)) { warning(paste("NA values present in POSIX column after fread for file:", base_filename)) }
      
      raw_data_df <- as.data.frame(raw_data) # Keep a data frame version if needed
      
      # --- Prepare Timestamps ---
      message("  Preparing timestamps...")
      timestamps_posixct <- as.POSIXct(raw_data$POSIX / 1e6, origin = "1970-01-01", tz = TIMEZONE)
      if (all(is.na(timestamps_posixct))) { stop("Timestamp conversion resulted in all NA values") }
      else if (anyNA(timestamps_posixct)) { warning("Some NA values in timestamps after conversion") }
      
      # --- Calculate Intermediate Data (only if needed by subsequent steps) ---
      # We need File 3 for event detection, File 5 for binning, Wheel for saving
      
      # Create File 3 Data (needed for event detection)
      digital_pins_raw <- raw_data$Digital.Pins # Use standard data.table access
      binary_matrix <- t(sapply(digital_pins_raw, number2binary, noBits = 8))
      inverted_binary_matrix <- ifelse(binary_matrix == 0, 1, 0)
      file3_data <- data.table(Timestamp = timestamps_posixct, DigitalPins_Raw = digital_pins_raw)
      channel_colnames <- paste0("Channel_", 0:7)
      inverted_binary_dt <- as.data.table(inverted_binary_matrix)
      setnames(inverted_binary_dt, channel_colnames)
      file3_data <- cbind(file3_data, inverted_binary_dt)
      
      # Calculate File 4/5 Data (Event List - needed for binning)
      file4_list <- list()
      for (j in 0:7) {
        channel_vector <- file3_data[[paste0("Channel_", j)]]
        starts <- which(diff(c(0, channel_vector)) == 1)
        ends <- which(diff(c(channel_vector, 0)) == -1)
        if (length(starts) > 0 && length(ends) > 0) {
          n_events <- min(length(starts), length(ends)); if(n_events > 0) {
            valid_indices <- (1:n_events)[ends[1:n_events] >= starts[1:n_events]]
            if(length(valid_indices) > 0) {
              start_indices <- starts[valid_indices]; end_indices <- ends[valid_indices]
              event_start_times <- timestamps_posixct[start_indices]; event_end_times <- timestamps_posixct[end_indices]
              durations_seconds <- as.numeric(difftime(event_end_times, event_start_times, units = "secs"))
              valid_durations <- durations_seconds > 0
              if (sum(valid_durations) > 0) {
                channel_events <- data.table( StartTime = event_start_times[valid_durations], ChannelID = j, Duration = durations_seconds[valid_durations] )
                if(nrow(channel_events) > 0) { file4_list[[length(file4_list) + 1]] <- channel_events }
              } } } } } # End channel loop
      
      if (length(file4_list) > 0) {
        file4_data <- data.table::rbindlist(file4_list)
        data.table::setorder(file4_data, StartTime)
        # Create File 5 by adding labels
        file5_data <- file4_data[, Label := CHANNEL_LABELS[ChannelID + 1]][, .(StartTime, Label, Duration)]
      } else {
        file5_data <- data.table(StartTime=as.POSIXct(character()), Label=character(), Duration=numeric())
        message("      WARNING: No beambreak events detected.")
      }
      
      # Calculate Binned Data (Files 6, 7, 8)
      if (nrow(file5_data) > 0) {
        message("    Generating binned data...")
        file6_data <- create_binned_data(file5_data, bin_width = "1 min")
        file7_data <- create_binned_data(file5_data, bin_width = "1 hour")
        file8_data <- create_binned_data(file5_data, bin_width = "1 day")
      } else {
        # Create empty structures if no events
        file6_data <- create_binned_data(file5_data, bin_width = "1 min") # Will return empty structure
        file7_data <- create_binned_data(file5_data, bin_width = "1 hour")
        file8_data <- create_binned_data(file5_data, bin_width = "1 day")
        message("      SKIPPING Binned file generation as no events detected.")
      }
      
      # Calculate Wheel Data
      message("  Processing wheel data...")
      wheel_voltages <- raw_data$Wheel.Analog # Adjust name if needed
      wheel_movement <- calculate_wheel_movement_wrap( wheel_voltages, WHEEL_VOLTAGE_MAX, WHEEL_VOLTAGE_THRESHOLD )
      wheel_output <- data.table(Timestamp = timestamps_posixct, Wheel_Movement = wheel_movement)
      
      # --- Consolidate and Save Processed Data Chunks ---
      message("    Consolidating and saving processed data chunks...")
      
      # List of final data.tables to save/append
      output_data_list <- list(
        File3 = file3_data,
        File6 = file6_data,
        File7 = file7_data,
        File8 = file8_data,
        Wheel = wheel_output
      )
      timestamp_cols <- list(File3 = "Timestamp", File6 = "BinStartTime", File7 = "BinStartTime", File8 = "BinStartTime", Wheel = "Timestamp")
      
      for (data_type_name in names(output_data_list)) {
        current_data_chunk <- output_data_list[[data_type_name]]
        if (is.null(current_data_chunk) || nrow(current_data_chunk) == 0) { next }
        if (!is.data.table(current_data_chunk)) { current_data_chunk <- data.table::as.data.table(current_data_chunk) }
        
        ts_col <- timestamp_cols[[data_type_name]]
        if (!ts_col %in% names(current_data_chunk)) {
          warning(paste("Timestamp column '", ts_col, "' not found for data type '", data_type_name, "' in file ", base_filename, ". Skipping.", sep="")); next
        }
        
        # Ensure timestamp column is POSIXct before flooring
        if (!lubridate::is.POSIXct(current_data_chunk[[ts_col]])) {
          warning(paste("Timestamp column '", ts_col, "' is not POSIXct for '", data_type_name, "' in file ", base_filename, ". Attempting conversion.", sep=""))
          # Add robust conversion attempt here if needed, otherwise skip/error
          current_data_chunk[, (ts_col) := as.POSIXct(get(ts_col), origin="1970-01-01", tz=TIMEZONE)] # Basic attempt
        }
        
        chunk_dates <- unique(as.Date(lubridate::floor_date(current_data_chunk[[ts_col]], "day", week_start = 1))) # Ensure week_start if using week bins later
        chunk_dates <- chunk_dates[!is.na(chunk_dates)]
        if (length(chunk_dates) == 0) {
          warning(paste("No valid dates found for data type '", data_type_name, "' in file ", base_filename, ". Skipping.", sep="")); next
        }
        
        for (current_date in chunk_dates) {
          current_date_str <- format(current_date, "%Y-%m-%d")
          daily_output_filename <- file.path( target_sub_dir, paste0(rat_id, "_", data_type_name, "_", current_date_str, ".csv") )
          
          # Filter using data.table syntax (more explicit and potentially safer)
          date_filter_expr <- substitute(as.Date(floor_date(TS_COL, "day")) == DATE_VAL, list(TS_COL = as.name(ts_col), DATE_VAL = current_date))
          data_for_this_date <- current_data_chunk[eval(date_filter_expr)]
          
          if (nrow(data_for_this_date) > 0) {
            file_already_exists <- file.exists(daily_output_filename)
            message(paste("      -> ", if(file_already_exists) "Appending" else "Writing", nrow(data_for_this_date), "rows to:", basename(daily_output_filename)))
            tryCatch({
              data.table::fwrite( data_for_this_date, file = daily_output_filename, append = file_already_exists, col.names = !file_already_exists )
            }, error = function(e){ warning(paste("!!! Failed to write/append to", basename(daily_output_filename), "Error:", e$message)) })
          }
        } # End date loop
      } # End data type loop
      
      # --- SUCCESS: Log Raw File as Processed ---
      if (!OVERRIDE_LOG) {
        message("  Successfully processed. Logging file.")
        tryCatch( write(input_csv_path, file = processed_log_file, append = TRUE), error = function(e) {warning("Could not write to processed log file: ", processed_log_file)} )
      } else {
        message("  Successfully processed (Override Mode - log file not updated).")
      }
      files_processed_this_run <- files_processed_this_run + 1
      
    }, error = function(e) { # Error handling for the whole file
      files_error_count <- files_error_count + 1
      message("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"); message(paste("ERROR processing file:", base_filename)); message(paste("Error message:", e$message)); message("Stack trace:"); try(message(paste(capture.output(traceback()), collapse = "\n")), silent = TRUE); message("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    }) # End tryCatch wrapper
  } # End main file loop
} else { # If no input files found initially
  message("No input files found to process.")
}

# --- Script Completion ---
end_time <- Sys.time()
duration <- end_time - start_time

message("\n========================================================")
message("RBB data processing finished.")
message(paste("  Total time:", format(duration)))
message(paste("  Files processed/reprocessed in this run:", files_processed_this_run))
message(paste("  Files skipped (already processed):", files_skipped_count))
message(paste("  Files with errors:", files_error_count))
message(paste("Output files saved in subdirectories under:", main_output_dir))
message(paste("Processed file log:", processed_log_file))
if (files_error_count > 0) {
  message(paste("!! WARNING:", files_error_count, "file(s) encountered errors during processing. Check messages above or error log if implemented."))
}
message("========================================================")