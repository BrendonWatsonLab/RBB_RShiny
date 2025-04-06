# ============================================================================
# Script: RBB_Data_Processor_Final_CLI.R # Renamed for clarity
# Author: Noah Muscat, Simeone Marino, Gemini AI Assistant
# Date: 2025-04-04
# R Version: 4.x recommended
# Description:
#   Processes raw behavioral data from RBB experiments. Iterates through
#   experiment/cohort subdirectories, reads individual .csv files, performs
#   analysis for wheel movement and beambreak events (Files 3-8).
#
#   **INCREMENTAL PROCESSING:** Maintains a log file to track successfully
#   processed files and only processes new files by default. Set the
#   OVERRIDE_LOG parameter to TRUE to force reprocessing of all files.
#
#   **MIRRORED OUTPUT STRUCTURE:** Recreates the relative input directory
#   structure within the main output directory.
#
#   **COMMAND-LINE/CRON USAGE NOTES:**
#     - Ensure all paths (root_data_dir, main_output_dir, processed_log_file)
#       are specified as ABSOLUTE paths.
#     - Ensure the user running the script via cron has necessary READ permissions
#       for input data and READ/WRITE permissions for the output directory and log files.
#     - Redirect stdout/stderr in the crontab entry (e.g., >> /path/to/cron.log 2>&1)
#       to capture messages and errors.
#     - The script attempts to install missing packages; ensure permissions and
#       internet access allow this, or pre-install packages manually.
#     - Minimal cron environment might lack specific environment variables if needed.
#
# Inputs/Outputs: (Same as previous version description)
# Dependencies: data.table, dplyr, tidyr, lubridate, stringr
# ============================================================================

# --- User-Defined Parameters ---

# Path to the root directory containing experiment_XX/cohort_XX folders
# ** Use absolute path for cron compatibility **
root_data_dir <- "/nfs/turbo/umms-brendonw/RBB_Data" # <<< SET INPUT ROOT PATH HERE

# Path to the *main* directory where all output files and subdirectories will be saved
# ** Use absolute path for cron compatibility **
# !! IMPORTANT: Ensure this directory exists or the script has permission to create it. !!
main_output_dir <- "/nfs/turbo/umms-brendonw/RBB_Data_Cleaned" # <<< SET MAIN OUTPUT PATH HERE

# Path for the log file tracking processed files.
# ** Use absolute path for cron compatibility **
processed_log_file <- file.path(main_output_dir, "processed_files.log")

# --- Reprocessing Override Switch ---
# Set to TRUE to ignore the processed log file and re-analyze all found CSV files.
# Set to FALSE to only process new/unlogged files.
OVERRIDE_LOG <- FALSE # <<< SET TO TRUE TO FORCE REPROCESSING

# Wheel movement calculation parameters
WHEEL_VOLTAGE_MAX <- 5.0
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
required_packages <- c("data.table", "dplyr", "tidyr", "lubridate", "stringr")

# Check, install if missing, and load packages
# Note: Installation might fail in restricted cron environments. Pre-install if needed.
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
  binary_vector <- rev(as.numeric(intToBits(number)))
  if (length(binary_vector) >= noBits) {
    return(binary_vector[-(1:(length(binary_vector) - noBits))])
  } else {
    return(c(rep(0, noBits - length(binary_vector)), binary_vector))
  }
}

#' Calculate single step wheel movement (handles wrap-around & threshold)
calculate_single_step_movement <- function(v1, v2, v_max = 3.3, v_threshold = 0.25) {
  if(is.na(v1) || is.na(v2)) { return(0) }
  dist_simple <- abs(v1 - v2)
  dist_wrap <- v_max - max(v1, v2) + min(v1, v2)
  dist_actual <- min(dist_simple, dist_wrap)
  movement <- ifelse(dist_actual <= v_threshold, 0, dist_actual)
  return(movement)
}

#' Calculate wheel movement vector
calculate_wheel_movement_wrap <- function(wheel_voltages, voltage_max = 3.3, voltage_threshold = 0.25) {
  if (!is.numeric(wheel_voltages)) { stop("Input 'wheel_voltages' must be a numeric vector.") }
  n <- length(wheel_voltages)
  if (n < 2) { return(rep(0, n)) }
  movement_values <- numeric(n)
  movement_values[1] <- 0
  for (i in 2:n) {
    movement_values[i] <- calculate_single_step_movement(
      v1 = wheel_voltages[i - 1], v2 = wheel_voltages[i],
      v_max = voltage_max, v_threshold = voltage_threshold
    )
  }
  return(movement_values)
}

#' Create binned summary data (Counts and Durations) using data.table.
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
    message(paste("Input data for binning ('", bin_width, "') is empty. Returning empty data.table.", sep=""))
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
    # message(paste("    Adding missing value columns (filled with 0):", paste(missing_cols, collapse=", "))) # Keep comment minimal
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
      # message(paste("    Removing extra columns:", paste(extra_cols, collapse=", "))) # Keep comment minimal
      binned_wide_dt[, (extra_cols) := NULL]
    }
    # Ensure final column order
    data.table::setcolorder(binned_wide_dt, neworder = final_select_cols)
    # Order rows
    data.table::setorder(binned_wide_dt, BinStartTime)
    binned_final <- binned_wide_dt
  } else {
    # Handle cases where dcast failed or produced empty results
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
    # Remove any potential empty lines read
    processed_files_list <- processed_files_list[processed_files_list != ""]
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
all_files <- list.files(path = normalized_root, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
# Filter more specifically if needed
input_files <- grep("experiment_[0-9]+/cohort_[0-9]+/.+\\.csv$", all_files, value = TRUE)

if (length(input_files) == 0) {
  stop(paste("No input CSV files found matching the expected pattern in:", root_data_dir))
} else {
  message(paste("Found", length(input_files), "total input CSV files potentially needing processing."))
}

# --- Process Files Incrementally ---
message("Processing files...")
files_processed_this_run <- 0
files_skipped_count <- 0
files_error_count <- 0

for (input_csv_path in input_files) {
  
  base_filename <- basename(input_csv_path)
  
  # Check if file was already processed (skip only if OVERRIDE_LOG is FALSE)
  if (!OVERRIDE_LOG && (input_csv_path %in% processed_files_list)) {
    files_skipped_count <- files_skipped_count + 1
    next # Skip to the next file
  }
  
  if (OVERRIDE_LOG && (input_csv_path %in% processed_files_list)) {
    message(paste("\nReprocessing previously processed file:", base_filename))
  } else {
    message(paste("\nProcessing NEW file:", base_filename))
  }
  
  # Wrap individual file processing in tryCatch
  tryCatch({
    
    # Determine output subdirectory based on input relative path
    file_dir <- dirname(input_csv_path)
    relative_path <- sub(paste0("^", normalized_root, "/?"), "", file_dir)
    target_sub_dir <- file.path(main_output_dir, relative_path)
    
    # Create output subdirectory if needed
    if (!dir.exists(target_sub_dir)) {
      message(paste("  Creating output subdirectory:", target_sub_dir))
      dir.create(target_sub_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Base name for output files
    base_output_name <- tools::file_path_sans_ext(base_filename)
    
    # Read Input CSV Data
    message("  Reading data...")
    raw_data <- data.table::fread(input_csv_path, showProgress = FALSE)
    required_cols <- c("POSIX", "Digital Pins", "Wheel Analog")
    if (!all(required_cols %in% names(raw_data))) {
      missing_cols <- setdiff(required_cols, names(raw_data))
      stop(paste("Input file missing required columns:", paste(missing_cols, collapse=", ")))
    }
    raw_data <- as.data.frame(raw_data) # Convert as some helpers might expect data.frame
    
    # Prepare Timestamps
    message("  Preparing timestamps...")
    timestamps_posixct <- as.POSIXct(raw_data$POSIX / 1e6, origin = "1970-01-01", tz = TIMEZONE)
    
    # --- Process Wheel Data ---
    message("  Processing wheel data...")
    wheel_voltages <- raw_data$`Wheel Analog`
    wheel_movement <- calculate_wheel_movement_wrap( wheel_voltages, WHEEL_VOLTAGE_MAX, WHEEL_VOLTAGE_THRESHOLD )
    wheel_output <- data.frame(Timestamp = timestamps_posixct, Wheel_Movement = wheel_movement)
    wheel_output_filename <- file.path(target_sub_dir, paste0(base_output_name, "_WheelMovement.csv"))
    message(paste("    Saving:", basename(wheel_output_filename)))
    write.csv(wheel_output, wheel_output_filename, row.names = FALSE, quote = TRUE)
    
    # --- Process Beambreak Data ---
    message("  Processing beambreak data...")
    
    # == Generate File 3: Raw Binary States ==
    message("    Generating File 3...")
    digital_pins_raw <- raw_data$`Digital Pins`
    binary_matrix <- t(sapply(digital_pins_raw, number2binary, noBits = 8))
    inverted_binary_matrix <- ifelse(binary_matrix == 0, 1, 0) # 1 = Event/Break
    file3_data <- data.frame(Timestamp = timestamps_posixct, DigitalPins_Raw = digital_pins_raw)
    colnames(inverted_binary_matrix) <- paste0("Channel_", 0:7)
    file3_data <- cbind(file3_data, inverted_binary_matrix)
    file3_output_filename <- file.path(target_sub_dir, paste0(base_output_name, "_File3.csv"))
    message(paste("      Saving:", basename(file3_output_filename)))
    write.csv(file3_data, file3_output_filename, row.names = FALSE, quote = TRUE)
    
    # == Generate File 4: Event List ==
    message("    Generating File 4 (Detecting Events)...")
    file4_list <- list()
    for (j in 0:7) {
      channel_vector <- file3_data[[paste0("Channel_", j)]]
      starts <- which(diff(c(0, channel_vector)) == 1)
      ends <- which(diff(c(channel_vector, 0)) == -1)
      if (length(starts) > 0 && length(ends) > 0) {
        n_events <- min(length(starts), length(ends))
        if(n_events > 0) {
          valid_indices <- 1:n_events
          valid_indices <- valid_indices[ends[valid_indices] >= starts[valid_indices]]
          if(length(valid_indices) > 0) {
            start_indices <- starts[valid_indices]; end_indices <- ends[valid_indices]
            event_start_times <- timestamps_posixct[start_indices]; event_end_times <- timestamps_posixct[end_indices]
            durations_seconds <- as.numeric(difftime(event_end_times, event_start_times, units = "secs"))
            valid_durations <- durations_seconds > 0
            if (sum(valid_durations) > 0) {
              channel_events <- data.frame( StartTime = event_start_times[valid_durations], ChannelID = j, Duration = durations_seconds[valid_durations] )
              if(nrow(channel_events) > 0) { file4_list[[length(file4_list) + 1]] <- channel_events }
            } } } } } # End channel loop
    
    if (length(file4_list) > 0) {
      file4_data <- dplyr::bind_rows(file4_list) %>% dplyr::arrange(StartTime)
    } else {
      file4_data <- data.frame(StartTime=as.POSIXct(character()), ChannelID=integer(), Duration=numeric())
      message("      WARNING: No beambreak events detected for File 4.")
    }
    file4_output_filename <- file.path(target_sub_dir, paste0(base_output_name, "_File4.csv"))
    message(paste("      Saving:", basename(file4_output_filename)))
    write.csv(file4_data, file4_output_filename, row.names = FALSE, quote = TRUE)
    
    
    # == Generate File 5: Labeled Events ==
    message("    Generating File 5...")
    if (nrow(file4_data) > 0) {
      file5_data <- file4_data %>% mutate(Label = CHANNEL_LABELS[ChannelID + 1]) %>% select(StartTime, Label, Duration)
    } else {
      file5_data <- data.frame(StartTime=as.POSIXct(character()), Label=character(), Duration=numeric())
      message("      WARNING: File 4 was empty, File 5 will also be empty.")
    }
    file5_output_filename <- file.path(target_sub_dir, paste0(base_output_name, "_File5.csv"))
    message(paste("      Saving:", basename(file5_output_filename)))
    write.csv(file5_data, file5_output_filename, row.names = FALSE, quote = TRUE)
    
    
    # == Generate Files 6, 7, 8: Binned Data ==
    if (nrow(file5_data) > 0) {
      message("    Generating Files 6, 7, 8 (Binned Data)...")
      bin_widths <- c("1 min", "1 hour", "1 day"); file_numbers <- c(6, 7, 8)
      for(i in 1:length(bin_widths)){
        binned_data <- create_binned_data(file5_data, bin_width = bin_widths[i])
        output_filename <- file.path(target_sub_dir, paste0(base_output_name, "_File", file_numbers[i], ".csv"))
        message(paste("      Saving:", basename(output_filename)))
        # Use write.csv for binned data (as fwrite previously caused issues)
        write.csv(binned_data, output_filename, row.names = FALSE, quote = TRUE)
      } # End loop through bin widths
    } else {
      message("      SKIPPING Files 6, 7, 8 because File 5 was empty.")
    }
    
    # --- SUCCESS: Log File as Processed (only if override is FALSE) ---
    if (!OVERRIDE_LOG) {
      message("  Successfully processed. Logging file.")
      # Use tryCatch for writing log, in case of permission issues etc.
      tryCatch( write(input_csv_path, file = processed_log_file, append = TRUE),
                error = function(e) {warning("Could not write to processed log file: ", processed_log_file)} )
    } else {
      message("  Successfully processed (Override Mode - log file not updated).")
    }
    files_processed_this_run <- files_processed_this_run + 1
    
    
  }, error = function(e) {
    # --- ERROR Handling for this file ---
    files_error_count <- files_error_count + 1
    message(paste("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"))
    message(paste("ERROR processing file:", base_filename))
    message(paste("Error message:", e$message))
    message("Stack trace:")
    try(message(paste(capture.output(traceback()), collapse = "\n")), silent = TRUE) # Add try() around traceback
    message(paste("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"))
    # Consider adding failed file path to a separate error log
    # error_log_file <- file.path(main_output_dir, "ERROR_FILES.log")
    # tryCatch( write(input_csv_path, file = error_log_file, append = TRUE), error = function(e2){} )
    
  }) # End tryCatch wrapper for single file processing
  
} # End loop through input files

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