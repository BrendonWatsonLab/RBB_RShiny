# ============================================================================
# Script: RBB_LocalAnalysis_V3.R
# Author: Noah Muscat, Simeone Marino
# Date: 2025-04-05
# R Version: 4.x recommended
# Description:
#   Processes raw behavioral data from RBB experiments. Iterates through
#   experiment/cohort subdirectories, reads individual .csv files, performs
#   analysis for wheel movement and beambreak events (Files 3-8).
#   Handles input files containing only headers (no data rows).
#
#   **INCREMENTAL PROCESSING:** Maintains a log file to track successfully
#   processed/handled files and only processes new files by default. Set the
#   OVERRIDE_LOG parameter to TRUE to force reprocessing of all files.
#
#   **MANIFEST FILE:** Generates and updates a "processed_manifest.rds" file
#   in the main output directory, containing metadata about successfully
#   generated output files (files with actual data). Used by Shiny app.
#
#   **MIRRORED OUTPUT STRUCTURE:** Recreates the relative input directory
#   structure within the main output directory.
#
#   **COMMAND-LINE/CRON USAGE NOTES:** (Same as before)
#
# Inputs/Outputs: (Same as before, plus processed_manifest.rds)
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
# Also forces regeneration of the manifest file.
# Set to FALSE to only process new/unlogged files and update the manifest incrementally.
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
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing package:", pkg))
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}
message("Packages loaded successfully.")

# --- Helper Functions ---
# (Helper functions: number2binary, calculate_single_step_movement,
# calculate_wheel_movement_wrap, create_binned_data remain exactly the same as before)
# ...

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
calculate_single_step_movement <- function(v1, v2, v_max = WHEEL_VOLTAGE_MAX, v_threshold = WHEEL_VOLTAGE_THRESHOLD) {
  if(is.na(v1) || is.na(v2)) { return(0) }
  dist_simple <- abs(v1 - v2)
  dist_wrap <- v_max - max(v1, v2) + min(v1, v2)
  dist_actual <- min(dist_simple, dist_wrap)
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
  # Need to handle potential missing StartTime during conversion/aggregation
  event_dt <- event_dt[!is.na(StartTime)]
  if (nrow(event_dt) == 0) {
    message("No valid StartTime entries found after NA removal for binning.")
    # Return empty structure matching expected output
    empty_dt <- data.table::data.table(BinStartTime = as.POSIXct(character()))
    for (col in expected_value_cols) { empty_dt[, (col) := if(startsWith(col, "Count_")) integer(0) else numeric(0)] }
    try(data.table::setcolorder(empty_dt, neworder = final_ordered_cols), silent=TRUE)
    return(empty_dt)
  }
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
    # message(paste("     Adding missing value columns (filled with 0):", paste(missing_cols, collapse=", "))) # Keep comment minimal
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
      # message(paste("     Removing extra columns:", paste(extra_cols, collapse=", "))) # Keep comment minimal
      binned_wide_dt[, (extra_cols) := NULL]
    }
    # Ensure final column order
    data.table::setcolorder(binned_wide_dt, neworder = final_select_cols)
    # Order rows
    data.table::setorder(binned_wide_dt, BinStartTime)
    binned_final <- binned_wide_dt
  } else {
    # Handle cases where dcast failed or produced empty results
    message("     Result after dcast/adding columns is empty or missing BinStartTime. Creating empty structure.")
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

# <<< ADDED: Initialization for Manifest >>>
all_metadata_this_run <- list()
manifest_file_path <- file.path(main_output_dir, "processed_manifest.rds")
# <<< END ADDED Initialization >>>

# --- Read Log of Previously Processed Files (or Override) ---
# ... (log reading code remains the same) ...
processed_files_list <- character(0)
if (OVERRIDE_LOG) {
  message("OVERRIDE_LOG is TRUE. Ignoring processed files log and reprocessing all found files.")
} else if (file.exists(processed_log_file)) {
  message(paste("Reading processed files log:", processed_log_file))
  tryCatch({
    processed_files_list <- unique(readLines(processed_log_file, warn = FALSE))
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
# ... (file finding code remains the same) ...
message(paste("Searching for input CSV files in:", root_data_dir))
normalized_root <- normalizePath(root_data_dir, winslash = "/", mustWork = FALSE)
all_files <- list.files(path = normalized_root, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
input_files <- grep("experiment_\\d{2}/cohort_\\d{2}/RBB\\d{2}_\\d{8}_\\d{6}\\.csv$", all_files, value = TRUE, ignore.case = TRUE)
if (length(input_files) == 0) {
  stop(paste("No input CSV files found matching the expected pattern 'experiment_XX/cohort_XX/RBB##_YYYYMMDD_HHMMSS.csv' in:", root_data_dir))
} else {
  message(paste("Found", length(input_files), "total input CSV files potentially needing processing."))
}

# --- Process Files Incrementally ---
message("Processing files...")
files_processed_this_run <- 0
files_skipped_count <- 0
files_error_count <- 0
files_skipped_empty_count <- 0 # Counter for empty files

for (input_csv_path in input_files) {
  
  base_filename <- basename(input_csv_path)
  file_processed_flag <- FALSE # Flag to track if handled in any way this run
  
  # Check if file was already processed (skip only if OVERRIDE_LOG is FALSE)
  if (!OVERRIDE_LOG && (input_csv_path %in% processed_files_list)) {
    files_skipped_count <- files_skipped_count + 1
    next # Skip to the next file
  }
  
  # If we reach here, the file needs processing or handling (new, or override=TRUE)
  
  if (OVERRIDE_LOG && (input_csv_path %in% processed_files_list)) {
    message(paste("\nReprocessing previously processed file:", base_filename))
  } else {
    message(paste("\nProcessing NEW file:", base_filename))
  }
  
  # Wrap individual file processing in tryCatch
  tryCatch({
    
    # Determine output subdirectory based on input relative path
    # ... (path logic remains the same) ...
    file_dir <- dirname(input_csv_path)
    relative_path <- sub(paste0("^", gsub("\\\\", "/", normalized_root), "/?"), "", gsub("\\\\", "/", file_dir)) # Use gsub for cross-platform separator consistency
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
    
    # <<< --- ADDED: Check for empty data file (header only) --- >>>
    if (nrow(raw_data) == 0) {
      warning(paste("Input file contains no data rows (only header):", base_filename))
      files_skipped_empty_count <- files_skipped_empty_count + 1 # Increment specific counter
      
      # Log as processed to avoid re-processing, even though no output generated
      if (!OVERRIDE_LOG) {
        message("  Logging empty file as processed in log file.")
        tryCatch( write(input_csv_path, file = processed_log_file, append = TRUE),
                  error = function(e) {warning("Could not write to processed log file: ", processed_log_file)} )
      }
      # Set flag indicating handled, then skip rest of tryCatch block
      file_processed_flag <- TRUE
      # Use 'return()' to exit the tryCatch expression for this file cleanly
      return()
      
    }
    # <<< --- End of Empty File Check --- >>>
    
    # If we reach here, the file has data rows. Proceed with processing.
    
    # Check for required columns AFTER confirming rows exist
    required_cols <- c("POSIX", "Digital Pins", "Wheel Analog")
    if (!all(required_cols %in% names(raw_data))) {
      missing_cols <- setdiff(required_cols, names(raw_data))
      # Use stop() to trigger the error handler below
      stop(paste("Input file missing required columns:", paste(missing_cols, collapse=", ")))
    }
    
    # Extract Metadata (only if data rows exist)
    rat_id <- NA; date_str <- NA; time_str <- NA; exp_id <- NA; cohort_id <- NA; date_obj <- as.Date(NA)
    base_name_pattern <- "^(RBB\\d{2})_(\\d{8}_\\d{6})$"
    path_pattern <- ".*/(experiment_(\\d{2}))/(cohort_(\\d{2}))/.*"
    base_match <- stringr::str_match(base_output_name, base_name_pattern)
    path_match <- stringr::str_match(gsub("\\\\", "/", input_csv_path), path_pattern)
    if (!anyNA(base_match) && NCOL(base_match) >= 4 && !anyNA(path_match) && NCOL(path_match) >= 5) {
      rat_id <- base_match[1, 2] ; date_str <- base_match[1, 3] ; time_str <- base_match[1, 4]
      exp_id <- path_match[1, 3] ; cohort_id <- path_match[1, 5]
      date_obj <- lubridate::ymd(date_str, quiet = TRUE)
      message(paste("  Extracted Metadata: Exp=", exp_id, " Cohort=", cohort_id, " Rat=", rat_id, " Date=", date_str))
    } else {
      warning(paste("Could not parse Experiment/Cohort/Rat/Date from path/filename:", input_csv_path, " - Metadata will be skipped."))
    }
    
    # Prepare Timestamps
    message("  Preparing timestamps...")
    if (inherits(raw_data$POSIX, "integer64")) {
      timestamps_posixct <- as.POSIXct(as.double(raw_data$POSIX) / 1e6, origin = "1970-01-01", tz = TIMEZONE)
    } else {
      timestamps_posixct <- as.POSIXct(raw_data$POSIX / 1e6, origin = "1970-01-01", tz = TIMEZONE)
    }
    
    # --- Process Wheel Data ---
    message("  Processing wheel data...")
    wheel_voltages <- raw_data$`Wheel Analog`
    wheel_movement <- calculate_wheel_movement_wrap( wheel_voltages, WHEEL_VOLTAGE_MAX, WHEEL_VOLTAGE_THRESHOLD )
    wheel_output <- data.table(Timestamp = timestamps_posixct, Wheel_Movement = wheel_movement)
    wheel_output_filename <- file.path(target_sub_dir, paste0(base_output_name, "_WheelMovement.csv"))
    message(paste("    Saving:", basename(wheel_output_filename)))
    data.table::fwrite(wheel_output, wheel_output_filename, dateTimeAs = "write.csv")
    
    # --- Process Beambreak Data ---
    message("  Processing beambreak data...")
    # == Generate File 3 ==
    message("    Generating File 3...")
    digital_pins_raw <- raw_data$`Digital Pins`
    if (inherits(digital_pins_raw, "integer64")) digital_pins_raw <- as.integer(digital_pins_raw)
    binary_matrix <- t(sapply(digital_pins_raw, number2binary, noBits = 8))
    inverted_binary_matrix <- ifelse(binary_matrix == 0, 1, 0)
    file3_data <- data.table(Timestamp = timestamps_posixct, DigitalPins_Raw = digital_pins_raw)
    colnames(inverted_binary_matrix) <- paste0("Channel_", 0:7)
    file3_data <- cbind(file3_data, inverted_binary_matrix)
    file3_output_filename <- file.path(target_sub_dir, paste0(base_output_name, "_File3.csv"))
    message(paste("      Saving:", basename(file3_output_filename)))
    data.table::fwrite(file3_data, file3_output_filename, dateTimeAs = "write.csv")
    
    # == Generate File 4 ==
    message("    Generating File 4 (Detecting Events)...")
    # ... (File 4 logic remains the same) ...
    file4_list <- list()
    for (j in 0:7) {
      channel_vector <- file3_data[[paste0("Channel_", j)]]
      starts <- which(diff(c(0, channel_vector)) == 1)
      ends <- which(diff(c(channel_vector, 0)) == -1)
      if (length(starts) > 0 && length(ends) > 0) {
        if(starts[1] > ends[1]) ends <- ends[-1] ; if(length(ends) == 0) next
        if(ends[length(ends)] < starts[length(starts)]) starts <- starts[-length(starts)] ; if(length(starts) == 0) next
        n_events <- min(length(starts), length(ends))
        if(n_events > 0) {
          start_indices <- starts[1:n_events]; end_indices <- ends[1:n_events]
          valid_pair_indices <- which(end_indices >= start_indices)
          if(length(valid_pair_indices) > 0) {
            start_indices <- start_indices[valid_pair_indices]; end_indices <- end_indices[valid_pair_indices]
            event_start_times <- timestamps_posixct[start_indices]; event_end_times <- timestamps_posixct[end_indices]
            durations_seconds <- as.numeric(difftime(event_end_times, event_start_times, units = "secs"))
            valid_durations <- durations_seconds > 0
            if (sum(valid_durations) > 0) {
              channel_events <- data.table(StartTime = event_start_times[valid_durations], ChannelID = j, Duration = durations_seconds[valid_durations])
              if(nrow(channel_events) > 0) { file4_list[[length(file4_list) + 1]] <- channel_events }
            } } } } } # End channel loop
    if (length(file4_list) > 0) { file4_data <- data.table::rbindlist(file4_list) %>% dplyr::arrange(StartTime) }
    else { file4_data <- data.table(StartTime=as.POSIXct(character()), ChannelID=integer(), Duration=numeric()) ; message("      WARNING: No beambreak events detected for File 4.") }
    file4_output_filename <- file.path(target_sub_dir, paste0(base_output_name, "_File4.csv"))
    message(paste("      Saving:", basename(file4_output_filename)))
    data.table::fwrite(file4_data, file4_output_filename, dateTimeAs = "write.csv")
    
    # == Generate File 5 ==
    message("    Generating File 5...")
    if (nrow(file4_data) > 0) {
      file5_data <- copy(file4_data)[, Label := CHANNEL_LABELS[ChannelID + 1]][, .(StartTime, Label, Duration)]
    } else {
      file5_data <- data.table(StartTime=as.POSIXct(character()), Label=character(), Duration=numeric())
      message("      WARNING: File 4 was empty, File 5 will also be empty.")
    }
    file5_output_filename <- file.path(target_sub_dir, paste0(base_output_name, "_File5.csv"))
    message(paste("      Saving:", basename(file5_output_filename)))
    data.table::fwrite(file5_data, file5_output_filename, dateTimeAs = "write.csv")
    
    # == Generate Files 6, 7, 8 ==
    if (nrow(file5_data) > 0) {
      message("    Generating Files 6, 7, 8 (Binned Data)...")
      bin_widths <- c("1 min", "1 hour", "1 day"); file_numbers <- c(6, 7, 8)
      for(i in 1:length(bin_widths)){
        binned_data <- create_binned_data(as.data.frame(file5_data), bin_width = bin_widths[i]) # Pass data.frame if helper needs it
        output_filename <- file.path(target_sub_dir, paste0(base_output_name, "_File", file_numbers[i], ".csv"))
        message(paste("      Saving:", basename(output_filename)))
        data.table::fwrite(binned_data, output_filename, dateTimeAs = "write.csv")
      }
    } else {
      message("      SKIPPING Files 6, 7, 8 because File 5 was empty.")
    }
    
    # Log successful processing of file with data
    if (!OVERRIDE_LOG) {
      message("  Successfully processed data. Logging file.")
      tryCatch( write(input_csv_path, file = processed_log_file, append = TRUE),
                error = function(e) {warning("Could not write to processed log file: ", processed_log_file)} )
    } else {
      message("  Successfully processed data (Override Mode - log file not updated).")
    }
    
    # Collect Metadata for Manifest (only if data processing occurred)
    if(!is.na(rat_id) && !is.na(exp_id) && !is.na(cohort_id) && !is.na(date_obj)) {
      current_run_file_metadata <- list()
      output_files_to_log <- list(
        list(type = "WheelMovement", filename = wheel_output_filename),
        list(type = "File3", filename = file3_output_filename),
        list(type = "File6", filename = file.path(target_sub_dir, paste0(base_output_name, "_File6.csv"))),
        list(type = "File7", filename = file.path(target_sub_dir, paste0(base_output_name, "_File7.csv"))),
        list(type = "File8", filename = file.path(target_sub_dir, paste0(base_output_name, "_File8.csv")))
      )
      for (file_info in output_files_to_log) {
        if (file.exists(file_info$filename)) {
          normalized_output_path <- normalizePath(file_info$filename, winslash = "/", mustWork = FALSE)
          current_run_file_metadata[[length(current_run_file_metadata) + 1]] <- data.table(
            file_path = normalized_output_path, experiment_id = exp_id, cohort_id = cohort_id,
            rat_id = rat_id, date = date_obj, time_str = time_str, file_type = file_info$type)
        } else { message(paste("    Output file not found, skipping metadata:", basename(file_info$filename))) }
      }
      if(length(current_run_file_metadata) > 0){ all_metadata_this_run <- c(all_metadata_this_run, current_run_file_metadata) ; message(paste("  Collected metadata for", length(current_run_file_metadata), "output files for manifest.")) }
      else { message("  No output files found/logged for manifest for this input file.") }
    } else { message("  Skipping metadata collection due to earlier parsing error.") }
    
    file_processed_flag <- TRUE # Set flag for successful data processing
    
    
  }, error = function(e) {
    # --- ERROR Handling for this file ---
    files_error_count <- files_error_count + 1 # Increment error counter
    message(paste("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"))
    message(paste("ERROR processing file:", base_filename))
    message(paste("Error message:", e$message))
    message("Stack trace (limited):")
    try(message(paste(capture.output(traceback(1)), collapse = "\n")), silent = TRUE)
    message(paste("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"))
    # Do NOT log file as processed in processed_files.log if an error occurred
    # Do NOT add metadata to manifest if an error occurred
  }) # End tryCatch wrapper for single file processing
  
  # Increment the main processed counter if the file was handled (either processed with data or skipped as empty)
  if(file_processed_flag) {
    files_processed_this_run <- files_processed_this_run + 1
  }
  
} # End loop through input files


# --- Update Manifest File ---
# ... (Manifest update logic remains the same as provided previously) ...
message("\n========================================================")
message("--- Updating Manifest File ---")
message(paste("Manifest file path:", manifest_file_path))
if (length(all_metadata_this_run) > 0) {
  new_manifest_data <- rbindlist(all_metadata_this_run)
  new_manifest_data <- na.omit(new_manifest_data, cols=c("file_path", "experiment_id", "cohort_id", "rat_id", "date", "file_type"))
  message(paste("Collected metadata for", nrow(new_manifest_data), "valid output files in this run."))
} else {
  new_manifest_data <- data.table()
  message("No new valid metadata collected in this run.")
}
final_manifest_data <- data.table()
tryCatch({
  if (OVERRIDE_LOG || !file.exists(manifest_file_path)) {
    message(ifelse(OVERRIDE_LOG, "OVERRIDE_LOG is TRUE. Creating new manifest from this run's processed files.",
                   "Manifest file does not exist. Creating new manifest from this run's processed files."))
    final_manifest_data <- new_manifest_data
  } else {
    message("Manifest file exists. Reading existing manifest...")
    existing_manifest_data <- readRDS(manifest_file_path)
    required_manifest_cols <- c("file_path", "experiment_id", "cohort_id", "rat_id", "date", "file_type")
    if (!is.data.table(existing_manifest_data) || !all(required_manifest_cols %in% names(existing_manifest_data))) {
      existing_manifest_data <- data.table()
      warning("Existing manifest file was invalid or missing required columns. Manifest will be built only from this run's data combined with potentially empty existing data.")
    }
    message(paste("Read", nrow(existing_manifest_data), "entries from existing manifest."))
    unique_new_file_paths <- unique(new_manifest_data$file_path)
    if (nrow(existing_manifest_data) > 0 && length(unique_new_file_paths) > 0 && "file_path" %in% names(existing_manifest_data)) {
      rows_to_keep <- !(existing_manifest_data$file_path %in% unique_new_file_paths)
      updated_existing_manifest <- existing_manifest_data[rows_to_keep, ]
      removed_count <- nrow(existing_manifest_data) - nrow(updated_existing_manifest)
      if(removed_count > 0) { message(paste("Removed", removed_count, "old manifest entries for files processed/overwritten in this run.")) }
    } else { updated_existing_manifest <- existing_manifest_data }
    final_manifest_data <- rbindlist(list(updated_existing_manifest, new_manifest_data), use.names = TRUE, fill = TRUE)
  }
  core_columns <- c("file_path", "experiment_id", "cohort_id", "rat_id", "date", "time_str", "file_type")
  final_manifest_data <- final_manifest_data[, intersect(core_columns, names(final_manifest_data)), with = FALSE]
  if (nrow(final_manifest_data) > 0 && "file_path" %in% names(final_manifest_data)) {
    final_manifest_data <- final_manifest_data[!is.na(file_path) & file_path != "" & !is.na(experiment_id) & !is.na(cohort_id) & !is.na(rat_id) & !is.na(date) & !is.na(file_type)]
    final_manifest_data <- unique(final_manifest_data, by = "file_path")
  } else { final_manifest_data <- data.table() }
  message(paste("Saving updated manifest with", nrow(final_manifest_data), "entries..."))
  saveRDS(final_manifest_data, manifest_file_path)
  message("Manifest file successfully updated.")
}, error = function(e) {
  warning(paste("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"))
  warning(paste("CRITICAL ERROR updating manifest file:", manifest_file_path))
  warning("The Shiny app may not function correctly or show updated data.")
  warning(paste("Error message:", e$message))
  warning(paste("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"))
})


# --- Script Completion ---
end_time <- Sys.time()
duration <- end_time - start_time

message("\n========================================================")
message("RBB data processing finished.")
message(paste("  Total time:", format(duration)))
message(paste("  Files handled (processed data or empty):", files_processed_this_run)) # Updated counter description
message(paste("  Files skipped (already processed in log):", files_skipped_count))
message(paste("  Files skipped (empty w/ only header):", files_skipped_empty_count)) # Added specific counter
message(paste("  Files with processing errors:", files_error_count))
message(paste("Output files saved in subdirectories under:", main_output_dir))
message(paste("Processed input file log:", processed_log_file))
message(paste("Output file manifest:", manifest_file_path))
if (files_error_count > 0) {
  message(paste("!! WARNING:", files_error_count, "file(s) encountered errors during processing. Check messages above."))
}
if (files_skipped_empty_count > 0) {
  message(paste("NOTE:", files_skipped_empty_count, "input file(s) contained only headers and were skipped (no output generated)."))
}
message("========================================================")