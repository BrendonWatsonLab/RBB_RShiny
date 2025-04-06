# ============================================================================
# Script: RBB_LocalAnalysis_V3.R
# Author: Noah Muscat, Simeone Marino
# Date: 2025-04-05 # Updated Date
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
#   **MANIFEST FILE:** Generates and updates a "processed_manifest.rds" file
#   in the main output directory, containing metadata about successfully
#   generated output files. This is used by the Shiny app for faster file discovery.
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
# List to store metadata of output files generated in THIS run
all_metadata_this_run <- list()
# Define manifest file path (using main_output_dir)
manifest_file_path <- file.path(main_output_dir, "processed_manifest.rds")
# <<< END ADDED Initialization >>>

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
# Filter more specifically to match expected RBB structure
# Regex expects: experiment_XX/cohort_XX/RBB<2-digit-RatID>_<YYYYMMDD>_<HHMMSS>.csv
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

for (input_csv_path in input_files) {
  
  base_filename <- basename(input_csv_path)
  
  # Check if file was already processed (skip only if OVERRIDE_LOG is FALSE)
  if (!OVERRIDE_LOG && (input_csv_path %in% processed_files_list)) {
    files_skipped_count <- files_skipped_count + 1
    # message(paste("Skipping (already processed):", base_filename)) # Optional verbose skipping message
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
    relative_path <- sub(paste0("^", gsub("\\\\", "/", normalized_root), "/?"), "", gsub("\\\\", "/", file_dir)) # Use gsub for cross-platform separator consistency
    target_sub_dir <- file.path(main_output_dir, relative_path)
    
    # Create output subdirectory if needed
    if (!dir.exists(target_sub_dir)) {
      message(paste("  Creating output subdirectory:", target_sub_dir))
      dir.create(target_sub_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Base name for output files
    base_output_name <- tools::file_path_sans_ext(base_filename)
    
    # <<< ADDED: Extract Metadata Identifiers from path/filename >>>
    rat_id <- NA; date_str <- NA; time_str <- NA; exp_id <- NA; cohort_id <- NA; date_obj <- as.Date(NA) # Initialize NAs
    # Regex for RBB filename: RBB<RatID>_<YYYYMMDD>_<HHMMSS>
    base_name_pattern <- "^(RBB\\d{2})_(\\d{8})_(\\d{6})$"
    # Regex for path components
    path_pattern <- ".*/(experiment_(\\d{2}))/(cohort_(\\d{2}))/.*"
    
    base_match <- stringr::str_match(base_output_name, base_name_pattern)
    path_match <- stringr::str_match(gsub("\\\\", "/", input_csv_path), path_pattern) # Ensure forward slashes
    
    if (!anyNA(base_match) && NCOL(base_match) >= 4 && !anyNA(path_match) && NCOL(path_match) >= 5) {
      rat_id <- base_match[1, 2]
      date_str <- base_match[1, 3]
      time_str <- base_match[1, 4]
      exp_id <- path_match[1, 3] # Experiment number (e.g., "01")
      cohort_id <- path_match[1, 5] # Cohort number (e.g., "01")
      date_obj <- lubridate::ymd(date_str, quiet = TRUE) # Ensure date is Date object
      message(paste("  Extracted Metadata: Exp=", exp_id, " Cohort=", cohort_id, " Rat=", rat_id, " Date=", date_str))
    } else {
      warning(paste("Could not parse Experiment/Cohort/Rat/Date from path/filename:", input_csv_path, " - Metadata will be skipped for this file."))
      # Keep NAs initialized above
    }
    # <<< END ADDED METADATA EXTRACTION >>>
    
    # Read Input CSV Data
    message("  Reading data...")
    raw_data <- data.table::fread(input_csv_path, showProgress = FALSE)
    required_cols <- c("POSIX", "Digital Pins", "Wheel Analog")
    if (!all(required_cols %in% names(raw_data))) {
      missing_cols <- setdiff(required_cols, names(raw_data))
      stop(paste("Input file missing required columns:", paste(missing_cols, collapse=", ")))
    }
    # Keep as data.table if possible, otherwise convert if helpers need data.frame
    # raw_data_df <- as.data.frame(raw_data) # Example if conversion needed
    
    
    # Prepare Timestamps
    message("  Preparing timestamps...")
    # Check if POSIX is integer64, convert if necessary before division
    if (inherits(raw_data$POSIX, "integer64")) {
      timestamps_posixct <- as.POSIXct(as.double(raw_data$POSIX) / 1e6, origin = "1970-01-01", tz = TIMEZONE)
    } else {
      timestamps_posixct <- as.POSIXct(raw_data$POSIX / 1e6, origin = "1970-01-01", tz = TIMEZONE)
    }
    
    
    # --- Process Wheel Data ---
    message("  Processing wheel data...")
    wheel_voltages <- raw_data$`Wheel Analog`
    wheel_movement <- calculate_wheel_movement_wrap( wheel_voltages, WHEEL_VOLTAGE_MAX, WHEEL_VOLTAGE_THRESHOLD )
    wheel_output <- data.table(Timestamp = timestamps_posixct, Wheel_Movement = wheel_movement) # Use data.table
    wheel_output_filename <- file.path(target_sub_dir, paste0(base_output_name, "_WheelMovement.csv"))
    message(paste("    Saving:", basename(wheel_output_filename)))
    data.table::fwrite(wheel_output, wheel_output_filename, dateTimeAs = "write.csv") # Use fwrite for consistency, ensure timestamp format
    
    
    # --- Process Beambreak Data ---
    message("  Processing beambreak data...")
    
    # == Generate File 3: Raw Binary States ==
    message("    Generating File 3...")
    digital_pins_raw <- raw_data$`Digital Pins`
    # Handle potential integer64 for digital pins as well
    if (inherits(digital_pins_raw, "integer64")) digital_pins_raw <- as.integer(digital_pins_raw) # Assuming pins fit in standard integer
    
    binary_matrix <- t(sapply(digital_pins_raw, number2binary, noBits = 8))
    inverted_binary_matrix <- ifelse(binary_matrix == 0, 1, 0) # 1 = Event/Break
    file3_data <- data.table(Timestamp = timestamps_posixct, DigitalPins_Raw = digital_pins_raw) # Use data.table
    colnames(inverted_binary_matrix) <- paste0("Channel_", 0:7)
    file3_data <- cbind(file3_data, inverted_binary_matrix)
    file3_output_filename <- file.path(target_sub_dir, paste0(base_output_name, "_File3.csv"))
    message(paste("      Saving:", basename(file3_output_filename)))
    data.table::fwrite(file3_data, file3_output_filename, dateTimeAs = "write.csv") # Use fwrite
    
    # == Generate File 4: Event List ==
    message("    Generating File 4 (Detecting Events)...")
    file4_list <- list()
    for (j in 0:7) {
      channel_vector <- file3_data[[paste0("Channel_", j)]]
      starts <- which(diff(c(0, channel_vector)) == 1)
      ends <- which(diff(c(channel_vector, 0)) == -1)
      if (length(starts) > 0 && length(ends) > 0) {
        # Ensure starts precede ends reasonably
        if(starts[1] > ends[1]) ends <- ends[-1] # Remove initial end if it precedes first start
        if(length(starts) == 0 || length(ends) == 0) next
        if(ends[length(ends)] < starts[length(starts)]) starts <- starts[-length(starts)] # Remove trailing start if no corresponding end
        if(length(starts) == 0 || length(ends) == 0) next
        
        n_events <- min(length(starts), length(ends))
        if(n_events > 0) {
          start_indices <- starts[1:n_events]; end_indices <- ends[1:n_events]
          # Ensure end index is actually after start index for each pair
          valid_pair_indices <- which(end_indices >= start_indices)
          if(length(valid_pair_indices) > 0) {
            start_indices <- start_indices[valid_pair_indices]; end_indices <- end_indices[valid_pair_indices]
            event_start_times <- timestamps_posixct[start_indices]; event_end_times <- timestamps_posixct[end_indices]
            durations_seconds <- as.numeric(difftime(event_end_times, event_start_times, units = "secs"))
            valid_durations <- durations_seconds > 0 # Only keep events with positive duration
            
            if (sum(valid_durations) > 0) {
              channel_events <- data.table( # Use data.table
                StartTime = event_start_times[valid_durations],
                ChannelID = j,
                Duration = durations_seconds[valid_durations]
              )
              if(nrow(channel_events) > 0) { file4_list[[length(file4_list) + 1]] <- channel_events }
            }
          }
        }
      }
    } # End channel loop
    
    if (length(file4_list) > 0) {
      file4_data <- data.table::rbindlist(file4_list) %>% dplyr::arrange(StartTime)
    } else {
      file4_data <- data.table(StartTime=as.POSIXct(character()), ChannelID=integer(), Duration=numeric()) # Use data.table
      message("      WARNING: No beambreak events detected for File 4.")
    }
    file4_output_filename <- file.path(target_sub_dir, paste0(base_output_name, "_File4.csv"))
    message(paste("      Saving:", basename(file4_output_filename)))
    data.table::fwrite(file4_data, file4_output_filename, dateTimeAs = "write.csv") # Use fwrite
    
    
    # == Generate File 5: Labeled Events ==
    message("    Generating File 5...")
    if (nrow(file4_data) > 0) {
      # Use data.table syntax for efficiency
      file5_data <- copy(file4_data)[, Label := CHANNEL_LABELS[ChannelID + 1]][, .(StartTime, Label, Duration)]
    } else {
      file5_data <- data.table(StartTime=as.POSIXct(character()), Label=character(), Duration=numeric()) # Use data.table
      message("      WARNING: File 4 was empty, File 5 will also be empty.")
    }
    file5_output_filename <- file.path(target_sub_dir, paste0(base_output_name, "_File5.csv"))
    message(paste("      Saving:", basename(file5_output_filename)))
    data.table::fwrite(file5_data, file5_output_filename, dateTimeAs = "write.csv") # Use fwrite
    
    
    # == Generate Files 6, 7, 8: Binned Data ==
    if (nrow(file5_data) > 0) {
      message("    Generating Files 6, 7, 8 (Binned Data)...")
      bin_widths <- c("1 min", "1 hour", "1 day"); file_numbers <- c(6, 7, 8)
      for(i in 1:length(bin_widths)){
        # Need to pass file5_data as data.frame to helper if it expects it
        binned_data <- create_binned_data(as.data.frame(file5_data), bin_width = bin_widths[i])
        output_filename <- file.path(target_sub_dir, paste0(base_output_name, "_File", file_numbers[i], ".csv"))
        message(paste("      Saving:", basename(output_filename)))
        # Use fwrite for binned data as well for consistency
        data.table::fwrite(binned_data, output_filename, dateTimeAs = "write.csv")
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
    
    # <<< ADDED: Collect Metadata for Manifest >>>
    if(!is.na(rat_id) && !is.na(exp_id) && !is.na(cohort_id) && !is.na(date_obj)) { # Only collect if parsing was successful
      current_run_file_metadata <- list()
      # Define which output files are relevant for the Shiny app's manifest
      output_files_to_log <- list(
        list(type = "WheelMovement", suffix = "_WheelMovement.csv", filename = wheel_output_filename),
        list(type = "File3", suffix = "_File3.csv", filename = file3_output_filename),
        # list(type = "File4", suffix = "_File4.csv"), # Not directly used by Shiny currently
        # list(type = "File5", suffix = "_File5.csv"), # Not directly used by Shiny currently
        list(type = "File6", suffix = "_File6.csv", filename = file.path(target_sub_dir, paste0(base_output_name, "_File6.csv"))),
        list(type = "File7", suffix = "_File7.csv", filename = file.path(target_sub_dir, paste0(base_output_name, "_File7.csv"))), # Loaded but not used in UI - keep for now
        list(type = "File8", suffix = "_File8.csv", filename = file.path(target_sub_dir, paste0(base_output_name, "_File8.csv")))
      )
      
      for (file_info in output_files_to_log) {
        output_filename_to_check <- file_info$filename
        # Check if the file actually exists after processing attempt
        if (file.exists(output_filename_to_check)) {
          # Use normalizePath to get a canonical path, helpful for matching later
          normalized_output_path <- normalizePath(output_filename_to_check, winslash = "/", mustWork = FALSE)
          
          current_run_file_metadata[[length(current_run_file_metadata) + 1]] <- data.table(
            file_path = normalized_output_path,
            experiment_id = exp_id,
            cohort_id = cohort_id,
            rat_id = rat_id,
            date = date_obj, # Use the Date object
            time_str = time_str, # Include time string if needed for uniqueness/sorting
            file_type = file_info$type
          )
        } else {
          message(paste("    Output file not found, skipping metadata:", basename(output_filename_to_check)))
        }
      }
      if(length(current_run_file_metadata) > 0){
        all_metadata_this_run <- c(all_metadata_this_run, current_run_file_metadata)
        message(paste("  Collected metadata for", length(current_run_file_metadata), "output files for manifest."))
      } else {
        message("  No output files found/logged for manifest for this input file.")
      }
    } else {
      message("  Skipping metadata collection due to earlier parsing error.")
    }
    # <<< END ADDED METADATA COLLECTION >>>
    
    
    files_processed_this_run <- files_processed_this_run + 1
    
    
  }, error = function(e) {
    # --- ERROR Handling for this file ---
    files_error_count <- files_error_count + 1
    message(paste("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"))
    message(paste("ERROR processing file:", base_filename))
    message(paste("Error message:", e$message))
    message("Stack trace:")
    try(message(paste(capture.output(traceback(1)), collapse = "\n")), silent = TRUE) # Get limited traceback
    message(paste("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"))
    # Consider adding failed file path to a separate error log
    # error_log_file <- file.path(main_output_dir, "ERROR_FILES.log")
    # tryCatch( write(input_csv_path, file = error_log_file, append = TRUE), error = function(e2){} )
    
  }) # End tryCatch wrapper for single file processing
  
} # End loop through input files


# <<< ADDED: Manifest Update Logic >>>
message("\n========================================================")
message("--- Updating Manifest File ---")
message(paste("Manifest file path:", manifest_file_path))

# Combine metadata collected during this specific run execution into one data.table
if (length(all_metadata_this_run) > 0) {
  new_manifest_data <- rbindlist(all_metadata_this_run)
  # Remove any entries where key info might be NA due to parsing errors during the loop
  new_manifest_data <- na.omit(new_manifest_data, cols=c("file_path", "experiment_id", "cohort_id", "rat_id", "date", "file_type"))
  message(paste("Collected metadata for", nrow(new_manifest_data), "valid output files in this run."))
} else {
  new_manifest_data <- data.table() # Empty data.table if no files processed or parsing failed
  message("No new valid metadata collected in this run.")
}

final_manifest_data <- data.table()

tryCatch({
  if (OVERRIDE_LOG || !file.exists(manifest_file_path)) {
    # If overriding or manifest doesn't exist, the new data is the final data
    message(ifelse(OVERRIDE_LOG, "OVERRIDE_LOG is TRUE. Creating new manifest from this run's processed files.",
                   "Manifest file does not exist. Creating new manifest from this run's processed files."))
    final_manifest_data <- new_manifest_data
    
  } else {
    # If not overriding and manifest exists, read old, remove entries for files processed now, add new entries
    message("Manifest file exists. Reading existing manifest...")
    existing_manifest_data <- readRDS(manifest_file_path)
    
    # Basic validation
    required_manifest_cols <- c("file_path", "experiment_id", "cohort_id", "rat_id", "date", "file_type")
    if (!is.data.table(existing_manifest_data) || !all(required_manifest_cols %in% names(existing_manifest_data))) {
      existing_manifest_data <- data.table() # Create empty if invalid
      warning("Existing manifest file was invalid or missing required columns. Manifest will be built only from this run's data combined with potentially empty existing data.")
    }
    message(paste("Read", nrow(existing_manifest_data), "entries from existing manifest."))
    
    # Identify unique output file paths processed/overwritten in this run
    unique_new_file_paths <- unique(new_manifest_data$file_path)
    
    # Remove old entries corresponding to these specific file paths from the existing data
    if (nrow(existing_manifest_data) > 0 && length(unique_new_file_paths) > 0 && "file_path" %in% names(existing_manifest_data)) {
      rows_to_keep <- !(existing_manifest_data$file_path %in% unique_new_file_paths)
      updated_existing_manifest <- existing_manifest_data[rows_to_keep, ]
      removed_count <- nrow(existing_manifest_data) - nrow(updated_existing_manifest)
      if(removed_count > 0) { message(paste("Removed", removed_count, "old manifest entries for files processed/overwritten in this run.")) }
    } else {
      updated_existing_manifest <- existing_manifest_data # No removal needed if no existing or no new paths or missing column
    }
    
    # Combine the updated old manifest with the new data
    # Use fill=TRUE in case columns ever mismatch slightly (e.g., if time_str included sometimes)
    final_manifest_data <- rbindlist(list(updated_existing_manifest, new_manifest_data), use.names = TRUE, fill = TRUE)
  }
  
  # Final cleanup: Ensure uniqueness based on file_path and remove rows with missing/empty file_path
  # Also select only the core columns expected by Shiny to avoid extra cols from fill=TRUE
  core_columns <- c("file_path", "experiment_id", "cohort_id", "rat_id", "date", "time_str", "file_type")
  final_manifest_data <- final_manifest_data[, intersect(core_columns, names(final_manifest_data)), with = FALSE] # Select only existing core columns
  
  
  if (nrow(final_manifest_data) > 0 && "file_path" %in% names(final_manifest_data)) {
    # Remove rows with missing essential info (should have been caught earlier, but good final check)
    final_manifest_data <- final_manifest_data[!is.na(file_path) & file_path != "" & !is.na(experiment_id) & !is.na(cohort_id) & !is.na(rat_id) & !is.na(date) & !is.na(file_type)]
    # Ensure unique file paths
    final_manifest_data <- unique(final_manifest_data, by = "file_path")
    
  } else {
    final_manifest_data <- data.table() # Ensure it's an empty data.table if no rows or file_path missing
  }
  
  
  # Save the final manifest using saveRDS (preferred for R objects)
  message(paste("Saving updated manifest with", nrow(final_manifest_data), "entries..."))
  saveRDS(final_manifest_data, manifest_file_path)
  message("Manifest file successfully updated.")
  
}, error = function(e) {
  # Enhanced error message for manifest update failure
  warning(paste("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"))
  warning(paste("CRITICAL ERROR updating manifest file:", manifest_file_path))
  warning("The Shiny app may not function correctly or show updated data.")
  warning(paste("Error message:", e$message))
  warning(paste("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"))
})
# <<< END ADDED MANIFEST UPDATE >>>


# --- Script Completion ---
end_time <- Sys.time()
duration <- end_time - start_time

message("\n========================================================")
message("RBB data processing finished.")
message(paste("  Total time:", format(duration)))
message(paste("  Files processed/reprocessed in this run:", files_processed_this_run))
message(paste("  Files skipped (already processed):", files_skipped_count))
message(paste("  Files with errors during processing:", files_error_count))
message(paste("Output files saved in subdirectories under:", main_output_dir))
message(paste("Processed input file log:", processed_log_file))
message(paste("Output file manifest:", manifest_file_path)) # Added manifest path to summary
if (files_error_count > 0) {
  message(paste("!! WARNING:", files_error_count, "file(s) encountered errors during processing. Check messages above or error log if implemented."))
}
message("========================================================")