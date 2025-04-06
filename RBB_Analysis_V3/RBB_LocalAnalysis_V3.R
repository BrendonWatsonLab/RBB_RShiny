# ============================================================================
# Script: RBB_LocalAnalysis_V3.R # Renamed for clarity
# Author: Noah Muscat, Simeone Marino
# Date: 2025-04-06
# R Version: 4.x
# Description:
#   Processes raw behavioral data from RBB experiments. Iterates through
#   experiment/cohort subdirectories, reads individual .csv files, performs
#   analysis for wheel movement and beambreak events. Handles input files
#   containing only headers. Generates processed files including File 5
#   (Labeled Events), File 6, 7, 8 (Binned Data), and WheelMovement.
#
#   **INCREMENTAL PROCESSING:** Maintains a log file ('processed_files.log')
#   to track successfully processed/handled files and only processes new
#   files by default. Set OVERRIDE_LOG = TRUE to force reprocessing.
#
#   **MANIFEST FILE:** Generates and updates 'processed_manifest.rds'
#   in the main output directory. This manifest contains metadata about
#   successfully generated output files (File 5, 6, 7, 8, WheelMovement)
#   and is used by the associated Shiny app for faster file discovery.
#
#   **MIRRORED OUTPUT STRUCTURE:** Recreates the relative input directory
#   structure within the main output directory.
#
# Dependencies: data.table, dplyr, tidyr, lubridate, stringr
#
# Script Inputs and Outputs ---
#
# INPUTS:
#   - Configuration Parameters (defined below in script):
#     - root_data_dir: Path to raw data (e.g., /path/to/RBB_Data)
#     - main_output_dir: Path for processed data (e.g., /path/to/RBB_Data_Cleaned)
#     - processed_log_file: Path to log of processed input files (in main_output_dir)
#     - OVERRIDE_LOG: TRUE/FALSE for incremental processing control.
#     - WHEEL_VOLTAGE_MAX, WHEEL_VOLTAGE_THRESHOLD, TIMEZONE, CHANNEL_LABELS
#   - Raw Data Files:
#     - Location: Expected in root_data_dir/experiment_XX/cohort_XX/
#     - Naming: Expected RBB##_YYYYMMDD_HHMMSS.csv
#     - Format: CSV with columns including 'POSIX', 'Digital Pins', 'Wheel Analog'.
#   - Incremental Log File (Optional, if exists & OVERRIDE_LOG=FALSE):
#     - Location: Path from 'processed_log_file'.
#     - Content: List of absolute paths to previously processed input CSVs.
#     - Purpose: To skip reprocessing already handled files.
#   - Existing Manifest File (Optional, if exists & OVERRIDE_LOG=FALSE):
#     - Location: 'processed_manifest.rds' in main_output_dir.
#     - Content: RDS file with data.table of previous output file metadata.
#     - Purpose: Base for incremental update of the manifest.

# OUTPUTS:
#   - Processed Data Files:
#     - Location: In main_output_dir/experiment_XX/cohort_XX/
#     - Format: CSV files saved via data.table::fwrite().
#     - Types (per input file with data):
#       - *_WheelMovement.csv (Timestamp, Wheel_Movement) - Logged in manifest
#       - *_File3.csv (Raw digital states - NOTE: Not logged in manifest)
#       - *_File4.csv (Raw events list - NOTE: Not logged in manifest)
#       - *_File5.csv (Labeled events - Logged in manifest)
#       - *_File6.csv (1-min binned data - Logged in manifest)
#       - *_File7.csv (1-hour binned data - Logged in manifest)
#       - *_File8.csv (1-day binned data - Logged in manifest)
#   - Updated Incremental Log File:
#     - Location: Path from 'processed_log_file'.
#     - Action: Appended with absolute paths of input files handled in this run
#               (unless OVERRIDE_LOG=TRUE).
#   - Updated Manifest File:
#     - Location: 'processed_manifest.rds' in main_output_dir.
#     - Action: Overwritten with latest metadata.
#     - Format: RDS file containing a single data.table.
#     - Content: Metadata (file_path, experiment_id, cohort_id, rat_id, date,
#                time_str, file_type) for relevant output files (File5, File6,
#                File7, File8, WheelMovement). Includes previous & new data
#                (unless OVERRIDE_LOG=TRUE). Saves empty but structured table
#                if no valid outputs found/generated.
#   - Console Output:
#     - Messages: Progress, warnings (empty files, parsing errors), summary.
#     - Errors: Details if processing fails for specific files or critical steps.
# ============================================================================

# --- User-Defined Parameters ---
root_data_dir <- "/nfs/turbo/umms-brendonw/RBB_Data"
main_output_dir <- "/nfs/turbo/umms-brendonw/RBB_Data_Cleaned"
processed_log_file <- file.path(main_output_dir, "processed_files.log")

# Set TRUE to re-analyze all found CSV files & regenerate manifest from scratch
# Set FALSE to only process new/unlogged files & update manifest incrementally
OVERRIDE_LOG <- FALSE

WHEEL_VOLTAGE_MAX <- 5.0 # Volts (User specified)
WHEEL_VOLTAGE_THRESHOLD <- 0.25
TIMEZONE <- "UTC"

# Ensure these labels match the Shiny App
CHANNEL_LABELS <- c(
  "Water_1_Beambreak", "Water_2_Beambreak", "Food_1_Beambreak",
  "Food_2_Beambreak", "Water_1_Dispense", "Water_2_Dispense",
  "Food_1_Dispense", "Food_2_Dispense"
)

# --- Load Required Libraries ---
message("Loading required packages...")
required_packages <- c("data.table", "dplyr", "tidyr", "lubridate", "stringr")
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
    # Pad with leading zeros if needed
    return(c(rep(0, noBits - length(binary_vector)), binary_vector))
  }
}

#' Calculate single step wheel movement (handles wrap-around & threshold)
calculate_single_step_movement <- function(v1, v2,
                                           v_max = WHEEL_VOLTAGE_MAX,
                                           v_threshold = WHEEL_VOLTAGE_THRESHOLD) {
  if (is.na(v1) || is.na(v2)) { return(0) }
  dist_simple <- abs(v1 - v2)
  dist_wrap <- v_max - max(v1, v2) + min(v1, v2)
  dist_actual <- min(dist_simple, dist_wrap)
  movement <- ifelse(dist_actual <= v_threshold, 0, dist_actual)
  return(movement)
}

#' Calculate wheel movement vector using voltage differences
calculate_wheel_movement_wrap <- function(wheel_voltages,
                                          voltage_max = WHEEL_VOLTAGE_MAX,
                                          voltage_threshold = WHEEL_VOLTAGE_THRESHOLD) {
  if (!is.numeric(wheel_voltages)) {
    stop("Input 'wheel_voltages' must be a numeric vector.")
  }
  n <- length(wheel_voltages)
  if (n < 2) { return(rep(0, n)) }
  
  movement_values <- numeric(n)
  movement_values[1] <- 0 # No movement at the first point
  
  for (i in 2:n) {
    movement_values[i] <- calculate_single_step_movement(
      v1 = wheel_voltages[i - 1],
      v2 = wheel_voltages[i],
      v_max = voltage_max,
      v_threshold = voltage_threshold
    )
  }
  return(movement_values)
}

#' Create binned summary data (Counts and Durations) using data.table
create_binned_data <- function(event_data, bin_width) {
  # --- Input Validation ---
  required_cols_binned <- c("StartTime", "Label", "Duration")
  if (!is.data.frame(event_data) ||
      !all(required_cols_binned %in% names(event_data))) {
    stop(
      paste("Input 'event_data' must be a data frame with columns:",
            paste(required_cols_binned, collapse=", "))
    )
  }
  if (!lubridate::is.POSIXct(event_data$StartTime)) {
    stop("'StartTime' column must be POSIXct.")
  }
  if (!is.numeric(event_data$Duration)) {
    stop("'Duration' column must be numeric (in seconds).")
  }
  
  # --- Setup ---
  all_labels <- unique(CHANNEL_LABELS)
  expected_count_cols <- paste("Count", all_labels, sep = "_")
  expected_duration_cols <- paste("TotalDuration", all_labels, sep = "_")
  expected_value_cols <- c(expected_count_cols, expected_duration_cols)
  final_ordered_cols <- c("BinStartTime", expected_value_cols)
  
  # --- Create Empty Output Structure (Helper) ---
  create_empty_binned_dt <- function() {
    empty_dt <- data.table::data.table(
      BinStartTime = as.POSIXct(character())
    )
    for (col in expected_value_cols) {
      col_type <- if(startsWith(col, "Count_")) integer(0) else numeric(0)
      empty_dt[, (col) := col_type]
    }
    try(data.table::setcolorder(empty_dt, neworder = final_ordered_cols),
        silent = TRUE)
    return(empty_dt)
  }
  
  # --- Handle Empty Input ---
  if (nrow(event_data) == 0) {
    message(
      paste("Input data for binning ('", bin_width, "') is empty.",
            "Returning empty data.table.", sep = "")
    )
    return(create_empty_binned_dt())
  }
  
  message(paste("  Binning data by:", bin_width, "using data.table"))
  
  # --- Convert to data.table and Aggregate ---
  event_dt <- data.table::as.data.table(event_data)
  event_dt <- event_dt[!is.na(StartTime)] # Remove rows with NA StartTime
  
  if (nrow(event_dt) == 0) {
    message("No valid StartTime entries found after NA removal for binning.")
    return(create_empty_binned_dt())
  }
  
  # Floor date and aggregate counts/durations
  event_dt[, BinStartTime := lubridate::floor_date(StartTime, unit = bin_width)]
  binned_dt <- event_dt[,
                        .(Count = .N, TotalDuration = sum(Duration, na.rm = TRUE)),
                        by = .(BinStartTime, Label)
  ]
  
  # --- Reshape to Wide Format ---
  binned_wide_dt <- tryCatch({
    data.table::dcast(
      binned_dt,
      BinStartTime ~ Label,
      value.var = c("Count", "TotalDuration"),
      fill = 0,
      sep = "_"
    )
  }, error = function(e) {
    message("!!! ERROR during data.table::dcast: ", e$message)
    return(data.table::data.table()) # Return empty table on dcast error
  })
  
  # --- Ensure All Expected Columns Exist ---
  current_columns <- names(binned_wide_dt)
  missing_cols <- setdiff(expected_value_cols, current_columns)
  
  if (length(missing_cols) > 0) {
    suppressWarnings({
      for (col in missing_cols) {
        # Assign correct type
        col_type <- if (startsWith(col, "Count_")) 0L else 0.0
        binned_wide_dt[, (col) := col_type]
      }
    })
  }
  
  # --- Final Structuring and Ordering ---
  binned_final <- NULL
  if ("BinStartTime" %in% names(binned_wide_dt) && nrow(binned_wide_dt) > 0) {
    # Keep only expected columns
    final_select_cols <- intersect(final_ordered_cols, names(binned_wide_dt))
    binned_wide_dt <- binned_wide_dt[, ..final_select_cols] # data.table subset
    
    # Ensure final column order and row order
    data.table::setcolorder(binned_wide_dt, neworder = final_select_cols)
    data.table::setorder(binned_wide_dt, BinStartTime)
    binned_final <- binned_wide_dt
  } else {
    # Handle cases where dcast failed or produced empty results
    message("    Result after dcast/column handling is empty. Creating empty structure.")
    binned_final <- create_empty_binned_dt()
  }
  
  return(binned_final)
} # End create_binned_data


# --- Main Processing Logic ---

message("Starting RBB data processing...")
start_time <- Sys.time()

# List to store metadata of output files generated in THIS run
all_metadata_this_run <- list()
manifest_file_path <- file.path(main_output_dir, "processed_manifest.rds")

# --- Read Log of Previously Processed Files (or Override) ---
processed_files_list <- character(0)
if (OVERRIDE_LOG) {
  message("OVERRIDE_LOG is TRUE. Ignoring processed files log.")
} else if (file.exists(processed_log_file)) {
  message(paste("Reading processed files log:", processed_log_file))
  tryCatch({
    processed_files_list <- unique(readLines(processed_log_file, warn = FALSE))
    processed_files_list <- processed_files_list[processed_files_list != ""] # Remove empty lines
    message(paste("  Found", length(processed_files_list), "files previously processed."))
  }, error = function(e) {
    warning(
      paste("Could not read processed files log:", processed_log_file,
            "\nError:", e$message, "\nProcessing all found files.")
    )
    processed_files_list <- character(0)
  })
} else {
  message("Processed files log not found. Will process all found files.")
}

# --- Find Input Files ---
message(paste("Searching for input CSV files in:", root_data_dir))
normalized_root <- normalizePath(root_data_dir, winslash = "/", mustWork = FALSE)
all_files <- list.files(
  path = normalized_root,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)
# Filter more specifically to match expected RBB structure
input_files_pattern <- "experiment_\\d{2}/cohort_\\d{2}/RBB\\d{2}_\\d{8}_\\d{6}\\.csv$"
input_files <- grep(input_files_pattern, all_files, value = TRUE, ignore.case = TRUE)

if (length(input_files) == 0) {
  stop(
    paste("No input CSV files found matching the pattern '", input_files_pattern,
          "' in:", root_data_dir)
  )
} else {
  message(
    paste("Found", length(input_files),
          "total input CSV files potentially needing processing.")
  )
}


# --- Process Files Incrementally ---
message("Processing files...")
files_processed_this_run <- 0
files_skipped_count <- 0
files_error_count <- 0
files_skipped_empty_count <- 0 # Counter for empty files

for (input_csv_path in input_files) {
  
  base_filename <- basename(input_csv_path)
  file_handled_this_iter <- FALSE # Flag if handled (empty or processed w/ data)
  
  # --- Check if Already Processed ---
  if (!OVERRIDE_LOG && (input_csv_path %in% processed_files_list)) {
    files_skipped_count <- files_skipped_count + 1
    next # Skip to the next file
  }
  
  # --- Announce Processing Target ---
  if (OVERRIDE_LOG && (input_csv_path %in% processed_files_list)) {
    message(paste("\nReprocessing previously processed file:", base_filename))
  } else {
    message(paste("\nProcessing NEW file:", base_filename))
  }
  
  # --- Process Single File (with error handling) ---
  tryCatch({
    
    # --- Determine Output Paths ---
    file_dir <- dirname(input_csv_path)
    # Ensure forward slashes for consistent relative path calculation
    normalized_file_dir <- gsub("\\\\", "/", file_dir)
    normalized_root_fwd <- gsub("\\\\", "/", normalized_root)
    relative_path <- sub(
      paste0("^", normalized_root_fwd, "/?"), "", normalized_file_dir
    )
    target_sub_dir <- file.path(main_output_dir, relative_path)
    
    # Create output subdirectory if needed
    if (!dir.exists(target_sub_dir)) {
      message(paste("  Creating output subdirectory:", target_sub_dir))
      dir.create(target_sub_dir, recursive = TRUE, showWarnings = FALSE)
    }
    base_output_name <- tools::file_path_sans_ext(base_filename)
    
    # --- Read Input CSV Data ---
    message("  Reading data...")
    raw_data <- data.table::fread(input_csv_path, showProgress = FALSE)
    
    # --- Handle Empty File Case (Header Only) ---
    if (nrow(raw_data) == 0) {
      warning(
        paste("Input file contains no data rows (only header):", base_filename)
      )
      files_skipped_empty_count <- files_skipped_empty_count + 1
      
      # Log as processed in main log file to avoid re-processing
      if (!OVERRIDE_LOG) {
        message("  Logging empty file as processed in log file.")
        tryCatch(
          write(input_csv_path, file = processed_log_file, append = TRUE),
          error = function(e) {
            warning("Could not write empty file to processed log file: ",
                    processed_log_file)
          }
        )
      }
      # Mark as handled for this iteration; the 'else' block is skipped
      file_handled_this_iter <- TRUE
      
      # --- Process File with Data Case ---
    } else {
      
      # --- Check Required Columns ---
      required_cols <- c("POSIX", "Digital Pins", "Wheel Analog")
      if (!all(required_cols %in% names(raw_data))) {
        missing_cols <- setdiff(required_cols, names(raw_data))
        stop(
          paste("Input file missing required columns:",
                paste(missing_cols, collapse = ", "))
        )
      }
      
      # --- Extract Metadata ---
      rat_id <- NA; date_str <- NA; time_str <- NA;
      exp_id <- NA; cohort_id <- NA; date_obj <- as.Date(NA)
      
      base_name_pattern <- "^(RBB\\d{2})_(\\d{8})_(\\d{6})$"
      path_pattern <- ".*/(experiment_(\\d{2}))/(cohort_(\\d{2}))/.*"
      normalized_input_path_for_regex <- gsub("\\\\", "/", input_csv_path)
      
      temp_base_match <- stringr::str_match(base_output_name, base_name_pattern)
      temp_path_match <- stringr::str_match(normalized_input_path_for_regex,
                                            path_pattern)
      
      # Check if both regex matches were successful
      if (!anyNA(temp_base_match) && NCOL(temp_base_match) >= 4 &&
          !anyNA(temp_path_match) && NCOL(temp_path_match) >= 5) {
        # Assign extracted values
        rat_id    <- temp_base_match[1, 2]
        date_str  <- temp_base_match[1, 3]
        time_str  <- temp_base_match[1, 4]
        exp_id    <- temp_path_match[1, 3]
        cohort_id <- temp_path_match[1, 5]
        date_obj  <- lubridate::ymd(date_str, quiet = TRUE)
        message(
          paste("  Extracted Metadata: Exp=", exp_id,
                " Cohort=", cohort_id, " Rat=", rat_id, " Date=", date_str)
        )
      } else {
        # Warn if parsing failed
        warning(
          paste("Could not parse Experiment/Cohort/Rat/Date from",
                "path/filename:", input_csv_path,
                " - Metadata will be skipped.")
        )
      }
      
      # --- Prepare Timestamps ---
      message("  Preparing timestamps...")
      if (inherits(raw_data$POSIX, "integer64")) {
        timestamps_posixct <- as.POSIXct(
          as.double(raw_data$POSIX) / 1e6,
          origin = "1970-01-01", tz = TIMEZONE
        )
      } else {
        timestamps_posixct <- as.POSIXct(
          raw_data$POSIX / 1e6,
          origin = "1970-01-01", tz = TIMEZONE
        )
      }
      
      # --- Process Wheel Data ---
      message("  Processing wheel data...")
      wheel_voltages <- raw_data$`Wheel Analog`
      wheel_movement <- calculate_wheel_movement_wrap(
        wheel_voltages, WHEEL_VOLTAGE_MAX, WHEEL_VOLTAGE_THRESHOLD
      )
      wheel_output <- data.table(
        Timestamp = timestamps_posixct,
        Wheel_Movement = wheel_movement
      )
      wheel_output_filename <- file.path(
        target_sub_dir,
        paste0(base_output_name, "_WheelMovement.csv")
      )
      message(paste("    Saving:", basename(wheel_output_filename)))
      data.table::fwrite(
        wheel_output,
        wheel_output_filename,
        dateTimeAs = "write.csv" # Use format readable by lubridate
      )
      
      # --- Process Beambreak Data ---
      message("  Processing beambreak data...")
      
      # == Generate File 3: Raw Binary States ==
      message("    Generating File 3...")
      digital_pins_raw <- raw_data$`Digital Pins`
      if (inherits(digital_pins_raw, "integer64")) {
        digital_pins_raw <- as.integer(digital_pins_raw)
      }
      binary_matrix <- t(sapply(digital_pins_raw, number2binary, noBits = 8))
      inverted_binary_matrix <- 1 - binary_matrix # Simpler inversion
      file3_data <- data.table(
        Timestamp = timestamps_posixct,
        DigitalPins_Raw = digital_pins_raw
      )
      colnames(inverted_binary_matrix) <- paste0("Channel_", 0:7)
      file3_data <- cbind(file3_data, inverted_binary_matrix)
      file3_output_filename <- file.path(
        target_sub_dir,
        paste0(base_output_name, "_File3.csv")
      )
      message(paste("      Saving:", basename(file3_output_filename)))
      data.table::fwrite(
        file3_data,
        file3_output_filename,
        dateTimeAs = "write.csv"
      )
      
      # == Generate File 4: Event List ==
      message("    Generating File 4 (Detecting Events)...")
      file4_list <- list()
      for (j in 0:7) {
        channel_vector <- file3_data[[paste0("Channel_", j)]]
        # Use run-length encoding to find start/end of consecutive 1s
        rle_result <- rle(channel_vector)
        event_indices <- which(rle_result$values == 1)
        
        if (length(event_indices) > 0) {
          event_lengths <- rle_result$lengths[event_indices]
          # Calculate cumulative sums to find end positions
          event_ends_cum <- cumsum(rle_result$lengths)[event_indices]
          # Calculate start positions
          event_starts <- event_ends_cum - event_lengths + 1
          
          # Get times and durations
          event_start_times <- timestamps_posixct[event_starts]
          # Use index before the end for duration calculation? No, use end index.
          event_end_times   <- timestamps_posixct[event_ends_cum]
          durations_seconds <- as.numeric(
            difftime(event_end_times, event_start_times, units = "secs")
          )
          
          # Add check for valid times and positive durations
          valid_events <- !is.na(event_start_times) &
            !is.na(event_end_times) &
            durations_seconds > 0
          
          if (any(valid_events)) {
            channel_events <- data.table(
              StartTime = event_start_times[valid_events],
              ChannelID = j,
              Duration  = durations_seconds[valid_events]
            )
            file4_list[[length(file4_list) + 1]] <- channel_events
          }
        }
      } # End channel loop (j)
      
      # Combine results for File 4
      if (length(file4_list) > 0) {
        file4_data <- data.table::rbindlist(file4_list)
        data.table::setorder(file4_data, StartTime) # Order events
      } else {
        file4_data <- data.table(StartTime = as.POSIXct(character()),
                                 ChannelID = integer(),
                                 Duration = numeric())
        message("      WARNING: No beambreak events detected for File 4.")
      }
      file4_output_filename <- file.path(
        target_sub_dir,
        paste0(base_output_name, "_File4.csv")
      )
      message(paste("      Saving:", basename(file4_output_filename)))
      data.table::fwrite(
        file4_data,
        file4_output_filename,
        dateTimeAs = "write.csv"
      )
      
      # == Generate File 5: Labeled Events ==
      message("    Generating File 5...")
      if (nrow(file4_data) > 0) {
        file5_data <- copy(file4_data)[, Label := CHANNEL_LABELS[ChannelID + 1]]
        # Select and reorder columns
        file5_data <- file5_data[, .(StartTime, Label, Duration)]
      } else {
        file5_data <- data.table(StartTime = as.POSIXct(character()),
                                 Label = character(),
                                 Duration = numeric())
        message("      WARNING: File 4 was empty, File 5 will also be empty.")
      }
      file5_output_filename <- file.path(
        target_sub_dir,
        paste0(base_output_name, "_File5.csv")
      )
      message(paste("      Saving:", basename(file5_output_filename)))
      data.table::fwrite(
        file5_data,
        file5_output_filename,
        dateTimeAs = "write.csv"
      )
      
      # == Generate Files 6, 7, 8: Binned Data ==
      if (nrow(file5_data) > 0) {
        message("    Generating Files 6, 7, 8 (Binned Data)...")
        bin_widths <- c("1 min", "1 hour", "1 day")
        file_numbers <- c(6, 7, 8)
        for (i in 1:length(bin_widths)) {
          binned_data <- create_binned_data(
            as.data.frame(file5_data), # Pass data.frame if helper expects it
            bin_width = bin_widths[i]
          )
          output_filename <- file.path(
            target_sub_dir,
            paste0(base_output_name, "_File", file_numbers[i], ".csv")
          )
          message(paste("      Saving:", basename(output_filename)))
          data.table::fwrite(
            binned_data,
            output_filename,
            dateTimeAs = "write.csv"
          )
        } # End loop through bin widths
      } else {
        message("      SKIPPING Files 6, 7, 8 because File 5 was empty.")
      }
      
      # --- Log successful processing in main log file ---
      if (!OVERRIDE_LOG) {
        message("  Successfully processed data. Logging file.")
        tryCatch(
          write(input_csv_path, file = processed_log_file, append = TRUE),
          error = function(e) {
            warning("Could not write success to processed log file: ",
                    processed_log_file)
          }
        )
      } else {
        message("  Successfully processed data (Override Mode - log file not updated).")
      }
      
      # --- Collect Metadata for Manifest ---
      # Only collect if metadata parsing was successful earlier
      if (!is.na(rat_id) && !is.na(exp_id) && !is.na(cohort_id) && !is.na(date_obj)) {
        current_run_file_metadata <- list()
        # Define which output files are relevant for the Shiny app's manifest
        # <<< CORRECTED LIST TO INCLUDE File5, REMOVE File3 >>>
        output_files_to_log <- list(
          list(type = "WheelMovement",
               filename = wheel_output_filename),
          list(type = "File5",
               filename = file5_output_filename),
          list(type = "File6",
               filename = file.path(target_sub_dir, paste0(base_output_name, "_File6.csv"))),
          list(type = "File7",
               filename = file.path(target_sub_dir, paste0(base_output_name, "_File7.csv"))),
          list(type = "File8",
               filename = file.path(target_sub_dir, paste0(base_output_name, "_File8.csv")))
        )
        # <<< END CORRECTION >>>
        
        files_logged_count = 0
        for (file_info in output_files_to_log) {
          # Check if the file actually exists after processing attempt
          if (file.exists(file_info$filename)) {
            # Use normalizePath for canonical paths
            normalized_output_path <- normalizePath(
              file_info$filename,
              winslash = "/",
              mustWork = FALSE
            )
            # Add metadata entry to list for this file
            current_run_file_metadata[[length(current_run_file_metadata) + 1]] <- data.table(
              file_path     = normalized_output_path,
              experiment_id = exp_id,
              cohort_id     = cohort_id,
              rat_id        = rat_id,
              date          = date_obj, # Use the Date object
              time_str      = time_str, # Include time string
              file_type     = file_info$type
            )
            files_logged_count <- files_logged_count + 1
          } else {
            # Message if expected file wasn't found
            message(
              paste("    Output file not found, skipping metadata:",
                    basename(file_info$filename))
            )
          }
        } # end loop through files to log
        
        if (files_logged_count > 0) {
          # Add all metadata collected for this input file to the run's list
          all_metadata_this_run <- c(all_metadata_this_run, current_run_file_metadata)
          message(
            paste("  Collected metadata for", files_logged_count,
                  "output files for manifest.")
          )
        } else {
          message("  No existing output files found to log for manifest for this input file.")
        }
      } else {
        # Message if metadata collection is skipped due to parsing error
        message("  Skipping metadata collection due to earlier parsing error.")
      }
      
      # --- Mark file as handled ---
      file_handled_this_iter <- TRUE
      
    } # --- End of 'else' block (processing files with data) ---
    
  }, error = function(e) {
    # --- ERROR Handling for this file ---
    files_error_count <- files_error_count + 1 # Increment error counter FIRST
    message(paste("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"))
    message(paste("ERROR processing file:", base_filename))
    message(paste("Error message:", e$message))
    message("Stack trace (limited):")
    # Provide limited traceback to avoid excessive logging
    try(message(paste(capture.output(traceback(1)), collapse = "\n")), silent = TRUE)
    message(paste("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"))
    # NOTE: file_handled_this_iter remains FALSE if an error occurs
  }) # End tryCatch wrapper for single file processing
  
  # Increment the main 'handled' counter if successful (data or empty)
  if (file_handled_this_iter) {
    files_processed_this_run <- files_processed_this_run + 1
  }
  
} # End main loop through input files


# ============================================================================
# --- Update Manifest File ---
# ============================================================================
message("\n========================================================")
message("--- Updating Manifest File ---")
message(paste("Manifest file path:", manifest_file_path))

# Combine metadata collected during this specific run execution
if (length(all_metadata_this_run) > 0) {
  new_manifest_data <- rbindlist(all_metadata_this_run)
  # Remove any entries where key info might be NA (e.g., due to parsing errors)
  required_manifest_cols_check <- c("file_path", "experiment_id", "cohort_id",
                                    "rat_id", "date", "file_type")
  new_manifest_data <- na.omit(
    new_manifest_data,
    cols = required_manifest_cols_check
  )
  message(
    paste("Collected metadata for", nrow(new_manifest_data),
          "valid output files in this run.")
  )
} else {
  # Create empty table if no new metadata collected
  new_manifest_data <- data.table()
  message("No new valid metadata collected in this run.")
}

final_manifest_data <- data.table()

tryCatch({
  # --- Handle OVERRIDE or Non-Existing Manifest ---
  if (OVERRIDE_LOG || !file.exists(manifest_file_path)) {
    action_msg <- ifelse(
      OVERRIDE_LOG,
      "OVERRIDE_LOG is TRUE. Creating new manifest from this run.",
      "Manifest file does not exist. Creating new manifest from this run."
    )
    message(action_msg)
    final_manifest_data <- new_manifest_data
    
    # --- Handle Incremental Update ---
  } else {
    message("Manifest file exists. Reading existing manifest...")
    existing_manifest_data <- readRDS(manifest_file_path)
    
    # Basic validation of existing manifest
    required_manifest_cols <- c("file_path", "experiment_id", "cohort_id",
                                "rat_id", "date", "file_type") # core cols
    if (!is.data.table(existing_manifest_data) ||
        !all(required_manifest_cols %in% names(existing_manifest_data))) {
      existing_manifest_data <- data.table() # Treat as empty if invalid
      warning(
        paste("Existing manifest file was invalid or missing required columns.",
              "Manifest will be built only from this run's data",
              "combined with potentially empty existing data.")
      )
    }
    message(
      paste("Read", nrow(existing_manifest_data), "entries from existing manifest.")
    )
    
    # Identify unique output file paths processed/overwritten in THIS run
    unique_new_file_paths <- unique(new_manifest_data$file_path)
    
    # Remove old entries corresponding to these specific files from existing data
    if (nrow(existing_manifest_data) > 0 &&
        length(unique_new_file_paths) > 0 &&
        "file_path" %in% names(existing_manifest_data)) {
      rows_to_keep <- !(existing_manifest_data$file_path %in% unique_new_file_paths)
      updated_existing_manifest <- existing_manifest_data[rows_to_keep, ]
      removed_count <- nrow(existing_manifest_data) - nrow(updated_existing_manifest)
      if (removed_count > 0) {
        message(
          paste("Removed", removed_count,
                "old manifest entries for files processed/overwritten in this run.")
        )
      }
    } else {
      # No removal needed if no existing data, no new data, or column missing
      updated_existing_manifest <- existing_manifest_data
    }
    
    # Combine the updated old manifest with the new data
    final_manifest_data <- rbindlist(
      list(updated_existing_manifest, new_manifest_data),
      use.names = TRUE,
      fill = TRUE # fill=TRUE handles potential column mismatches gracefully
    )
  } # End incremental update block
  
  # --- Final Manifest Cleanup ---
  # Define core columns expected by Shiny app + optional time_str
  core_columns <- c("file_path", "experiment_id", "cohort_id", "rat_id",
                    "date", "time_str", "file_type")
  # Select only existing core columns to avoid extras from fill=TRUE
  final_manifest_data <- final_manifest_data[,
                                             intersect(core_columns, names(final_manifest_data)),
                                             with = FALSE
  ]
  
  if (nrow(final_manifest_data) > 0 &&
      "file_path" %in% names(final_manifest_data)) {
    # Remove rows with missing essential info (final check)
    essential_cols_check <- c("file_path", "experiment_id", "cohort_id",
                              "rat_id", "date", "file_type")
    final_manifest_data <- final_manifest_data[
      !is.na(file_path) & file_path != "" &
        !is.na(experiment_id) & !is.na(cohort_id) &
        !is.na(rat_id) & !is.na(date) & !is.na(file_type)
    ]
    # Ensure unique file paths remain
    final_manifest_data <- unique(final_manifest_data, by = "file_path")
    
  } else {
    # Ensure it's an empty data.table if no rows or file_path missing
    final_manifest_data <- data.table()
  }
  
  # --- Save Empty Manifest with Structure (if needed) ---
  # Add check to ensure even an empty manifest has the correct columns for Shiny
  if (nrow(final_manifest_data) == 0) {
    message("Manifest data is empty. Saving empty manifest with defined columns.")
    # Define structure explicitly using the core columns expected by Shiny
    final_manifest_data <- data.table(
      file_path     = character(0),
      experiment_id = character(0),
      cohort_id     = character(0),
      rat_id        = character(0),
      date          = as.Date(character(0)),
      time_str      = character(0),
      file_type     = character(0)
    )[, intersect(core_columns, # Select only desired cols
                  c("file_path", "experiment_id", "cohort_id", "rat_id",
                    "date", "time_str", "file_type")), with = FALSE]
  }
  
  # --- Save Final Manifest ---
  message(
    paste("Saving updated manifest with", nrow(final_manifest_data), "entries...")
  )
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


# ============================================================================
# --- Script Completion Summary ---
# ============================================================================
end_time <- Sys.time()
duration <- end_time - start_time

message("\n========================================================")
message("RBB data processing finished.")
message(paste("  Total time:", format(duration)))
message(paste("  Files handled (processed data or empty):", files_processed_this_run))
message(paste("  Files skipped (already processed in log):", files_skipped_count))
message(paste("  Files skipped (empty w/ only header):", files_skipped_empty_count))
message(paste("  Files with processing errors:", files_error_count))
message(paste("Output files saved in subdirectories under:", main_output_dir))
message(paste("Processed input file log:", processed_log_file))
message(paste("Output file manifest:", manifest_file_path))
if (files_error_count > 0) {
  message(
    paste("!! WARNING:", files_error_count,
          "file(s) encountered errors during processing. Check messages above.")
  )
}
if (files_skipped_empty_count > 0) {
  message(
    paste("NOTE:", files_skipped_empty_count,
          "input file(s) contained only headers and were skipped (no output generated).")
  )
}
message("========================================================")