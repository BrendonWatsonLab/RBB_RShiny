# ============================================================================
# Script: RBB_LocalAnalysis.R
# Author: Noah Muscat, Simeone Marino
# Date: 2025-04-05
# R Version: 4.x
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
root_data_dir <- "/nfs/turbo/umms-brendonw/RBB_Data"
main_output_dir <- "/nfs/turbo/umms-brendonw/RBB_Data_Cleaned"
processed_log_file <- file.path(main_output_dir, "processed_files.log")
OVERRIDE_LOG <- FALSE
WHEEL_VOLTAGE_MAX <- 5.0 # User specified value
WHEEL_VOLTAGE_THRESHOLD <- 0.25
TIMEZONE <- "UTC"
CHANNEL_LABELS <- c( "Water_1_Beambreak", "Water_2_Beambreak", "Food_1_Beambreak", "Food_2_Beambreak", "Water_1_Dispense", "Water_2_Dispense", "Food_1_Dispense", "Food_2_Dispense" )

# --- Load Required Libraries ---
message("Loading required packages...")
required_packages <- c("data.table", "dplyr", "tidyr", "lubridate", "stringr")
for (pkg in required_packages) { if (!requireNamespace(pkg, quietly = TRUE)) { message(paste("Installing package:", pkg)); install.packages(pkg) }; library(pkg, character.only = TRUE) }
message("Packages loaded successfully.")

# --- Helper Functions ---
# ... (Helper functions are unchanged, using global WHEEL_VOLTAGE_MAX/THRESHOLD correctly) ...
number2binary <- function(number, noBits) { binary_vector <- rev(as.numeric(intToBits(number))); if (length(binary_vector) >= noBits) { return(binary_vector[-(1:(length(binary_vector) - noBits))]) } else { return(c(rep(0, noBits - length(binary_vector)), binary_vector)) } }
calculate_single_step_movement <- function(v1, v2, v_max = WHEEL_VOLTAGE_MAX, v_threshold = WHEEL_VOLTAGE_THRESHOLD) { if(is.na(v1) || is.na(v2)) { return(0) }; dist_simple <- abs(v1 - v2); dist_wrap <- v_max - max(v1, v2) + min(v1, v2); dist_actual <- min(dist_simple, dist_wrap); movement <- ifelse(dist_actual <= v_threshold, 0, dist_actual); return(movement) }
calculate_wheel_movement_wrap <- function(wheel_voltages, voltage_max = WHEEL_VOLTAGE_MAX, voltage_threshold = WHEEL_VOLTAGE_THRESHOLD) { if (!is.numeric(wheel_voltages)) { stop("Input 'wheel_voltages' must be a numeric vector.") }; n <- length(wheel_voltages); if (n < 2) { return(rep(0, n)) }; movement_values <- numeric(n); movement_values[1] <- 0; for (i in 2:n) { movement_values[i] <- calculate_single_step_movement( v1 = wheel_voltages[i - 1], v2 = wheel_voltages[i], v_max = voltage_max, v_threshold = voltage_threshold ) }; return(movement_values) }
create_binned_data <- function(event_data, bin_width) { if (!is.data.frame(event_data) || !all(c("StartTime", "Label", "Duration") %in% names(event_data))) { stop("Input 'event_data' must be a data frame with 'StartTime', 'Label', and 'Duration' columns.") }; if (!lubridate::is.POSIXct(event_data$StartTime)) { stop("'StartTime' column must be POSIXct.") }; if (!is.numeric(event_data$Duration)) { stop("'Duration' column must be numeric (in seconds).") }; all_labels <- unique(CHANNEL_LABELS); expected_count_cols <- paste("Count", all_labels, sep = "_"); expected_duration_cols <- paste("TotalDuration", all_labels, sep = "_"); expected_value_cols <- c(expected_count_cols, expected_duration_cols); final_ordered_cols <- c("BinStartTime", expected_value_cols); if (nrow(event_data) == 0) { message(paste("Input data for binning ('", bin_width, "') is empty. Returning empty data.table.", sep="")); empty_dt <- data.table::data.table(BinStartTime = as.POSIXct(character())); for (col in expected_value_cols) { empty_dt[, (col) := if(startsWith(col, "Count_")) integer(0) else numeric(0)] }; try(data.table::setcolorder(empty_dt, neworder = final_ordered_cols), silent=TRUE); return(empty_dt) }; message(paste("  Binning data by:", bin_width, "using data.table")); event_dt <- data.table::as.data.table(event_data); event_dt <- event_dt[!is.na(StartTime)]; if (nrow(event_dt) == 0) { message("No valid StartTime entries found after NA removal for binning."); empty_dt <- data.table::data.table(BinStartTime = as.POSIXct(character())); for (col in expected_value_cols) { empty_dt[, (col) := if(startsWith(col, "Count_")) integer(0) else numeric(0)] }; try(data.table::setcolorder(empty_dt, neworder = final_ordered_cols), silent=TRUE); return(empty_dt) }; event_dt[, BinStartTime := lubridate::floor_date(StartTime, unit = bin_width)]; binned_dt <- event_dt[, .( Count = .N, TotalDuration = sum(Duration, na.rm = TRUE) ), by = .(BinStartTime, Label)]; binned_wide_dt <- tryCatch({ data.table::dcast(binned_dt, BinStartTime ~ Label, value.var = c("Count", "TotalDuration"), fill = 0, sep = "_") }, error = function(e){ message("!!! ERROR during data.table::dcast: ", e$message); return(data.table::data.table()) }); current_columns <- names(binned_wide_dt); missing_cols <- setdiff(expected_value_cols, current_columns); if (length(missing_cols) > 0) { suppressWarnings({ for (col in missing_cols) { if (startsWith(col, "Count_")) { binned_wide_dt[, (col) := 0L] } else { binned_wide_dt[, (col) := 0.0] } } }) }; binned_final <- NULL; if("BinStartTime" %in% names(binned_wide_dt) && nrow(binned_wide_dt) > 0) { final_select_cols <- intersect(final_ordered_cols, names(binned_wide_dt)); extra_cols <- setdiff(names(binned_wide_dt), final_select_cols); if(length(extra_cols) > 0) { binned_wide_dt[, (extra_cols) := NULL] }; data.table::setcolorder(binned_wide_dt, neworder = final_select_cols); data.table::setorder(binned_wide_dt, BinStartTime); binned_final <- binned_wide_dt } else { message("     Result after dcast/adding columns is empty or missing BinStartTime. Creating empty structure."); empty_dt <- data.table::data.table(BinStartTime = as.POSIXct(character())); for (col in expected_value_cols) { empty_dt[, (col) := if(startsWith(col, "Count_")) integer(0) else numeric(0)] }; try(data.table::setcolorder(empty_dt, neworder = final_ordered_cols), silent=TRUE); binned_final <- empty_dt }; return(binned_final) }

# --- Main Processing Logic ---

message("Starting RBB data processing...")
start_time <- Sys.time()

all_metadata_this_run <- list()
manifest_file_path <- file.path(main_output_dir, "processed_manifest.rds")

# --- Read Log of Previously Processed Files (or Override) ---
# ... (Log reading is unchanged) ...
processed_files_list <- character(0)
if (OVERRIDE_LOG) { message("OVERRIDE_LOG is TRUE. Ignoring processed files log and reprocessing all found files.")
} else if (file.exists(processed_log_file)) { message(paste("Reading processed files log:", processed_log_file)); tryCatch({ processed_files_list <- unique(readLines(processed_log_file, warn = FALSE)); processed_files_list <- processed_files_list[processed_files_list != ""]; message(paste("  Found", length(processed_files_list), "files previously processed.")) }, error = function(e) { warning(paste("Could not read processed files log:", processed_log_file, "\nError:", e$message, "\nProcessing all found files.")); processed_files_list <- character(0) })
} else { message("Processed files log not found. Will process all found files.") }

# --- Find Input Files ---
# ... (File finding is unchanged) ...
message(paste("Searching for input CSV files in:", root_data_dir))
normalized_root <- normalizePath(root_data_dir, winslash = "/", mustWork = FALSE)
all_files <- list.files(path = normalized_root, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
input_files <- grep("experiment_\\d{2}/cohort_\\d{2}/RBB\\d{2}_\\d{8}_\\d{6}\\.csv$", all_files, value = TRUE, ignore.case = TRUE)
if (length(input_files) == 0) { stop(paste("No input CSV files found matching the expected pattern 'experiment_XX/cohort_XX/RBB##_YYYYMMDD_HHMMSS.csv' in:", root_data_dir)) } else { message(paste("Found", length(input_files), "total input CSV files potentially needing processing.")) }


# --- Process Files Incrementally ---
message("Processing files...")
files_processed_this_run <- 0
files_skipped_count <- 0
files_error_count <- 0
files_skipped_empty_count <- 0

for (input_csv_path in input_files) {
  
  base_filename <- basename(input_csv_path)
  file_handled_this_iter <- FALSE
  
  # Check log
  if (!OVERRIDE_LOG && (input_csv_path %in% processed_files_list)) {
    files_skipped_count <- files_skipped_count + 1
    next
  }
  
  # Announce
  if (OVERRIDE_LOG && (input_csv_path %in% processed_files_list)) { message(paste("\nReprocessing previously processed file:", base_filename)) }
  else { message(paste("\nProcessing NEW file:", base_filename)) }
  
  # Process
  tryCatch({
    
    # Determine paths
    file_dir <- dirname(input_csv_path); relative_path <- sub(paste0("^", gsub("\\\\", "/", normalized_root), "/?"), "", gsub("\\\\", "/", file_dir)); target_sub_dir <- file.path(main_output_dir, relative_path)
    if (!dir.exists(target_sub_dir)) { message(paste("  Creating output subdirectory:", target_sub_dir)); dir.create(target_sub_dir, recursive = TRUE, showWarnings = FALSE) }
    base_output_name <- tools::file_path_sans_ext(base_filename)
    
    # Read Data
    message("  Reading data...")
    raw_data <- data.table::fread(input_csv_path, showProgress = FALSE)
    
    # Handle Empty File
    if (nrow(raw_data) == 0) {
      warning(paste("Input file contains no data rows (only header):", base_filename))
      files_skipped_empty_count <- files_skipped_empty_count + 1
      if (!OVERRIDE_LOG) { message("  Logging empty file as processed in log file."); tryCatch( write(input_csv_path, file = processed_log_file, append = TRUE), error = function(e) {warning("Could not write to processed log file: ", processed_log_file)} ) }
      file_handled_this_iter <- TRUE
      
      # Process File with Data
    } else {
      
      # Check required columns
      required_cols <- c("POSIX", "Digital Pins", "Wheel Analog")
      if (!all(required_cols %in% names(raw_data))) { stop(paste("Input file missing required columns:", paste(setdiff(required_cols, names(raw_data)), collapse=", "))) }
      
      # Extract Metadata + DEBUGGING
      rat_id <- NA; date_str <- NA; time_str <- NA; exp_id <- NA; cohort_id <- NA; date_obj <- as.Date(NA)
      # --- Start Debugging Parsing ---
      print("------------------------------------------")
      print(paste("DEBUG: Checking path/filename:", input_csv_path))
      print(paste("DEBUG: base_output_name =", base_output_name))
      
      # <<< MODIFIED PATTERN >>>
      local_base_name_pattern <- "^(RBB\\d{2})_(\\d{8})_(\\d{6})$" # Separate date and time groups
      local_path_pattern <- ".*/(experiment_(\\d{2}))/(cohort_(\\d{2}))/.*"
      normalized_input_path_for_regex <- gsub("\\\\", "/", input_csv_path)
      
      print(paste("DEBUG: Normalized path for regex =", normalized_input_path_for_regex))
      print(paste("DEBUG: Filename pattern =", local_base_name_pattern)) # Will show updated pattern
      print(paste("DEBUG: Path pattern =", local_path_pattern))
      
      temp_base_match <- stringr::str_match(base_output_name, local_base_name_pattern)
      temp_path_match <- stringr::str_match(normalized_input_path_for_regex, local_path_pattern)
      
      print("DEBUG: base_match result:")
      print(temp_base_match) # Expect 4 columns now if match succeeds
      print("DEBUG: path_match result:")
      print(temp_path_match)
      print("--- End Debugging Parsing ---")
      
      # Original if condition - should now work correctly if patterns match
      if (!anyNA(temp_base_match) && NCOL(temp_base_match) >= 4 && !anyNA(temp_path_match) && NCOL(temp_path_match) >= 5) {
        # <<< ADJUSTED INDICES >>>
        rat_id <- temp_base_match[1, 2]   # Group 1 -> Column 2
        date_str <- temp_base_match[1, 3] # Group 2 -> Column 3
        time_str <- temp_base_match[1, 4] # Group 3 -> Column 4
        exp_id <- temp_path_match[1, 3]   # Group 2 -> Column 3
        cohort_id <- temp_path_match[1, 5] # Group 4 -> Column 5
        date_obj <- lubridate::ymd(date_str, quiet = TRUE)
        message(paste("  Extracted Metadata: Exp=", exp_id, " Cohort=", cohort_id, " Rat=", rat_id, " Date=", date_str))
      } else {
        warning(paste("Could not parse Experiment/Cohort/Rat/Date from path/filename:", input_csv_path, " - Metadata will be skipped."))
      }
      
      # Prepare Timestamps
      message("  Preparing timestamps...")
      # ... (Timestamp logic) ...
      if (inherits(raw_data$POSIX, "integer64")) { timestamps_posixct <- as.POSIXct(as.double(raw_data$POSIX) / 1e6, origin = "1970-01-01", tz = TIMEZONE) } else { timestamps_posixct <- as.POSIXct(raw_data$POSIX / 1e6, origin = "1970-01-01", tz = TIMEZONE) }
      
      # Process Wheel Data
      message("  Processing wheel data...")
      # ... (Wheel logic and saving) ...
      wheel_voltages <- raw_data$`Wheel Analog`; wheel_movement <- calculate_wheel_movement_wrap( wheel_voltages, WHEEL_VOLTAGE_MAX, WHEEL_VOLTAGE_THRESHOLD ); wheel_output <- data.table(Timestamp = timestamps_posixct, Wheel_Movement = wheel_movement); wheel_output_filename <- file.path(target_sub_dir, paste0(base_output_name, "_WheelMovement.csv")); message(paste("    Saving:", basename(wheel_output_filename))); data.table::fwrite(wheel_output, wheel_output_filename, dateTimeAs = "write.csv")
      
      # Process Beambreak Data
      message("  Processing beambreak data...")
      # ... (File 3, 4, 5, 6, 7, 8 logic and saving) ...
      # == File 3 ==
      message("    Generating File 3..."); digital_pins_raw <- raw_data$`Digital Pins`; if (inherits(digital_pins_raw, "integer64")) digital_pins_raw <- as.integer(digital_pins_raw); binary_matrix <- t(sapply(digital_pins_raw, number2binary, noBits = 8)); inverted_binary_matrix <- ifelse(binary_matrix == 0, 1, 0); file3_data <- data.table(Timestamp = timestamps_posixct, DigitalPins_Raw = digital_pins_raw); colnames(inverted_binary_matrix) <- paste0("Channel_", 0:7); file3_data <- cbind(file3_data, inverted_binary_matrix); file3_output_filename <- file.path(target_sub_dir, paste0(base_output_name, "_File3.csv")); message(paste("      Saving:", basename(file3_output_filename))); data.table::fwrite(file3_data, file3_output_filename, dateTimeAs = "write.csv")
      # == File 4 ==
      message("    Generating File 4 (Detecting Events)..."); file4_list <- list(); for (j in 0:7) { channel_vector <- file3_data[[paste0("Channel_", j)]]; starts <- which(diff(c(0, channel_vector)) == 1); ends <- which(diff(c(channel_vector, 0)) == -1); if (length(starts) > 0 && length(ends) > 0) { if(starts[1] > ends[1]) ends <- ends[-1] ; if(length(ends) == 0) next; if(ends[length(ends)] < starts[length(starts)]) starts <- starts[-length(starts)] ; if(length(starts) == 0) next; n_events <- min(length(starts), length(ends)); if(n_events > 0) { start_indices <- starts[1:n_events]; end_indices <- ends[1:n_events]; valid_pair_indices <- which(end_indices >= start_indices); if(length(valid_pair_indices) > 0) { start_indices <- start_indices[valid_pair_indices]; end_indices <- end_indices[valid_pair_indices]; event_start_times <- timestamps_posixct[start_indices]; event_end_times <- timestamps_posixct[end_indices]; durations_seconds <- as.numeric(difftime(event_end_times, event_start_times, units = "secs")); valid_durations <- durations_seconds > 0; if (sum(valid_durations) > 0) { channel_events <- data.table(StartTime = event_start_times[valid_durations], ChannelID = j, Duration = durations_seconds[valid_durations]); if(nrow(channel_events) > 0) { file4_list[[length(file4_list) + 1]] <- channel_events } } } } } } ; if (length(file4_list) > 0) { file4_data <- data.table::rbindlist(file4_list) %>% dplyr::arrange(StartTime) } else { file4_data <- data.table(StartTime=as.POSIXct(character()), ChannelID=integer(), Duration=numeric()) ; message("      WARNING: No beambreak events detected for File 4.") }; file4_output_filename <- file.path(target_sub_dir, paste0(base_output_name, "_File4.csv")); message(paste("      Saving:", basename(file4_output_filename))); data.table::fwrite(file4_data, file4_output_filename, dateTimeAs = "write.csv")
      # == File 5 ==
      message("    Generating File 5..."); if (nrow(file4_data) > 0) { file5_data <- copy(file4_data)[, Label := CHANNEL_LABELS[ChannelID + 1]][, .(StartTime, Label, Duration)] } else { file5_data <- data.table(StartTime=as.POSIXct(character()), Label=character(), Duration=numeric()); message("      WARNING: File 4 was empty, File 5 will also be empty.") }; file5_output_filename <- file.path(target_sub_dir, paste0(base_output_name, "_File5.csv")); message(paste("      Saving:", basename(file5_output_filename))); data.table::fwrite(file5_data, file5_output_filename, dateTimeAs = "write.csv")
      # == Files 6, 7, 8 ==
      if (nrow(file5_data) > 0) { message("    Generating Files 6, 7, 8 (Binned Data)..."); bin_widths <- c("1 min", "1 hour", "1 day"); file_numbers <- c(6, 7, 8); for(i in 1:length(bin_widths)){ binned_data <- create_binned_data(as.data.frame(file5_data), bin_width = bin_widths[i]); output_filename <- file.path(target_sub_dir, paste0(base_output_name, "_File", file_numbers[i], ".csv")); message(paste("      Saving:", basename(output_filename))); data.table::fwrite(binned_data, output_filename, dateTimeAs = "write.csv") } } else { message("      SKIPPING Files 6, 7, 8 because File 5 was empty.") }
      
      # Log successful processing
      if (!OVERRIDE_LOG) { message("  Successfully processed data. Logging file."); tryCatch( write(input_csv_path, file = processed_log_file, append = TRUE), error = function(e) {warning("Could not write to processed log file: ", processed_log_file)} ) }
      else { message("  Successfully processed data (Override Mode - log file not updated).") }
      
      # Collect Metadata for Manifest
      if(!is.na(rat_id) && !is.na(exp_id) && !is.na(cohort_id) && !is.na(date_obj)) {
        # ... (Manifest metadata collection) ...
        current_run_file_metadata <- list(); output_files_to_log <- list( list(type = "WheelMovement", filename = wheel_output_filename), list(type = "File3", filename = file3_output_filename), list(type = "File6", filename = file.path(target_sub_dir, paste0(base_output_name, "_File6.csv"))), list(type = "File7", filename = file.path(target_sub_dir, paste0(base_output_name, "_File7.csv"))), list(type = "File8", filename = file.path(target_sub_dir, paste0(base_output_name, "_File8.csv"))) ); for (file_info in output_files_to_log) { if (file.exists(file_info$filename)) { normalized_output_path <- normalizePath(file_info$filename, winslash = "/", mustWork = FALSE); current_run_file_metadata[[length(current_run_file_metadata) + 1]] <- data.table( file_path = normalized_output_path, experiment_id = exp_id, cohort_id = cohort_id, rat_id = rat_id, date = date_obj, time_str = time_str, file_type = file_info$type ) } else { message(paste("    Output file not found, skipping metadata:", basename(file_info$filename))) } }; if(length(current_run_file_metadata) > 0){ all_metadata_this_run <- c(all_metadata_this_run, current_run_file_metadata) ; message(paste("  Collected metadata for", length(current_run_file_metadata), "output files for manifest.")) } else { message("  No output files found/logged for manifest for this input file.") }
      } else { message("  Skipping metadata collection due to earlier parsing error.") }
      
      # Set flag
      file_handled_this_iter <- TRUE
      
    } # End else (processing file with data)
    
  }, error = function(e) {
    # Error Handling
    files_error_count <- files_error_count + 1
    message(paste("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"))
    message(paste("ERROR processing file:", base_filename))
    message(paste("Error message:", e$message))
    message("Stack trace (limited):")
    try(message(paste(capture.output(traceback(1)), collapse = "\n")), silent = TRUE)
    message(paste("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"))
  }) # End tryCatch
  
  # Increment handled counter
  if (file_handled_this_iter) {
    files_processed_this_run <- files_processed_this_run + 1
  }
  
} # End loop


# --- Update Manifest File ---
# ... (Manifest update logic - unchanged) ...
message("\n========================================================")
message("--- Updating Manifest File ---")
message(paste("Manifest file path:", manifest_file_path))
if (length(all_metadata_this_run) > 0) { new_manifest_data <- rbindlist(all_metadata_this_run); new_manifest_data <- na.omit(new_manifest_data, cols=c("file_path", "experiment_id", "cohort_id", "rat_id", "date", "file_type")); message(paste("Collected metadata for", nrow(new_manifest_data), "valid output files in this run.")) } else { new_manifest_data <- data.table(); message("No new valid metadata collected in this run.") }
final_manifest_data <- data.table()
tryCatch({ if (OVERRIDE_LOG || !file.exists(manifest_file_path)) { message(ifelse(OVERRIDE_LOG, "OVERRIDE_LOG is TRUE. Creating new manifest from this run's processed files.", "Manifest file does not exist. Creating new manifest from this run's processed files.")); final_manifest_data <- new_manifest_data } else { message("Manifest file exists. Reading existing manifest..."); existing_manifest_data <- readRDS(manifest_file_path); required_manifest_cols <- c("file_path", "experiment_id", "cohort_id", "rat_id", "date", "file_type"); if (!is.data.table(existing_manifest_data) || !all(required_manifest_cols %in% names(existing_manifest_data))) { existing_manifest_data <- data.table(); warning("Existing manifest file was invalid or missing required columns. Manifest will be built only from this run's data combined with potentially empty existing data.") }; message(paste("Read", nrow(existing_manifest_data), "entries from existing manifest.")); unique_new_file_paths <- unique(new_manifest_data$file_path); if (nrow(existing_manifest_data) > 0 && length(unique_new_file_paths) > 0 && "file_path" %in% names(existing_manifest_data)) { rows_to_keep <- !(existing_manifest_data$file_path %in% unique_new_file_paths); updated_existing_manifest <- existing_manifest_data[rows_to_keep, ]; removed_count <- nrow(existing_manifest_data) - nrow(updated_existing_manifest); if(removed_count > 0) { message(paste("Removed", removed_count, "old manifest entries for files processed/overwritten in this run.")) } } else { updated_existing_manifest <- existing_manifest_data }; final_manifest_data <- rbindlist(list(updated_existing_manifest, new_manifest_data), use.names = TRUE, fill = TRUE) }; core_columns <- c("file_path", "experiment_id", "cohort_id", "rat_id", "date", "time_str", "file_type"); final_manifest_data <- final_manifest_data[, intersect(core_columns, names(final_manifest_data)), with = FALSE]; if (nrow(final_manifest_data) > 0 && "file_path" %in% names(final_manifest_data)) { final_manifest_data <- final_manifest_data[!is.na(file_path) & file_path != "" & !is.na(experiment_id) & !is.na(cohort_id) & !is.na(rat_id) & !is.na(date) & !is.na(file_type)]; final_manifest_data <- unique(final_manifest_data, by = "file_path") } else { final_manifest_data <- data.table() }; if (nrow(final_manifest_data) == 0) { message("Manifest data is empty. Saving empty manifest with defined columns."); final_manifest_data <- data.table( file_path = character(0), experiment_id = character(0), cohort_id = character(0), rat_id = character(0), date = as.Date(character(0)), time_str = character(0), file_type = character(0) )[, ..core_columns] }; message(paste("Saving updated manifest with", nrow(final_manifest_data), "entries...")); saveRDS(final_manifest_data, manifest_file_path); message("Manifest file successfully updated.") }, error = function(e) { warning("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"); warning(paste("CRITICAL ERROR updating manifest file:", manifest_file_path)); warning("The Shiny app may not function correctly or show updated data."); warning(paste("Error message:", e$message)); warning("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!") })


# --- Script Completion ---
# ... (Completion summary is unchanged) ...
end_time <- Sys.time(); duration <- end_time - start_time
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
if (files_error_count > 0) { message(paste("!! WARNING:", files_error_count, "file(s) encountered errors during processing. Check messages above.")) }
if (files_skipped_empty_count > 0) { message(paste("NOTE:", files_skipped_empty_count, "input file(s) contained only headers and were skipped (no output generated).")) }
message("========================================================")