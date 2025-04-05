# ============================================================================
# Script: RBB_Shiny_App_Optimized.R
# Author: Noah Muscat, Simeone Marino, Gemini AI Assistant
# Date: 2025-04-04
# Description:
#   Optimized R Shiny app for visualizing pre-processed RBB data.
#   Uses caching (shiny::bindCache) and efficient data loading (data.table)
#   to improve performance. Reads processed files (Files 3, 6, 7, 8, WheelMovement)
#   and displays tables and plots.
#
# Data Source: (Same as previous version description)
# Dependencies: shiny (>= 1.6.0 recommended for bindCache), DT, data.table,
#               dplyr, tidyr, lubridate, stringr, ggplot2, shinycssloaders, bit64
# ============================================================================

# --- Load Required Libraries ---
# Load packages quietly for deployed app
suppressPackageStartupMessages({
  library(shiny)
  library(DT)
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(stringr)
  library(ggplot2)
  library(shinycssloaders)
  library(bit64) # Added back as it might have helped with package install issues
})

# --- Global Parameters ---

# Path to the *main* directory where the BATCH SCRIPT saved its output files
# ** Use absolute path **
# --- Global Parameters ---
processed_data_dir <- "/nfs/turbo/umms-brendonw/RBB_Data_Cleaned"
subject_config_file <- "config.txt" # Path to config file (relative to app.R)

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

# Expected column names for binned data (using Measure_Label format)
COUNT_COL_NAMES <- paste("Count", CHANNEL_LABELS, sep = "_")
DURATION_COL_NAMES <- paste("TotalDuration", CHANNEL_LABELS, sep = "_") # Define for potential future use

# --- UI Definition ---
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .shiny-input-container, .shiny-input-container > label { font-size: 14px !important; font-family: Arial, sans-serif !important; }
      .shiny-spinner-output-container { min-height: 300px; }
    "))
  ),
  titlePanel("RBB Experiment Portal (Processed Data Viewer)"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("experiment", "Experiment", choices = c("Loading..." = "")),
      selectInput("cohort", "Cohort", choices = c("Loading..." = "")),
      uiOutput("rat_select_ui"),
      radioButtons("date_choice", "Select Date Option:",
                   choices = c("All Available Dates" = "all", "Single Date" = "single", "Date Range" = "range"),
                   selected = "all"),
      uiOutput("date_input_ui")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs", # Add an ID for potential future use
        type = "tabs",
        tabPanel("Daily Plots (Counts)", withSpinner(plotOutput("daily_plots_output"))),
        tabPanel("Hourly Plots (Counts)", withSpinner(plotOutput("hourly_plots_output"))),
        tabPanel("Minute Bin Data (File 6)",
                 withSpinner(DT::dataTableOutput('tbl_minute')),
                 downloadButton('download_minute', 'Download Minute Data')
        ),
        tabPanel("Raw States (File 3)",
                 withSpinner(DT::dataTableOutput('tbl_raw_states')),
                 downloadButton('download_file3', 'Download Raw States Data')
        ),
        tabPanel("Wheel Movement",
                 withSpinner(DT::dataTableOutput('tbl_wheel')),
                 downloadButton('download_wheel', 'Download Wheel Data')
        ),
        tabPanel("IN PROGRESS - Actigraphy", p("Actigraphy analysis module pending."))
      )
    )
  ),
  div("RBB App Version: V.3.1-Optimized", align = "center")
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  # --- File Discovery ---
  # Cache based on directory path & current date (refreshes cache daily)
  all_processed_files_info <- reactive({
    message("--- all_processed_files_info: Executing (or cache miss) ---")
    processed_data_dir_local <- processed_data_dir
    req(dir.exists(processed_data_dir_local))
    
    # Define patterns
    file_suffixes <- c("_File3.csv", "_File4.csv", "_File5.csv", "_File6.csv", "_File7.csv", "_File8.csv", "_WheelMovement.csv")
    pattern_suffix <- paste(gsub("\\.", "\\\\.", file_suffixes), collapse = "|")
    full_pattern_basename <- paste0("(RBB\\d{2}_\\d{8}_\\d{6})(", pattern_suffix, ")$")
    full_pattern_parse <- "^(RBB\\d{2}_\\d{8}_\\d{6})_(File[3-8]|WheelMovement)\\.csv$"
    base_name_pattern <- "^(RBB\\d{2})_(\\d{8})_(\\d{6})$"
    
    all_files <- list.files(path = processed_data_dir_local, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
    if(length(all_files) == 0) return(data.table())
    
    processed_files <- all_files[grepl(full_pattern_basename, basename(all_files))]
    if(length(processed_files) == 0) return(data.table())
    
    file_info_list <- lapply(processed_files, function(file_path) {
      file_name <- basename(file_path)
      base_match <- stringr::str_match(file_name, full_pattern_parse)
      if (anyNA(base_match) || NCOL(base_match) < 3) return(NULL)
      
      base_name_part <- base_match[1, 2]; file_type <- base_match[1, 3]
      base_name_parts <- stringr::str_match(base_name_part, base_name_pattern)
      exp_match_group <- stringr::str_match(file_path, "/(experiment_(\\d{2}))/")
      cohort_match_group <- stringr::str_match(file_path, "/(cohort_(\\d{2}))/")
      
      if (!anyNA(c(base_name_parts)) && !anyNA(exp_match_group) && !anyNA(cohort_match_group) && NCOL(base_name_parts) >=4 && NCOL(exp_match_group) >=3 && NCOL(cohort_match_group) >=3 ) {
        rat_id <- base_name_parts[1, 2]; date_str <- base_name_parts[1, 3]; time_str <- base_name_parts[1, 4]
        exp_id <- exp_match_group[1, 3]; cohort_id <- cohort_match_group[1, 3]
        return(data.table( file_path = file_path, experiment_id = exp_id, cohort_id = cohort_id, rat_id = rat_id, date = ymd(date_str, quiet = TRUE), time_str = time_str, file_type = file_type ))
      } else { return(NULL) }
    })
    valid_info <- rbindlist(Filter(Negate(is.null), file_info_list))
    
    if(nrow(valid_info) == 0) { showNotification("No valid processed files found after parsing.", type = "warning", duration=15) }
    else { message("Found ", nrow(valid_info), " processed files matching structure.") }
    
    message("--- all_processed_files_info: END ---")
    req(nrow(valid_info) > 0, cancelOutput = FALSE)
    return(valid_info)
    # Cache based on the directory path AND the current date (forces refresh daily)
  }) %>% bindCache(processed_data_dir, Sys.Date())
  
  # --- Populate UI Selectors ---
  observe({
    info <- req(all_processed_files_info()) # Ensure data is ready
    choices <- unique(isolate(info$experiment_id)) # Use isolate
    updateSelectInput(session, "experiment", choices = sort(choices), selected=first(sort(choices))) # Select first item
  })
  
  observe({
    req(input$experiment) # Require experiment to be selected
    info <- req(all_processed_files_info())
    choices <- unique(isolate(info[experiment_id == input$experiment, ]$cohort_id))
    updateSelectInput(session, "cohort", choices = sort(choices), selected=first(sort(choices))) # Select first item
  })
  
  output$rat_select_ui <- renderUI({
    info <- req(all_processed_files_info())
    req(input$experiment, input$cohort) # Require selections
    choices <- unique(isolate(info[experiment_id == input$experiment & cohort_id == input$cohort, ]$rat_id))
    selectInput("rat", label = "Rat ID", choices = sort(choices), selected = first(sort(choices)))
  })
  
  available_dates <- reactive({
    info <- req(all_processed_files_info())
    req(input$experiment, input$cohort, input$rat) # Require selections
    info_sub <- isolate(info[experiment_id == input$experiment & cohort_id == input$cohort & rat_id == input$rat & !is.na(date), ])
    sort(unique(info_sub$date))
  })
  
  output$date_input_ui <- renderUI({
    dates <- available_dates()
    req(length(dates) > 0)
    if (input$date_choice == "single") {
      selectInput("single_date", "Select Date", choices = format(dates, "%Y-%m-%d"), selected = format(last(dates), "%Y-%m-%d"))
    } else if (input$date_choice == "range") {
      dateRangeInput("date_range", label = "Select Date Range", start = first(dates), end = last(dates), min = first(dates), max = last(dates))
    } else { NULL }
  })
  
  # --- Data Loading Reactives ---
  selected_files_base <- reactive({
    info <- req(all_processed_files_info())
    req(input$experiment, input$cohort, input$rat, input$date_choice)
    
    filtered_info <- info[experiment_id == input$experiment & cohort_id == input$cohort & rat_id == input$rat, ]
    req(nrow(filtered_info) > 0, cancelOutput = FALSE)
    
    # Filter by Date
    if (input$date_choice == "single") {
      req(input$single_date); selected_date <- ymd(input$single_date, quiet=TRUE); req(!is.na(selected_date))
      filtered_info <- filtered_info[date == selected_date, ]
    } else if (input$date_choice == "range") {
      req(input$date_range); start_date <- ymd(input$date_range[1], quiet=TRUE); end_date <- ymd(input$date_range[2], quiet=TRUE); req(!is.na(start_date), !is.na(end_date))
      filtered_info <- filtered_info[date >= start_date & date <= end_date, ]
    }
    if(nrow(filtered_info) == 0){ showNotification("No processed files found for the selected criteria.", type="warning") }
    return(filtered_info)
  })
  
  # Helper function to read selected files
  load_selected_files <- function(file_type_short) {
    base_info <- selected_files_base()
    req(nrow(base_info) > 0)
    
    file_suffix <- switch(file_type_short, "File3" = "_File3.csv", "File6" = "_File6.csv", "File7" = "_File7.csv", "File8" = "_File8.csv", "Wheel" = "_WheelMovement.csv", stop("Invalid file type"))
    files_to_load <- base_info[file_type == file_type_short, ]$file_path
    
    files_exist <- file.exists(files_to_load)
    if(!any(files_exist)){ showNotification(paste("No", file_type_short, "files exist for this selection."), type="warning"); return(data.table()) }
    if(!all(files_exist)) { files_to_load <- files_to_load[files_exist] }
    
    message(">>> Reading ", length(files_to_load), " '", file_type_short, "' file(s) from disk (or cache miss)...")
    
    tryCatch({
      # Consider adding colClasses here if fread guesses types wrong for processed files
      data_list <- lapply(files_to_load, data.table::fread)
      combined_data <- data.table::rbindlist(data_list, fill = TRUE, use.names=TRUE) # Use names robustly
      
      # Convert Timestamp/BinStartTime if character
      time_cols <- intersect(c("Timestamp", "BinStartTime"), names(combined_data))
      for(t_col in time_cols){
        if(is.character(combined_data[[t_col]])) {
          # Assuming UTC write from batch, or simple Ymd HMS from write.csv
          combined_data[, (t_col) := lubridate::parse_date_time(get(t_col), orders = c("Ymd HMS", "Ymd HM", "Y-m-d H:M:OS", "Ymd"), tz="UTC", quiet=TRUE)]
        }
        # Ensure it's POSIXct
        if(!lubridate::is.POSIXct(combined_data[[t_col]])) {
          warning("Timestamp column '", t_col, "' not converted to POSIXct successfully.")
          # Attempt conversion if numeric (might be seconds from epoch if read weirdly)
          if(is.numeric(combined_data[[t_col]])) combined_data[, (t_col) := as.POSIXct(get(t_col), origin="1970-01-01", tz="UTC")]
        }
      }
      # Convert Count columns back to integer if needed
      count_col_names_in_data <- grep("^Count_", names(combined_data), value=TRUE)
      for(c_col in count_col_names_in_data){ if(is.numeric(combined_data[[c_col]]) && !is.integer(combined_data[[c_col]])){ combined_data[, (c_col) := as.integer(get(c_col))] }}
      
      message("Loaded data dimensions: ", nrow(combined_data), " rows, ", ncol(combined_data), " cols.")
      return(combined_data)
    }, error = function(e) { showNotification(paste("Error reading/combining", file_type_short, "files:", e$message), type="error"); return(data.table()) })
  }
  
  # Cache key reactive based on selection inputs
  cache_key_inputs <- reactive({ list(input$experiment, input$cohort, input$rat, input$date_choice, input$single_date, input$date_range) })
  
  # Apply bindCache to loading reactives
  loaded_file3 <- reactive({ load_selected_files("File3") }) %>% bindCache(cache_key_inputs())
  loaded_file6 <- reactive({ load_selected_files("File6") }) %>% bindCache(cache_key_inputs())
  loaded_file7 <- reactive({ load_selected_files("File7") }) %>% bindCache(cache_key_inputs())
  loaded_file8 <- reactive({ load_selected_files("File8") }) %>% bindCache(cache_key_inputs())
  loaded_wheel <- reactive({ load_selected_files("WheelMovement") }) %>% bindCache(cache_key_inputs())
  
  # --- Render Outputs ---
  
  # == Plotting Tabs ==
  output$daily_plots_output <- renderPlot({
    data_daily <- loaded_file8(); req(nrow(data_daily) > 0)
    # Ensure BinStartTime is Date
    if (!lubridate::is.Date(data_daily$BinStartTime)) data_daily[, BinStartTime := as.Date(BinStartTime)]
    
    plot_cols <- intersect(COUNT_COL_NAMES, names(data_daily)) # Only plot count cols that exist
    req(length(plot_cols) > 0)
    
    data_long <- tryCatch(melt(data_daily, id.vars = "BinStartTime", measure.vars = plot_cols, variable.name = "Label", value.name = "Count"), error=function(e) NULL)
    req(data_long)
    data_long[, Label := gsub("Count_", "", Label)] # Clean label name
    
    ggplot(data_long, aes(x = BinStartTime, y = Count)) +
      geom_col(fill = "steelblue") +
      facet_wrap(~ Label, scales = "free_y", ncol = 4) + # Try 4 columns
      labs(title = "Total Daily Beambreak Counts per Channel", x = "Date", y = "Total Count") +
      theme_bw(base_size = 9) + # Smaller base size
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), # Rotate 90
            strip.text = element_text(size = 7)) + # Even smaller facet titles
      scale_x_date(date_breaks = "2 days", date_labels = "%m-%d") # Adjust breaks/labels
  }, height=600) # Increase plot height if needed
  
  output$hourly_plots_output <- renderPlot({
    data_hourly <- loaded_file7(); req(nrow(data_hourly) > 0)
    req(lubridate::is.POSIXct(data_hourly$BinStartTime))
    
    plot_cols <- intersect(COUNT_COL_NAMES, names(data_hourly))
    req(length(plot_cols) > 0)
    
    data_long <- tryCatch(melt(data_hourly, id.vars = "BinStartTime", measure.vars = plot_cols, variable.name = "Label", value.name = "Count"), error=function(e) NULL)
    req(data_long)
    data_long[, Label := gsub("Count_", "", Label)]
    
    ggplot(data_long, aes(x = BinStartTime, y = Count)) +
      geom_col(fill = "lightblue") +
      facet_wrap(~ Label, scales = "free_y", ncol = 4) + # Try 4 columns
      labs(title = "Total Hourly Beambreak Counts per Channel", x = "Time", y = "Total Count") +
      theme_bw(base_size = 9) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), # Rotate 90
            strip.text = element_text(size = 7)) +
      scale_x_datetime(date_breaks = "6 hours", date_labels = "%b %d - %H:%M") # Adjust breaks/labels
  }, height=600) # Increase plot height if needed
  
  # == Table Tabs ==
  output$tbl_minute <- DT::renderDataTable({
    data <- loaded_file6(); req(nrow(data) > 0)
    # Optionally format BinStartTime for display
    # data_display <- copy(data)[, BinStartTime := format(BinStartTime, "%Y-%m-%d %H:%M")]
    datatable(data, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  }, server = TRUE) # Enable server-side processing for potentially large minute data
  
  output$tbl_raw_states <- DT::renderDataTable({
    data <- loaded_file3(); req(nrow(data) > 0)
    cols_to_show <- intersect(c("Timestamp", "DigitalPins_Raw", paste0("Channel_", 0:7)), names(data))
    # data[, Timestamp := format(Timestamp, "%Y-%m-%d %H:%M:%OS3")] # Optional formatting
    datatable(data[, ..cols_to_show], options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  }, server = TRUE) # Enable server-side processing for potentially large raw data
  
  output$tbl_wheel <- DT::renderDataTable({
    data <- loaded_wheel(); req(nrow(data) > 0)
    # data[, Timestamp := format(Timestamp, "%Y-%m-%d %H:%M:%OS3")] # Optional formatting
    datatable(data, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    # No server=TRUE unless WheelMovement files are expected to be huge
  })
  
  # --- Download Handlers ---
  output$download_minute <- downloadHandler(
    filename = function() { paste0("minute_binned_data_", input$experiment, "_", input$cohort, "_", input$rat, "_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(loaded_file6(), file, row.names = FALSE) }
  )
  output$download_file3 <- downloadHandler(
    filename = function() { paste0("raw_states_file3_", input$experiment, "_", input$cohort, "_", input$rat, "_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(loaded_file3(), file, row.names = FALSE) }
  )
  output$download_wheel <- downloadHandler(
    filename = function() { paste0("wheel_movement_", input$experiment, "_", input$cohort, "_", input$rat, "_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(loaded_wheel(), file, row.names = FALSE) }
  )
  
} # End Server Function

# --- Run the Shiny App ---
shinyApp(ui = ui, server = server)