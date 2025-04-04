library(shiny)
library(DT)
library(lubridate)
library(dplyr)
library(data.table)
library(bit64)
library(memoise)
library(shinycssloaders)
library(ggplot2)

# Define the path to the home directory where experiments are stored
home_directory <- "/nfs/turbo/umms-brendonw/RBB_Data"

# Memoize the data loading function to cache results and improve performance
cached_load_data <- memoise(function(path, type = "regular", compressed = TRUE) {
  if (is.na(path) || path == "" || !file.exists(path)) {
    cat("Invalid file path detected: ", path, "\n")
    return(NULL)
  }
  tryCatch({
    data <- fread(path, colClasses = c(POSIX = "integer64"), check.names = TRUE)
    
    if (type == "regular" || type == "actigraphy") {
      if ("POSIX" %in% names(data) && !any(is.na(data$POSIX))) {
        if (class(data$POSIX) != "integer64") {
          stop("POSIX column is not integer64")
        }
        data[, datetime := as.POSIXct(as.numeric(data$POSIX) / 1e6, origin = "1970-01-01", tz = "UTC")]
        if (compressed) {
          data <- data[Digital.Pins != 255]
        }
      } else {
        stop("POSIX column is missing or contains NA")
      }
      
      if (type == "regular") {
        data[, channel_states := sapply(Digital.Pins, function(x) paste(rev(as.integer(intToBits(x)[1:8])), collapse = ""))]
        data[, paste0("Channel_", 1:8) := lapply(8:1, function(i) 1 - as.integer(substr(channel_states, i, i)))]
      }
    }
    return(data)
  }, error = function(e) {
    message <- paste("Error loading data from path:", path, "- Error message:", e$message)
    cat(message, "\n")
    showNotification(message, type = "error")
    NULL
  })
})

# Define UI layout and elements
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .shiny-input-container, .shiny-input-container > label {
        font-size: 14px !important;
        font-family: Arial, sans-serif !important;
      }
    "))
  ),
  titlePanel("RBB Experiment Portal"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("experiment", "Experiment", choices = c()),
      selectInput("cohort", "Cohort", choices = c()),
      uiOutput("rat_select"),
      radioButtons("date_choice", "Select Date Option:", 
                   choices = c("All" = "all", "Single Date" = "single", "Date Range" = "range")),
      uiOutput("date_input")
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Daily Plots",
                 withSpinner(plotOutput("daily_plot_1")), withSpinner(plotOutput("daily_plot_2")), withSpinner(plotOutput("daily_plot_3")),
                 withSpinner(plotOutput("daily_plot_4")), withSpinner(plotOutput("daily_plot_5")), withSpinner(plotOutput("daily_plot_6")),
                 withSpinner(plotOutput("daily_plot_7")), withSpinner(plotOutput("daily_plot_8"))),
        tabPanel("Hourly Plots", 
                 withSpinner(plotOutput("hourly_plot_1")), withSpinner(plotOutput("hourly_plot_2")), withSpinner(plotOutput("hourly_plot_3")),
                 withSpinner(plotOutput("hourly_plot_4")), withSpinner(plotOutput("hourly_plot_5")), withSpinner(plotOutput("hourly_plot_6")),
                 withSpinner(plotOutput("hourly_plot_7")), withSpinner(plotOutput("hourly_plot_8"))),
        tabPanel("Hour Binned Data", withSpinner(DT::dataTableOutput('tbl_hour')), downloadButton('download_hour', 'Download Hour Data')),
        tabPanel("Minute Binned Data", withSpinner(DT::dataTableOutput('tbl_minute')), downloadButton('download_minute', 'Download Minute Data')),
        tabPanel("Digital Data", withSpinner(DT::dataTableOutput('tbl_raw')), downloadButton('download_raw', 'Download Raw Data')),
        tabPanel("IN PROGRESS - Actigraphy", withSpinner(DT::dataTableOutput('tbl_actigraphy')), downloadButton('download_actigraphy', 'Download Actigraphy Data'))
      )
    )
  ),
  div("RBB App Version: V.2.2", align = "center")
)

# Define server logic components
server <- function(input, output, session) {
  
  # Utility function to fetch experiment and cohort list
  list_experiments_and_cohorts <- function(home_directory) {
    experiment_dirs <- list.dirs(path = home_directory, full.names = FALSE, recursive = FALSE)
    experiment_ids <- gsub("experiment_", "", grep("^experiment_\\d{2}$", experiment_dirs, value = TRUE))
    
    cohort_lists <- lapply(experiment_ids, function(id) {
      cohort_dirs <- list.dirs(path = file.path(home_directory, paste0("experiment_", id)), full.names = FALSE, recursive = FALSE)
      cohort_ids <- gsub("cohort_", "", grep("^cohort_\\d{2}$", cohort_dirs, value = TRUE))
      return(cohort_ids)
    })
    names(cohort_lists) <- experiment_ids
    
    return(list(experiments = experiment_ids, cohorts = cohort_lists))
  }
  
  # Store experiment and cohort list in reactive values
  experiments_and_cohorts <- reactiveValues(data = list())
  
  observe({
    experiments_and_cohorts$data <- list_experiments_and_cohorts(home_directory)
  })
  
  # Update experiment dropdown choices
  observe({
    updateSelectInput(session, "experiment", choices = experiments_and_cohorts$data$experiments)
  })
  
  # Update cohort dropdown based on selected experiment
  observeEvent(input$experiment, {
    selected_experiment <- input$experiment
    cohort_choices <- experiments_and_cohorts$data$cohorts[[selected_experiment]]
    
    updateSelectInput(session, "cohort", choices = cohort_choices)
  })
  
  # Collect metadata for available files given an experiment and cohort
  all_files_info <- reactive({
    tryCatch({
      experiment_path <- file.path(home_directory, paste0("experiment_", input$experiment))
      cohort_path <- file.path(experiment_path, paste0("cohort_", input$cohort))
      csv_files <- list.files(cohort_path, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
      
      file_info <- lapply(csv_files, function(file_path) {
        file_name <- basename(file_path)
        matches <- regmatches(file_name, regexec("^RBB(\\d{2})_(\\d{8})_(\\d{6})\\.csv$", file_name))
        if (length(matches[[1]]) == 4) {
          date_extracted <- ymd(matches[[1]][3])
          if (!is.na(date_extracted)) {
            return(data.frame(
              file_path = file_path,
              rat_id = matches[[1]][2],
              date = date_extracted,
              time = matches[[1]][4],
              type = "regular",
              stringsAsFactors = FALSE
            ))
          }
        }
        NULL
      })
      
      valid_info <- do.call(rbind, na.omit(file_info))
      valid_info
    }, error = function(e) {
      message <- paste("Error gathering file info:", e$message)
      cat(message)
      showNotification(message, type = "error")
      NULL
    })
  })
  
  # UI component for selecting a rat
  output$rat_select <- renderUI({
    rats <- tryCatch({
      unique(all_files_info()$rat_id)
    }, error = function(e) {
      showNotification("Error retrieving rat list", type = "error")
      NULL
    })
    selectInput("rat", label = "Rat", choices = rats, selected = first(rats, default = NULL))
  })
  
  # UI logic for date selection
  output$date_input <- renderUI({
    req(input$rat)
    all_dates <- tryCatch({
      unique(all_files_info()[all_files_info()$rat_id == input$rat, ]$date)
    }, error = function(e) {
      showNotification("Error retrieving dates for selected rat", type = "error")
      NULL
    })
    
    if (input$date_choice == "single") {
      selectInput("single_date", "Select Date", choices = format(all_dates, "%Y-%m-%d"), selected = first(all_dates))
    } else if (input$date_choice == "range") {
      dateRangeInput("date_range", 
                     label = "Select Date Range", 
                     start = min(all_dates, na.rm = TRUE),
                     end = max(all_dates, na.rm = TRUE),
                     min = min(all_dates, na.rm = TRUE),
                     max = max(all_dates, na.rm = TRUE))
    } else {
      div()
    }
  })
  
  # Automatically load the compressed data whenever necessary conditions are met
  current_data <- reactive({
    req(input$rat)
    req(all_files_info())
    
    tryCatch({
      if (input$date_choice == "all") {
        selected_files <- all_files_info()[all_files_info()$rat_id == input$rat, ]
      } else if (input$date_choice == "single" && !is.null(input$single_date)) {
        selected_files <- all_files_info()[all_files_info()$rat_id == input$rat & 
                                             all_files_info()$date == as.Date(input$single_date), ]
      } else if (input$date_choice == "range" && !is.null(input$date_range)) {
        selected_files <- all_files_info()[all_files_info()$rat_id == input$rat &
                                             all_files_info()$date >= as.Date(input$date_range[1]) & 
                                             all_files_info()$date <= as.Date(input$date_range[2]), ]
      } else {
        stop("Please select valid dates.")
      }
      
      if (nrow(selected_files) == 0) {
        stop("No data files match the selected criteria.")
      }
      
      data_list <- lapply(1:nrow(selected_files), function(i) {
        file_row <- selected_files[i, ]
        cached_load_data(file_row$file_path, type = file_row$type, compressed = TRUE)
      })
      
      req(data_list)
      return(rbindlist(data_list, fill = TRUE))
    }, error = function(e) {
      showNotification(paste("Error loading data files:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Render digital data as a table
  output$tbl_raw <- DT::renderDataTable({
    data_raw <- current_data()
    req(data_raw)
    datatable(data_raw %>% select(-datetime, -starts_with("Channel_")), options = list(pageLength = 10))
  }, server = TRUE)
  
  # Render minute binned data for detailed analysis
  output$tbl_minute <- DT::renderDataTable({
    data_raw <- current_data()
    req(data_raw)
    
    minute_binned <- data_raw[, .(
      Wheel_Analog_Mean = mean(Wheel.Analog, na.rm = TRUE),
      Beambreaks_Channel_1 = sum(Channel_1),
      Beambreaks_Channel_2 = sum(Channel_2),
      Beambreaks_Channel_3 = sum(Channel_3),
      Beambreaks_Channel_4 = sum(Channel_4),
      Beambreaks_Channel_5 = sum(Channel_5),
      Beambreaks_Channel_6 = sum(Channel_6),
      Beambreaks_Channel_7 = sum(Channel_7),
      Beambreaks_Channel_8 = sum(Channel_8)
    ), by = .(minute = floor_date(datetime, "minute"))]
    minute_binned[, minute := format(minute, "%Y-%m-%d %H:%M", tz = "UTC")]
    
    datatable(minute_binned, options = list(pageLength = 10))
  }, server = TRUE)
  
  # Process data for hourly interval analysis
  hour_binned_data <- reactive({
    data_raw <- current_data()
    req(data_raw)
    
    data_raw[, .(
      Wheel_Analog_Mean = mean(Wheel.Analog, na.rm = TRUE),
      Beambreaks_Channel_1 = sum(Channel_1),
      Beambreaks_Channel_2 = sum(Channel_2),
      Beambreaks_Channel_3 = sum(Channel_3),
      Beambreaks_Channel_4 = sum(Channel_4),
      Beambreaks_Channel_5 = sum(Channel_5),
      Beambreaks_Channel_6 = sum(Channel_6),
      Beambreaks_Channel_7 = sum(Channel_7),
      Beambreaks_Channel_8 = sum(Channel_8)
      ), by = .(hour = floor_date(datetime, "hour"))]
  })
  
  # Render hour binned data as a table
  output$tbl_hour <- DT::renderDataTable({
    hour_binned <- hour_binned_data()
    req(hour_binned)
    # Format the hour to show full datetime
    hour_binned[, hour := format(hour, "%Y-%m-%d %H:%M", tz = "UTC")]
    
    datatable(hour_binned, options = list(pageLength = 10))
  }, server = TRUE)
  
  # Generate hourly plots for each channel
  for (i in 1:8) {
    local({
      channel_index <- i
      output[[paste0("hourly_plot_", channel_index)]] <- renderPlot({
        hour_binned <- hour_binned_data()
        req(hour_binned)
        channel_name <- paste0("Beambreaks_Channel_", channel_index)
        ggplot(hour_binned, aes(x = as.numeric(format(as.POSIXct(hour), "%H")), y = get(channel_name))) +
          geom_bar(stat = "identity", fill = "steelblue") +
          theme_minimal() +
          labs(title = paste("Beambreaks per Hour - Channel", channel_index),
               x = "Hour of Day", y = "Total Beambreaks") +
          scale_x_continuous(breaks = seq(0, 23, by = 1))
      })
    })
  }
  
  # Aggregate daily binned data for visualization
  daily_binned_data <- reactive({
    data_raw <- current_data()
    req(data_raw)
    
    data_raw[, .(
      Beambreaks_Channel_1 = sum(Channel_1),
      Beambreaks_Channel_2 = sum(Channel_2),
      Beambreaks_Channel_3 = sum(Channel_3),
      Beambreaks_Channel_4 = sum(Channel_4),
      Beambreaks_Channel_5 = sum(Channel_5),
      Beambreaks_Channel_6 = sum(Channel_6),
      Beambreaks_Channel_7 = sum(Channel_7),
      Beambreaks_Channel_8 = sum(Channel_8)
    ), by = .(day = as.Date(floor_date(datetime, "day")))]  # Converting explicitly to Date
  })
  
  # Generate daily plots for each channel
  for (i in 1:8) {
    local({
      channel_index <- i
      output[[paste0("daily_plot_", channel_index)]] <- renderPlot({
        daily_binned <- daily_binned_data()
        req(daily_binned)
        channel_name <- paste0("Beambreaks_Channel_", channel_index)
        ggplot(daily_binned, aes(x = day, y = get(channel_name))) +
          geom_bar(stat = "identity", fill = "steelblue") +
          theme_minimal() +
          labs(title = paste("Beambreaks per Day - Channel", channel_index),
               x = "Day", y = "Total Beambreaks") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          scale_x_date(date_breaks = "1 day", date_labels = "%Y-%m-%d")
      })
    })
  }
  
  # Download handlers for each data table
  output$download_hour <- downloadHandler(
    filename = function() { 
      paste('hour_binned_data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(hour_binned_data(), file, row.names = FALSE)
    }
  )
  
  output$download_minute <- downloadHandler(
    filename = function() { 
      paste('minute_binned_data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(current_data()[, .(
        Wheel_Analog_Mean = mean(Wheel.Analog, na.rm = TRUE),
        Beambreaks_Channel_1 = sum(Channel_1),
        Beambreaks_Channel_2 = sum(Channel_2),
        Beambreaks_Channel_3 = sum(Channel_3),
        Beambreaks_Channel_4 = sum(Channel_4),
        Beambreaks_Channel_5 = sum(Channel_5),
        Beambreaks_Channel_6 = sum(Channel_6),
        Beambreaks_Channel_7 = sum(Channel_7),
        Beambreaks_Channel_8 = sum(Channel_8)
      ), by = .(minute = format(floor_date(datetime, "minute"), "%Y-%m-%d %H:%M"))], file, row.names = FALSE)
    }
  )
  
  output$download_raw <- downloadHandler(
    filename = function() { 
      paste('digital_data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      data_raw <- current_data()
      req(data_raw)
      write.csv(data_raw %>% select(-datetime, -starts_with("Channel_")), file, row.names = FALSE)
    }
  )
  
  output$download_actigraphy <- downloadHandler(
    filename = function() { 
      paste('actigraphy_data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      # Assuming 'tbl_actigraphy' represents some loaded actigraphy data
      write.csv(current_data(), file, row.names = FALSE)
    }
  )
}

# Run the Shiny App
shinyApp(ui = ui, server = server)