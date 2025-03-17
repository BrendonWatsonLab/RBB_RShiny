library(shiny)
library(DT)
library(lubridate)
library(dplyr)

# Define the path to the home directory where experiments are stored
# home_directory <- "/Users/noahmuscat/Downloads"
home_directory <- "/nfs/turbo/umms-brendonw/RBB_Data"

### UI Definition
ui <- fluidPage(
  # Define the title of the Shiny app
  titlePanel("RBB Experiment Portal"),
  
  sidebarLayout(
    sidebarPanel(
      # Dynamically generated UI elements for selecting experiment, cohort, rat, and day
      uiOutput("experiment_select"),
      uiOutput("cohort_select"),
      uiOutput("rat_select"),
      uiOutput("day_select")
    ),
    mainPanel(
      # Define tab panels for different data views
      tabsetPanel(
        type = "tabs",
        tabPanel("Digital Data", DT::dataTableOutput('tbl_raw')), # Display raw digital data
        tabPanel("Minute Binned Data", DT::dataTableOutput('tbl_minute')), # Display minute-binned summary
        tabPanel("Hour Binned Data", DT::dataTableOutput('tbl_hour')) # Display hour-binned summary
      )
    )
  ),
  # Footer version information
  div("RBB App Version: V.1.0", align = "center")
)

### SERVER Logic
server <- function(input, output, session) {
  
  # Reactive to detect available experiment folders, checks every 5 seconds
  available_experiments <- reactivePoll(5000, session,  
    checkFunc = function() {
      # Check current directories in home directory
      dirs <- list.dirs(home_directory, full.names = FALSE, recursive = FALSE)
      return(dirs)
    },
    valueFunc = function() {
      # Filter out only directories with names starting with "experiment_"
      experiment_dirs <- list.dirs(home_directory, full.names = FALSE, recursive = FALSE)
      experiment_dirs <- experiment_dirs[grepl("^experiment_", experiment_dirs)]
      return(experiment_dirs)
    })
  
  # Reactive to find available cohorts based on the selected experiment
  available_cohorts <- reactive({
    # Construct path for the current experiment
    experiment_path <- file.path(home_directory, input$experiment)
    # Check if the path exists, return empty if not
    if (!dir.exists(experiment_path)) return(character(0))
    # List subdirectories representing cohorts
    list.dirs(experiment_path, full.names = FALSE, recursive = FALSE)
  })
  
  # Reactive function to find rat folders within the selected cohort
  rat_folders <- reactive({
    # Construct path for the current cohort
    cohort_path <- file.path(home_directory, input$experiment, input$cohort)
    # Check if the path exists; return empty if not found
    if (!dir.exists(cohort_path)) return(character(0))
    # List subdirectories representing individual rats
    list.dirs(cohort_path, full.names = FALSE, recursive = FALSE)
  })
  
  # Reactive function to identify unique days from the files for the selected rat
  unique_days <- reactive({
    # Construct path for the current rat
    current_rat_path <- file.path(home_directory, input$experiment, input$cohort, input$rat)
    # List all CSV files in the directory
    files_csv <- list.files(path = current_rat_path, pattern = "\\.csv$", full.names = TRUE)
    # Return empty if no files are found
    if (length(files_csv) == 0) return(character(0))
    
    # Extract unique days from each file's datetime column
    days <- lapply(files_csv, function(file) {
      # Read the CSV file
      data <- read.csv(file, colClasses = c("POSIX" = "numeric"))
      # Convert POSIX timestamps to date-time format
      data$datetime <- as.POSIXct(data$POSIX / 1e6, origin = "1970-01-01", tz = "GMT")
      # Collect unique dates
      unique(as.Date(data$datetime))
    })
    # Combine all unique dates and format; add "All" as a selection option
    days <- unique(do.call(c, days))
    return(c("All", format(days, "%Y-%m-%d")))
  })
  
  # UI output functions to create dropdowns for user selections
  output$experiment_select <- renderUI({
    selectInput("experiment", 
      label = h3("Experiment"),
      choices = available_experiments(), 
      selected = NULL)
  })
  
  output$cohort_select <- renderUI({
    selectInput("cohort", 
      label = h3("Cohort"),
      choices = available_cohorts(), 
      selected = NULL)
  })
  
  output$rat_select <- renderUI({
    selectInput("rat", 
      label = h3("Rat"),
      choices = rat_folders(), 
      selected = NULL)
  })
  
  output$day_select <- renderUI({
    selectInput("day", 
      label = h3("Day"),
      choices = unique_days(), 
      selected = "All")
  })
  
  # Function to get the modification time of the most recent file in a path
  latest_file_time <- function(current_rat_path) {
    files_csv <- list.files(path = current_rat_path, pattern = "\\.csv$", full.names = TRUE)
    if (length(files_csv) == 0) return(0)
    file.info(files_csv)$mtime %>% max
  }
  
  # Reactive polling to monitor updates in the CSV files
  data_updated <- reactivePoll(10000, session, 
    checkFunc = function() {
      current_rat_path <- file.path(home_directory, input$experiment, input$cohort, input$rat)
      latest_file_time(current_rat_path)
    },
    valueFunc = function() {
      current_rat_path <- file.path(home_directory, input$experiment, input$cohort, input$rat)
      files_csv <- list.files(path = current_rat_path, pattern = "\\.csv$", full.names = TRUE)
      # Read and combine data from all CSV files
      data_list <- lapply(files_csv, function(file) {
        # Read CSV file and specify column types
        data <- read.csv(file, colClasses = c("POSIX" = "numeric", "Digital.Pins" = "integer",
                                              "Light.State" = "integer", "Wheel.Analog" = "numeric"))
        # Convert POSIX timestamps to readable date-time format
        data$datetime <- as.POSIXct(data$POSIX / 1e6, origin = "1970-01-01", tz = "GMT")
        # Compute individual binary channels from Digital.Pins
        data$channel_states <- sapply(data$Digital.Pins, function(x) paste(rev(as.integer(intToBits(x)[1:8])), collapse = ""))
        # Inverting to reflect '1' means beambreak for channels
        data$Channel_1 <- 1 - as.integer(substr(data$channel_states, 8, 8))
        data$Channel_2 <- 1 - as.integer(substr(data$channel_states, 7, 7))
        data$Channel_3 <- 1 - as.integer(substr(data$channel_states, 6, 6))
        data$Channel_4 <- 1 - as.integer(substr(data$channel_states, 5, 5))
        data$Channel_5 <- 1 - as.integer(substr(data$channel_states, 4, 4))
        data$Channel_6 <- 1 - as.integer(substr(data$channel_states, 3, 3))
        data$Channel_7 <- 1 - as.integer(substr(data$channel_states, 2, 2))
        data$Channel_8 <- 1 - as.integer(substr(data$channel_states, 1, 1))
        return(data)
      })
      # Combine data frames from different files
      return(do.call(rbind, data_list))
    }
  )
  
  # Filter data for display based on selected day
  filtered_data <- reactive({
    if (input$day == "All") {
      return(data_updated()) # Return all data
    } else {
      return(subset(data_updated(), as.Date(datetime) == as.Date(input$day))) # Filter by selected day
    }
  })
  
  # Render the Digital Raw Data table
  output$tbl_raw <- DT::renderDataTable({
    data_raw <- filtered_data()
    # Remove columns that are not needed for raw view
    data_display <- data_raw %>%
      select(-datetime, -contains("Channel_")) # Excludes datetime and channel state columns
    datatable(data_display, options = list(server = TRUE, pageLength = 10))
  }, server = TRUE)
  
  # Render the Minute Binned Data table
  output$tbl_minute <- DT::renderDataTable({
    data_raw <- filtered_data()
    # Summarize data by minute
    minute_binned <- data_raw %>%
      mutate(minute = floor_date(datetime, "minute")) %>%
      group_by(minute) %>%
      summarize(
        Wheel_Analog_Mean = mean(Wheel.Analog, na.rm = TRUE),
        Beambreaks_Channel_1 = sum(Channel_1),
        Beambreaks_Channel_2 = sum(Channel_2),
        Beambreaks_Channel_3 = sum(Channel_3),
        Beambreaks_Channel_4 = sum(Channel_4),
        Beambreaks_Channel_5 = sum(Channel_5),
        Beambreaks_Channel_6 = sum(Channel_6),
        Beambreaks_Channel_7 = sum(Channel_7),
        Beambreaks_Channel_8 = sum(Channel_8)
      ) %>%
      mutate(minute = format(minute, "%Y-%m-%d %H:%M", tz = "GMT"))
    
    datatable(minute_binned, options = list(pageLength = 10))
  }, server = TRUE)
  
  # Render the Hour Binned Data table
  output$tbl_hour <- DT::renderDataTable({
    data_raw <- filtered_data()
    # Summarize data by hour
    hour_binned <- data_raw %>%
      mutate(hour = floor_date(datetime, "hour")) %>%
      group_by(hour) %>%
      summarize(
        Wheel_Analog_Mean = mean(Wheel.Analog, na.rm = TRUE),
        Beambreaks_Channel_1 = sum(Channel_1),
        Beambreaks_Channel_2 = sum(Channel_2),
        Beambreaks_Channel_3 = sum(Channel_3),
        Beambreaks_Channel_4 = sum(Channel_4),
        Beambreaks_Channel_5 = sum(Channel_5),
        Beambreaks_Channel_6 = sum(Channel_6),
        Beambreaks_Channel_7 = sum(Channel_7),
        Beambreaks_Channel_8 = sum(Channel_8)
      ) %>%
      mutate(hour = format(hour, "%Y-%m-%d %H", tz = "GMT"))
    
    datatable(hour_binned, options = list(pageLength = 10))
  }, server = TRUE)
}

### App
shinyApp(ui = ui, server = server)