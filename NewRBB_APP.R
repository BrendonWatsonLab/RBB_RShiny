library(shiny)
library(DT)
library(lubridate)
library(dplyr)

# Define the path to the home directory where experiments are stored
# home_directory <- "/Users/noahmuscat/Desktop/WatsonLab/SimeoneStuff/RBB_RShiny"
home_directory <- "/nfs/turbo/umms-brendonw/RBB_Data"

# UI Definition
ui <- fluidPage(
  titlePanel("RBB Experiment Portal"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("experiment_select"),
      uiOutput("cohort_select"),
      uiOutput("rat_select"),
      uiOutput("day_select"),
      actionButton("refresh", "Refresh Data")
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Digital Data", DT::dataTableOutput('tbl_raw')),
        tabPanel("Minute Binned Data", DT::dataTableOutput('tbl_minute')),
        tabPanel("Hour Binned Data", DT::dataTableOutput('tbl_hour')),
        tabPanel("Actigraphy", DT::dataTableOutput('tbl_actigraphy'))
      )
    )
  ),
  div("RBB App Version: V.1.0", align = "center")
)

# SERVER Logic
server <- function(input, output, session) {
  
  available_experiments <- reactive({
    input$refresh
    dirs <- list.dirs(home_directory, full.names = FALSE, recursive = FALSE)
    experiment_dirs <- grep("^experiment_", dirs, value = TRUE)
    experiment_dirs
  })
  
  available_cohorts <- reactive({
    input$refresh
    experiment_path <- file.path(home_directory, input$experiment)
    if (!dir.exists(experiment_path)) return(character(0))
    cohort_dirs <- list.dirs(experiment_path, full.names = FALSE, recursive = FALSE)
    cohort_dirs
  })
  
  rat_folders <- reactive({
    input$refresh
    cohort_path <- file.path(home_directory, input$experiment, input$cohort)
    if (!dir.exists(cohort_path)) return(character(0))
    rat_dirs <- list.dirs(cohort_path, full.names = FALSE, recursive = FALSE)
    rat_dirs
  })
  
  unique_days <- reactive({
    input$refresh
    current_rat_path <- file.path(home_directory, input$experiment, input$cohort, input$rat)
    
    # Regular data in microseconds
    files_csv <- list.files(path = current_rat_path, pattern = "\\.csv$", full.names = TRUE)
    regular_days <- lapply(files_csv, function(file) {
      data <- read.csv(file, colClasses = c("POSIX" = "numeric"))
      data$datetime <- as.POSIXct(data$POSIX / 1e6, origin = "1970-01-01", tz = "GMT")
      unique(as.Date(data$datetime))
    })
    
    # Actigraphy data in milliseconds
    actigraphy_path <- file.path(current_rat_path, paste0(input$rat, "_Actigraphy"))
    actigraphy_csv <- list.files(path = actigraphy_path, pattern = "\\.csv$", full.names = TRUE)
    actigraphy_days <- lapply(actigraphy_csv, function(file) {
      data <- read.csv(file, colClasses = c("POSIX" = "numeric"))
      data$datetime <- as.POSIXct(data$POSIX / 1000, origin = "1970-01-01", tz = "GMT")
      unique(as.Date(data$datetime))
    })
    
    # Combine days from both datasets, deduplicate, and prepare for dropdown
    all_days <- unique(do.call(c, c(regular_days, actigraphy_days)))
    return(c("All", format(all_days, "%Y-%m-%d")))
  })
  
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
  
  data_updated <- eventReactive(input$refresh, {
    current_rat_path <- file.path(home_directory, input$experiment, input$cohort, input$rat)
    files_csv <- list.files(path = current_rat_path, pattern = "\\.csv$", full.names = TRUE)
    data_list <- lapply(files_csv, function(file) {
      data <- read.csv(file, colClasses = c("POSIX" = "numeric", "Digital.Pins" = "integer",
                                            "Light.State" = "integer", "Wheel.Analog" = "numeric"))
      data$datetime <- as.POSIXct(data$POSIX / 1e6, origin = "1970-01-01", tz = "GMT")
      data$channel_states <- sapply(data$Digital.Pins, function(x) paste(rev(as.integer(intToBits(x)[1:8])), collapse = ""))
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
    return(do.call(rbind, data_list))
  })
  
  actigraphy_data <- eventReactive(input$refresh, {
    actigraphy_path <- file.path(home_directory, input$experiment, input$cohort, input$rat, paste0(input$rat, "_Actigraphy"))
    if (!dir.exists(actigraphy_path)) return(data.frame())
    files_csv <- list.files(path = actigraphy_path, pattern = "\\.csv$", full.names = TRUE)
    data_list <- lapply(files_csv, function(file) {
      data <- read.csv(file, colClasses = c("Frame" = "numeric", "TimeElapsedMicros" = "numeric", "RawDifference" = "numeric",
                                            "RMSE" = "numeric", "SelectedPixelDifference" = "numeric", "POSIX" = "numeric"))
      data$datetime <- as.POSIXct(data$POSIX / 1000, origin = "1970-01-01", tz = "GMT")
      return(data)
    })
    return(do.call(rbind, data_list))
  })
  
  filtered_data <- reactive({
    if (input$day == "All") {
      return(data_updated())
    } else {
      return(subset(data_updated(), as.Date(datetime) == as.Date(input$day)))
    }
  })
  
  filtered_actigraphy_data <- reactive({
    actigraphy_data_current <- actigraphy_data()
    if (input$day == "All") {
      return(actigraphy_data_current)
    } else {
      return(subset(actigraphy_data_current, as.Date(datetime) == as.Date(input$day)))
    }
  })
  
  output$tbl_raw <- DT::renderDataTable({
    data_raw <- filtered_data()
    data_display <- data_raw %>%
      select(-datetime, -contains("Channel_"))
    datatable(data_display, options = list(server = TRUE, pageLength = 10))
  }, server = TRUE)
  
  output$tbl_minute <- DT::renderDataTable({
    data_raw <- filtered_data()
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
  
  output$tbl_hour <- DT::renderDataTable({
    data_raw <- filtered_data()
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
  
  output$tbl_actigraphy <- DT::renderDataTable({
    data_actigraphy <- filtered_actigraphy_data()
    datatable(data_actigraphy, options = list(server = TRUE, pageLength = 10))
  }, server = TRUE)
}

# Run the Shiny App
shinyApp(ui = ui, server = server)