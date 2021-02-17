library(shiny)
library(stringr)
library(tidyverse)
library(httr)
library(jsonlite)
library(ggplot2)
library(shinyjs)
library(shinydashboard)
library(shinycssloaders)
#

# library(rsconnect)
# deployApp()

# Define UI for application that draws a histogram


ui <- dashboardPage(
  # Application title
  dashboardHeader(title = "pavloviaShinyApp"),
  dashboardSidebar(
    column(12,align = "center",offset = 0,
           tags$style(".skin-blue .sidebar a { color: #444; }"),
           
           textInput("password", label = h3("Password"), placeholder = "Enter Password to get access to Data..."),
           
           # Button
           conditionalPanel(
             condition = "input.password == 'example'",
             downloadButton("downloadData", "Download", ) %>% withSpinner(color = "#0dc5c1")
           ))
  ),
  
  
  # Main panel for displaying outputs ----
  dashboardBody(
    useShinyjs(),
    fluidRow(conditionalPanel(
      condition = "input.password == 'example'",
      DT::dataTableOutput("dataOverview")
    )),
    
    fluidRow(conditionalPanel(
      condition = "input.password == 'example'",
      plotOutput("plotTest")
    ))
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  # Reactive value for selected dataset ----
  datasetInput <- reactiveVal({
    disable("downloadData")
    token <- read_file("token") # Personal Access Token for the Project
    project_id <- 104201 # Project ID
    gitlabPavloviaURL <- paste0("https://gitlab.pavlovia.org/api/v4/projects/", project_id, "/repository/archive.zip") # API - URL to download whole repository
    r <- GET(gitlabPavloviaURL, add_headers("PRIVATE-TOKEN" = token)) # Getting Archive
    
    bin <- content(r, "raw") # Writing Binary
    
    temp <- tempfile() # Init Tempfile
    
    writeBin(bin, temp) # Write Binary of Archive to Tempfile
    
    listofFiles <- unzip(
      zipfile = temp, overwrite = T,
      junkpaths = T, list = T
    ) # Unzip only list of all files in the archive.zip file
    
    
    csvFiles <- grep("*.csv", x = listofFiles$Name, value = T) # Grep only the csv Files (Pattern can be extended to get only data-csv file)
    
    unzip(
      zipfile = temp, overwrite = T,
      junkpaths = T, files = csvFiles, exdir = "temp"
    ) # Unzip the csv Files in the temp-file
    
    csvFilesPaths <- list.files("temp/", full.names = T) # Get the unzipped csv-Files in the temp-directory
    
    
    # To get only Valid CSV-Files and enable us to filter by DateTime of the File we can parse the files standard date-time string in the Pavlovia-Default FileNames
    dateTimeOfFiles <- tibble(filepaths = csvFilesPaths) %>%
      mutate(dateTime = str_extract(filepaths, "[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}h[0-9]{2}")) %>%
      filter(!is.na(dateTime)) %>%
      mutate(dateTime = parse_datetime(dateTime, "%Y-%m-%d_%Hh%M"))
    # %>%  filter(dateTime > parse_datetime("2019-02-01_15h00", "%Y-%m-%d_%Hh%M")) # This can be used to Filter by a specific time
    
    
    # Purrr Magic  - Thanks to https://clauswilke.com/blog/2016/06/13/reading-and-combining-many-tidy-data-files-in-r/
    
    # Now the read the desired data Files with purrr:
    data <- data_frame(filename = dateTimeOfFiles$filepaths) %>% # create a data frame
      # holding the file names
      mutate(
        file_contents = map(
          filename, # read files into
          ~ read_csv(file.path(.))
        ) # a new data column
      )
    
    # Unlink temp because we don't need it anymore
    unlink("temp/*")
    disable("downloadData")
    data
  })
  
  

  output$plotTest <- renderPlot({
    ggplot(dataMerged(), aes(y=resp.rt, x = congruent), color=participant) +
     stat_summary(geom="point",fun = "mean") + 
     stat_summary(geom="errorbar", fun.data = mean_se)
  })
  
  # Table of selected dataset ----
  output$dataOverview <- DT::renderDataTable({
    
    DT::datatable(  datasetInput() %>%
                      rowwise() %>%
                      mutate(participant = file_contents$participant[1], fileDim = paste0("Rows:", dim(file_contents)[1], " Vars:", dim(file_contents)[2])[1]) %>%
                      select(-file_contents)
                    , options = list(scrollX = TRUE))
    
  })
  
  observeEvent(input$password, {
    if (input$password != "example") hide("table") else show("table")
  })
  observeEvent(input$password, {
    req(datasetInput())
    if (input$password != "example") disable("downloadData") else enable("downloadData")
  })
  
  dataMerged <- reactive({
    # Read in all available data in a single tibble
    datasetInput() %>% select(file_contents) %>% # remove filenames, not needed anynmore
      unnest(cols = c(file_contents))
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Test_Data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataMerged(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
