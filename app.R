library(shiny)
library(DT)
library(sf)
library(leaflet)

# USER INTERFACE ---------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Data Frame Editor"),
  sidebarLayout(
    sidebarPanel(
      selectInput("countrySelect", "Select pre-loaded country", choices = c("None", "AGO", "COD")),
      fileInput('file1', 'Or upload an .rds sf spatial file',
                accept = c(
                  '.rds'
                )),
      
      uiOutput("columnSelect")
    ),
    mainPanel(
      DTOutput("editableTable"),
      leafletOutput("map")
    )
  )
)

# ------------------------------------------------------------------------------

# BACK END ---------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Reactive value for storing the data frame
  data <- reactiveVal()
  # Initialize reactiveValues to store the data frame
  reactiveDF <- reactiveValues(spatial = NULL, table = NULL)
  
  # Observe file upload
  observe({
    file <- input$file1
    if (is.null(file)) return(NULL)
    reactiveDF$spatial <- readRDS(file$datapath)
  })
  # Observe selection of pre-loaded spatial data
  observeEvent(input$countrySelect, {
    if(input$countrySelect != "None"){
      reactiveDF$spatial <- readRDS(paste0("data/", input$countrySelect, "_1.RDS"))
    }
  })
  
  # Update column selection
  output$columnSelect <- renderUI({
    df <- reactiveDF$spatial
    if (is.null(df)) return(NULL)
    selectInput("selectedColumn", "Choose Column", names(df))
  })
  
  observeEvent(input$selectedColumn, {
    df <- reactiveDF$spatial  # Assuming 'data()' is your uploaded data frame
    if (is.null(df)) return()
    
    selectedColumn <- input$selectedColumn
    uniqueValues <- unique(df[[selectedColumn]])
    initialDF <- data.frame(Values = uniqueValues, Input = NA_real_)
    colnames(initialDF) <- c(selectedColumn, "Input")
    # Initialize reactiveDF with initialDF
    reactiveDF$table <- initialDF
    
    
  })
  
  # Render editable table
  output$editableTable <- renderDT({
    df <- reactiveDF$spatial
    selCol <- input$selectedColumn
    if (is.null(df) || is.null(selCol)) return(data.frame())
    uniqueValues <- unique(df[[selCol]])
    
    editableDF <- data.frame(Values = uniqueValues, Input = NA_real_)
    datatable(editableDF, editable = TRUE)
  }, server = TRUE)
  
  # Update reactive data frame on table edit
  observeEvent(input$editableTable_cell_edit, {
    info <- input$editableTable_cell_edit
    
    # Retrieve the current data frame from reactiveDF
    currentDF <- reactiveDF$table
    
    # Update the value in the data frame based on the edited cell
    currentDF[info$row, info$col] <- info$value
    
    # Update the reactive data frame
    reactiveDF$table <- currentDF
  })
  
  # Render Plot
  observe({
    output$map <- renderLeaflet({
      spatial_data <- reactiveDF$spatial
      table <- reactiveDF$table
      # browser()
      if (is.null(spatial_data) | is.null(table) | all(is.na(table$Input))) return(NULL)
      # browser()
      
      leaflet_input <- dplyr::left_join(spatial_data, table)
      palette <- colorNumeric("Blues", domain = NULL)
      
      leaflet() %>% 
        addTiles() %>%
        leaflet::addPolygons(
          data = leaflet_input,
          stroke = TRUE,
          smoothFactor = 0.5,
          opacity = 1,
          fill = TRUE,
          weight = 1,
          fillOpacity = 0.9,
          label = paste(leaflet_input[["NAME_1"]], leaflet_input[["Input"]]),
          fillColor = ~palette(leaflet_input[["Input"]]),
          layerId = ~ leaflet_input[["NAME_1"]]
        )
    })
  })
}

# ------------------------------------------------------------------------------

# RUN --------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
# ------------------------------------------------------------------------------
