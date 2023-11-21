library(shiny)
library(DT)
library(sf)
library(leaflet)

# USER INTERFACE ---------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Easy map!"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "countrySelect",
        label = "Select pre-loaded country",
        choices = c("None", "AGO", "COD")
      ),
      fileInput(
        inputId = 'file1',
        label = "Or upload an .rds sf spatial file",
        accept = c(".rds")
      ),
      uiOutput(
        outputId = "columnSelect"
      )
    ),
    mainPanel(
      DTOutput(
        outputId = "editableTable"
      ),
      leafletOutput(
        outputId = "map"
      )
    )
  )
)

# ------------------------------------------------------------------------------

# BACK END ---------------------------------------------------------------------
server <- function(input, output, session) {
  
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
    df <- reactiveDF$spatial
    if (is.null(df)) return()
    selectedColumn <- input$selectedColumn
    uniqueValues <- unique(df[[selectedColumn]])
    initialDF <- data.frame(Values = uniqueValues, Input = NA_real_)
    colnames(initialDF) <- c(selectedColumn, "Input")
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
    currentDF <- reactiveDF$table
    currentDF[info$row, info$col] <- info$value
    reactiveDF$table <- currentDF
  })
  
  # Render Map
  observe({
    output$map <- renderLeaflet({
      spatial_data <- reactiveDF$spatial
      table <- reactiveDF$table
      if (is.null(spatial_data) | is.null(table) | all(is.na(table$Input))) return(NULL)
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
