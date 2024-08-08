# Calculate growing-season heat accumulation relative to cotton development

# Libraries
library(azmetr)
library(dplyr)
library(ggplot2)
library(htmltools)
library(lubridate)
library(shiny)
library(vroom)

# Functions
#source("./R/fxnABC.R", local = TRUE)

# Scripts
#source("./R/scr##DEF.R", local = TRUE)


# UI --------------------

ui <- htmltools::htmlTemplate(
  
  "azmet-shiny-template.html",
  
  sidebarLayout = sidebarLayout(
    position = "left",
    
    sidebarPanel(
      id = "sidebarPanel",
      width = 4,
      
      verticalLayout(
        helpText(em(
          "Select an AZMet station and set dates for planting and the end of the period of interest. Then, click or tap 'CALCULATE HEAT UNITS'."
        )),
        
        br(),
        selectInput(
          inputId = "azmetStation", 
          label = "AZMet Station",
          choices = azmetStations[order(azmetStations$stationName), ]$stationName,
          selected = "Aguila"
        ),
        
        dateInput(
          inputId = "plantingDate",
          label = "Planting Date",
          value = initialPlantingDate,
          min = initialPlantingDate - 31, # January 1 of current growing season
          max = Sys.Date() - 1,
          format = "MM d, yyyy",
          startview = "month",
          weekstart = 0, # Sunday
          width = "100%",
          autoclose = TRUE
        ),
        
        dateInput(
          inputId = "endDate",
          label = "End Date",
          value = initialEndDate,
          min = initialPlantingDate - 31, # January 1 of current growing season,
          max = initialEndDate,
          format = "MM d, yyyy",
          startview = "month",
          weekstart = 0, # Sunday
          width = "100%",
          autoclose = TRUE
        ),
        
        br(),
        actionButton(
          inputId = "calculateHeatUnits", 
          label = "CALCULATE HEAT UNITS",
          class = "btn btn-block btn-blue"
        )
      )
    ), # sidebarPanel()
    
    mainPanel(
      id = "mainPanel",
      width = 8,
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "figureTitle"))
      ), 
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "figureSubtitle"))
      ),
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, plotOutput(outputId = "figure"))
      ), 
      
      br(),
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "figureSubtext"))
      ),
      
      br(), br(),
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "figureFooterHelpText"))
      ),
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "figureFooter"))
      ),
      
      br()
    ) # mainPanel()
  ) # sidebarLayout()
) # htmltools::htmlTemplate()


# Server --------------------

server <- function(input, output, session) {
  
  # Reactive events -----
  
  # AZMet heat-unit accumulation data
  dataAZMetDataMerge <- eventReactive(input$calculateHeatUnits, {
    validate(
      need(expr = input$plantingDate <= input$endDate, message = FALSE)
    )
    
    idCalculatingHeatUnits <- showNotification(
      ui = "Calculating heat units . . .", 
      action = NULL, 
      duration = NULL, 
      closeButton = FALSE,
      id = "idCalculatingHeatUnits",
      type = "message"
    )
    
    on.exit(removeNotification(id = idCalculatingHeatUnits), add = TRUE)
    
    # Calls calls 'fxnAZMetDataELT()' and 'fxnAZMetDataHeatSum()'
    fxnAZMetDataMerge(
      azmetStation = input$azmetStation, 
      startDate = input$plantingDate, 
      endDate = input$endDate
    )
  })
  
  # Build figure
  figure <- eventReactive(dataAZMetDataMerge(), {
    fxnFigure(
      azmetStation = input$azmetStation,
      inData = dataAZMetDataMerge(), 
      startDate = input$plantingDate, 
      endDate = input$endDate
    )
  })
  
  # Build figure footer
  figureFooter <- eventReactive(dataAZMetDataMerge(), {
    fxnFigureFooter(
      azmetStation = input$azmetStation,
      startDate = input$plantingDate, 
      endDate = input$endDate, 
      timeStep = "Daily"
    )
  })
  
  # Build figure footer help text
  figureFooterHelpText <- eventReactive(dataAZMetDataMerge(), {
    fxnFigureFooterHelpText()
  })
  
  # Build figure subtext
  figureSubtext <- eventReactive(dataAZMetDataMerge(), {
    fxnFigureSubtext(
      azmetStation = input$azmetStation,
      startDate = input$startDate, 
      endDate = input$endDate
    )
  })
  
  # Build figure subtitle
  figureSubtitle <- eventReactive(dataAZMetDataMerge(), {
    fxnFigureSubtitle(
      azmetStation = input$azmetStation, 
      inData = dataAZMetDataMerge(),
      startDate = input$plantingDate, 
      endDate = input$endDate)
  })
  
  # Build figure title
  figureTitle <- eventReactive(input$calculateHeatUnits, {
    validate(
      need(
        expr = input$plantingDate <= input$endDate, 
        message = "Please select a 'Planting Date' that is earlier than or the same as the 'End Date'."
      ),
      errorClass = "datepicker"
    )
    
    fxnFigureTitle(inData = dataAZMetDataMerge(), endDate = input$endDate)
  })
  
  # Outputs -----
  
  output$figure <- renderPlot({
    figure()
  }, res = 96)
  
  output$figureFooter <- renderUI({
    figureFooter()
  })
  
  output$figureFooterHelpText <- renderUI({
    figureFooterHelpText()
  })
  
  output$figureSubtext <- renderUI({
    figureSubtext()
  })
  
  output$figureSubtitle <- renderUI({
    figureSubtitle()
  })
  
  output$figureTitle <- renderUI({
    figureTitle()
  })
}

# Run --------------------

shinyApp(ui = ui, server = server)
