

# Calculate growing-season heat accumulation relative to cotton development

# Add code for the following
# 
# 'azmet-shiny-template.html': <!-- Google tag (gtag.js) -->
# 'azmet-shiny-template.html': <!-- CSS specific to this AZMet Shiny app -->

# Edit the following [in square brackets]:
# 
# 'azmet-shiny-template.html': <article role="article" about="[/application-areas]" class="node node--type-az-flexible-page node--view-mode-full clearfix">


# Libraries
library(azmetr)
library(dplyr)
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
          "Select an AZMet station and set dates for planting and the end of the period of interest. Then, click or tap 'CALCULATE HEAT ACCUMULATION'."
        )),
        
        br(),
        selectInput(
          inputId = "azmetStation", 
          label = "AZMet Station",
          choices = stationNames[order(stationNames$stationName), ]$stationName,
          selected = "Aguila"
        ),
        
        dateInput(
          inputId = "plantingDate",
          label = "Planting Date",
          value = initialPlantingDate,
          min = initialPlantingDate,
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
          min = initialPlantingDate + 1,
          max = initialEndDate,
          format = "MM d, yyyy",
          startview = "month",
          weekstart = 0, # Sunday
          width = "100%",
          autoclose = TRUE
        ),
        
        br(),
        actionButton(
          inputId = "calculateHeatAccumulation", 
          label = "CALCULATE HEAT ACCUMULATION",
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
      
      #br(),
      #fluidRow(
      #  column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "tableHelpText"))
      #),
      
      #fluidRow(
      #  column(width = 11, align = "left", offset = 1, plotOutput(outputId = "figureGrowthStage"))
      #), 
      
      #br(), br(),
      #fluidRow(
      #  column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "figureCaption"))
      #),
      
      #fluidRow(
      #  column(width = 11, align = "left", offset = 1, uiOutput(outputId = "downloadButtonTSV"))
      #),
      
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
  
  # AZMet data ELT
  dAZMetData <- eventReactive(input$calculateHeatAccumulation, {
    validate(
      need(
        input$plantingDate <= input$endDate, 
        "Please select a 'Planting Date' that is earlier than or the same as the 'End Date'."
      ),
      errorClass = "datepickerBlank"
    )
    
    idCalculatingHeatAccumulation <- showNotification(
      ui = "Calculating heat accumulation . . .", 
      action = NULL, 
      duration = NULL, 
      closeButton = FALSE,
      id = "idCalculatingHeatAccumulation",
      type = "message"
    )
    
    on.exit(removeNotification(id = idCalculatingHeatAccumulation), add = TRUE)
    
    fxnAZMetDataELT(
      azmetStation = input$azmetStation, 
      timeStep = "Daily", 
      startDate = input$plantingDate, 
      endDate = input$endDate
    )
  })
  
  # Build figure caption
  #figureCaption <- eventReactive(dfAZMetData(), {
  #  fxnFigureCaption()
  #})
  
  # Build figure footer
  figureFooter <- eventReactive(dAZMetData(), {
    fxnFigureFooter(
      azmetStation = input$azmetStation,
      plantingDate = input$plantingDate, 
      endDate = input$endDate, 
      timeStep = "Daily"
    )
  })
  
  # Build table footer help text
  figureFooterHelpText <- eventReactive(dAZMetData(), {
    fxnFigureFooterHelpText()
  })
  
  # Build figure subtitle
  figureSubtitle <- eventReactive(dAZMetData(), {
    fxnFigureSubtitle(plantingDate = input$plantingDate, endDate = input$endDate)
  })
  
  # Build figure title
  figureTitle <- eventReactive(input$calculateHeatAccumulation, {
    validate(
      need(
        input$plantingDate <= input$endDate, 
        "Please select a 'Planting Date' that is earlier than or the same as the 'End Date'."
      ),
      errorClass = "datepicker"
    )
    
    figureTitle <- fxnFigureTitle(azmetStation = input$azmetStation)
  })
  
  # Outputs -----
  
  #output$figureCaption <- renderUI({
  #  figureCaption()
  #})
  
  output$figureSubtitle <- renderUI({
    figureSubtitle()
  })
  
  output$figureTitle <- renderUI({
    figureTitle()
  })
  
  output$figureFooter <- renderUI({
    figureFooter()
  })
  
  output$figureFooterHelpText <- renderUI({
    figureFooterHelpText()
  })
}

# Run --------------------

shinyApp(ui = ui, server = server)
