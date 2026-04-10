# Use cumulative heat units to estimate cotton growth stages by station and date range


# UI --------------------


ui <- 
  htmltools::htmlTemplate(
    
    filename = "azmet-shiny-template.html",
    
    pageFluid = 
      bslib::page_fluid(
        title = NULL,
        theme = theme, # `scr##_theme.R`
        
        bslib::layout_sidebar(
          sidebar = sidebar, # `scr##_sidebar.R`
          
          shiny::htmlOutput(outputId = "barChartTitle"),
          shiny::htmlOutput(outputId = "barChartSummary"),
          plotly::plotlyOutput(outputId = "barChart"),
          shiny::htmlOutput(outputId = "barChartCaption")
        ) |>
          htmltools::tagAppendAttributes(
            #https://getbootstrap.com/docs/5.0/utilities/api/
            class = "border-0 rounded-0 shadow-none"
          ),
        
        shiny::htmlOutput(outputId = "pageBottomText")
      )
    ) # htmltools::htmlTemplate()


# Server --------------------


server <- function(input, output, session) {
  
  
  # Observables -----
  
  # To update available dates based on selected station
  shiny::observeEvent(input$azmetStation, {
    stationStartDate <-
      dplyr::filter(azmetStationMetadata, meta_station_name == input$azmetStation) %>% 
      dplyr::pull(start_date)
    
    stationStartDateMinimum <- stationStartDate
    stationEndDateMinimum <- stationStartDate
    
    if (stationStartDate > input$startDate) {
      stationStartDateSelected <- stationStartDate
    } else {
      stationStartDateSelected <- input$startDate
    }
    
    if (stationStartDate > input$endDate) {
      stationEndDateSelected <- stationStartDate
    } else {
      stationEndDateSelected <- input$endDate
    }
    
    shiny::updateDateInput(
      inputId = "startDate",
      label = "Start Date",
      value = stationStartDateSelected,
      min = stationStartDateMinimum,
      max = Sys.Date() - 1
    )
    
    shiny::updateDateInput(
      inputId = "endDate",
      label = "End Date",
      value = stationEndDateSelected,
      min = stationEndDateMinimum,
      max = Sys.Date() - 1
    )
  })
  
  # Catch input errors before data download, show error modal
  shiny::observeEvent(input$calculateHeatUnits, {
    if (input$startDate > input$endDate) {
      shiny::showModal(datepickerErrorModal) # `scr##_datepickerErrorModal.R`
    }
    
    if (
      input$azmetStation == "Yuma N.Gila" & 
      lubridate::int_overlaps(
        int1 = yugNodataInterval, 
        int2 = lubridate::interval(input$startDate, input$endDate)
      ) == TRUE
    ) {
      shiny::showModal(datepickerYumaNGilaErrorModal) # `scr##_datepickerYumaNGilaErrorModal.R`
    }
  })
  
  
  # Reactives -----
  
  totalHeatUnits <-
    shiny::eventReactive(input$calculateHeatUnits, {
      shiny::validate(
        shiny::need(
          expr = input$startDate <= input$endDate,
          message = FALSE # Failing validation test
        ),
        shiny::need(
          expr = 
            !(input$azmetStation == "Yuma N.Gila" &
                lubridate::int_overlaps(
                  int1 = yugNodataInterval, 
                  int2 = lubridate::interval(input$startDate, input$endDate)
                )
            ),
          message = FALSE # Failing validation test
        )
      )

    idCalculatingHeatUnits <-
      shiny::showNotification(
        ui = "Calculating heat units . . .",
        action = NULL,
        duration = NULL,
        closeButton = FALSE,
        id = "idCalculatingHeatUnits",
        type = "message"
      )

    on.exit(removeNotification(id = idCalculatingHeatUnits), add = TRUE)

    fxn_totalHeatUnits( # Calls 'fxn_azDaily()' and 'fxn_dataHeatUnitsSeasonal()'
      azmetStation = input$azmetStation,
      startDate = input$startDate,
      endDate = input$endDate
    )
  })
  
  barChart <-
    shiny::eventReactive(totalHeatUnits(), {
      fxn_figure(
        inData = totalHeatUnits()[[2]],
        azmetStation = input$azmetStation
      )
    })

  barChartCaption <-
    shiny::eventReactive(totalHeatUnits(), {
      fxn_barChartCaption(
        azmetStation = input$azmetStation,
        inData = totalHeatUnits()[[2]],
        startDate = input$startDate,
        endDate = input$endDate
      )
    })

  barChartSummary <-
    shiny::eventReactive(totalHeatUnits(), {
      fxn_barChartSummary(
        azmetStation = input$azmetStation,
        inData = totalHeatUnits()[[2]],
        startDate = input$startDate,
        endDate = input$endDate
      )
    })

  barChartTitle <-
    shiny::eventReactive(totalHeatUnits(), {
      fxn_barChartTitle(azmetStation = input$azmetStation)
    })

  pageBottomText <-
    shiny::eventReactive(totalHeatUnits(), {
      fxn_pageBottomText(
        azmetStation = input$azmetStation,
        startDate = input$startDate,
        endDate = input$endDate
      )
    })
  
  
  # Outputs -----
  
  output$barChart <-
    plotly::renderPlotly({
      barChart()
    })

  output$barChartCaption <-
    shiny::renderUI({
      barChartCaption()
    })

  output$barChartSummary <-
    shiny::renderUI({
      barChartSummary()
    })

  output$barChartTitle <-
    shiny::renderUI({
      barChartTitle()
    })

  output$pageBottomText <-
    shiny::renderUI({
      pageBottomText()
    })
}


# Run --------------------


shinyApp(ui = ui, server = server)
