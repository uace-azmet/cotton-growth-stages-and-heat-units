#' `fxn_barChart` Generates bar chart of cumulative heat units of current and recent years with cotton-growth-stage labels
#' 
#' @param inData - Data table [[2]] from `fxn_totalHeatUnits.R`
#' @param azmetStation - User-specified AZMet station
#' @return `figure` - Plotly bar chart

# https://plotly-r.com/ 
# https://plotly.com/r/reference/ 
# https://plotly.github.io/schema-viewer/
# https://github.com/plotly/plotly.js/blob/c1ef6911da054f3b16a7abe8fb2d56019988ba14/src/components/fx/hover.js#L1596
# https://www.color-hex.com/color-palette/1041718


fxn_figure <- function(inData, azmetStation) {
  
  
  # Inputs -----
  
  dataCurrentYear <- inData %>% 
    dplyr::filter(end_date_year == max(end_date_year)) %>%
    dplyr::mutate(end_date_year = as.factor(end_date_year))
  
  dataOtherYears <- inData %>% 
    dplyr::filter(end_date_year != max(end_date_year)) %>% 
    dplyr::mutate(end_date_year = as.factor(end_date_year))
  
  layoutFontFamily <- 
    "proxima-nova, calibri, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, \"Noto Sans\", sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\""
  
  ticktext <- inData$date_year_label
  tickvals <- inData$end_date_year
  
  
  # Bar Chart -----
  
  barChart <- 
    plotly::plot_ly( # Bars for `dataOtherYears`
      data = dataOtherYears,
      x = ~end_date_year,
      y = ~total_heat_units_seasonal,
      marker = list(color = "#bfbfbf"),
      name = "other years",
      showlegend = FALSE,
      hoverinfo = "text",
      hovertext = 
        ~paste0(
          "<br><b>AZMet station:</b> ", azmetStation,
          "<br><b>Year:</b> ", date_year_label,
          "<br><b>Cumulative Heat Units:</b> ", total_heat_units_seasonal_label, " DDF"
        ),
      type = "bar",
      yaxis = "y1"
    ) %>% 
    
    plotly::add_trace( # Bar for `dataCurrentYear`
      inherit = FALSE,
      data = dataCurrentYear,
      x = ~end_date_year,
      y = ~total_heat_units_seasonal,
      marker = list(color = "#191919"),
      name = "current year",
      showlegend = FALSE,
      hoverinfo = "text",
      hovertext = 
        ~paste0(
          "<br><b>AZMet station:</b> ", azmetStation,
          "<br><b>Year:</b> ", date_year_label,
          "<br><b>Cumulative Heat Units:</b> ", total_heat_units_seasonal_label, " DDF"
        ),
      type = "bar",
      yaxis = "y2"
    ) %>%
    
    plotly::config(
      displaylogo = FALSE,
      displayModeBar = FALSE,
      modeBarButtonsToRemove = 
        c(
          "autoScale2d",
          "hoverClosestCartesian", 
          "hoverCompareCartesian", 
          "lasso2d",
          "select"
        ),
      scrollZoom = FALSE,
      toImageButtonOptions = 
        list(
          format = "png", # Either png, svg, jpeg, or webp
          filename = "AZMet-cotton-growth-stages-and-heat-units",
          height = 400,
          width = 700,
          scale = 5
        )
    ) %>%
    
    plotly::layout(
      font = list(color = "#191919", family = layoutFontFamily, size = 13),
      hoverlabel = list(font = list(family = layoutFontFamily, size = 14)),
      margin = 
        list(
          l = 0,
          r = 200, # For space between plot and modebar
          b = 0,
          t = 10, # For space to show `3400` tick
          pad = 3 # For space between gridlines and yaxis labels
        ),
      modebar = list(bgcolor = "#FFFFFF", orientation = "v"),
      shapes = 
        list(
          list(
            type = "rect",
            fillcolor = "#c9c9c9",
            line = list(width = 0),
            layer = "below",
            opacity = 0.5,
            x0 = 0,
            x1 = 1,
            xref = "paper",
            y0 = 1800, # Peak Bloom (Short)
            y1 = 2200, # Peak Bloom (Long)
            yref = "y"
          ),
          list(
            type = "rect",
            fillcolor = "#c9c9c9",
            line = list(width = 0),
            layer = "below",
            opacity = 0.5,
            x0 = 0,
            x1 = 1,
            xref = "paper",
            y0 = 2400, # Cutout (Short)
            y1 = 2800, # Cutout (Long)
            yref = "y"
          ),
          list(
            type = "rect",
            fillcolor = "#c9c9c9",
            line = list(width = 0),
            layer = "below",
            opacity = 0.5,
            x0 = 0,
            x1 = 1,
            xref = "paper",
            y0 = 3000, # Terminate (Short)
            y1 = 3400, # Terminate (Long)
            yref = "y"
          ),
          list( # To show 3400 line when bars are low
            type = "line",
            line = list(color = "#FFFFFF"),
            layer = "below",
            opacity = 0.0,
            x0 = 0,
            x1 = 1,
            xref = "paper",
            y0 = 3401,
            y1 = 3401,
            yref = "y"
          )
        ),
      xaxis = 
        list(
          fixedrange = TRUE,
          linewidth = 0,
          ticktext = ticktext,
          tickvals = tickvals,
          title = list(font = list(size = 14), standoff = 25, text = "<b>Year</b>"),
          zeroline = FALSE
        ),
      yaxis = 
        list(
          fixedrange = TRUE,
          gridcolor = "#c9c9c9",
          ticktext = list("0", "700", "1200", "1500", "1800", "2200", "2400", "2800", "3000", "3400"),
          tickvals = list(0, 700, 1200, 1500, 1800, 2200, 2400, 2800, 3000, 3400),
          title = list(font = list(size = 14), standoff = 25, text = "<b>Degree Days Fahrenheit (DDF)</b>"),
          zeroline = TRUE,
          zerolinecolor = "#c9c9c9"
        ),
      yaxis2 = 
        list(
          fixedrange = TRUE,
          gridcolor = "#c9c9c9",
          matches = "y",
          ticktext = 
            list(
              "Planting", 
              "Pinhead Square", 
              "First Flower", 
              "One-inch Boll", 
              "Peak Bloom (Short)", 
              "Peak Bloom (Long)", 
              "Cutout (Short)", 
              "Cutout (Long)", 
              "Terminate (Short)", 
              "Terminate (Long)"
            ),
          tickvals = list(0, 700, 1200, 1500, 1800, 2200, 2400, 2800, 3000, 3400),
          title = list(font = list(size = 14), standoff = 25, text = "<b>Cotton Growth Stage</b>"),
          overlaying = "y",
          side = "right",
          zeroline = TRUE,
          zerolinecolor = "#c9c9c9"
        )
    )
  
  return(barChart)
}
