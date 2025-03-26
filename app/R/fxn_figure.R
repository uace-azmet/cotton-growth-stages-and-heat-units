#' `fxn_figure` generates bar chart of cumulative heat units of current and recent years with cotton growth stage labels
#' 
#' @param inData - data table of seasonal heat accumulation values by year
#' @return `figure` - png of figure

# https://plotly-r.com/ 
# https://plotly.com/r/reference/ 
# https://plotly.github.io/schema-viewer/
# https://github.com/plotly/plotly.js/blob/c1ef6911da054f3b16a7abe8fb2d56019988ba14/src/components/fx/hover.js#L1596
# https://www.color-hex.com/color-palette/1041718


fxn_figure <- function(inData) {
  inData <- inData %>% 
    dplyr::mutate(dateYear = as.factor(dateYear))
  
  dataCurrentYear <- inData %>% 
    dplyr::filter(
      dateYear == lubridate::year(lubridate::today(tz = "America/Phoenix"))
    )
  
  dataOtherYears <- inData %>% 
    dplyr::filter(
      dateYear != lubridate::year(lubridate::today(tz = "America/Phoenix"))
    )
  
  
  
  figure <- 
    plotly::plot_ly() %>% 
    
    plotly::add_trace( # Bars for `dataOtherYears`
      data = dataOtherYears,
      x = ~dateYear,
      y = ~heatSum,
      type = "bar",
      marker = list(color = "#989898"),
      showlegend = FALSE,
      text = ~heatSum,
      textposition = "auto",
      name = "yaxis1",
      yaxis = "y1"
    ) %>% 
    
    plotly::add_trace( # Bar for `dataCurrentYear`
      inherit = FALSE,
      data = dataCurrentYear,
      x = ~dateYear,
      y = ~heatSum,
      type = "bar",
      marker = list(color = "#191919"),
      showlegend = FALSE,
      text = ~heatSum,
      textposition = "auto",
      name = "yaxis2",
      yaxis = "y2"
    ) %>%
    
    plotly::config(
      displaylogo = FALSE,
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c(
        "autoScale2d",
        "hoverClosestCartesian", 
        "hoverCompareCartesian", 
        "lasso2d",
        "select"
      ),
      scrollZoom = FALSE,
      toImageButtonOptions = list(
        format = "png", # Either png, svg, jpeg, or webp
        filename = "AZMet-data-viewer-15minute-station-level-summaries",
        height = 400,
        width = 700,
        scale = 5
      )
    ) %>%
    
    plotly::layout(
      font = list(
        color = "#191919",
        family = "proxima-nova, calibri, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, \"Noto Sans\", sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\"",
        size = 13
      ),
      margin = list(
        l = 0,
        r = 150, # For space between plot and modebar
        b = 80, # For space between x-axis title and caption or figure help text
        t = 0,
        pad = 3 # For space between gridlines and yaxis labels
      ),
      modebar = list(
        bgcolor = "#FFFFFF",
        orientation = "v"
      ),
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
          )
        ),
      xaxis = list(
        linewidth = 0,
        title = list(
          font = list(size = 14),
          standoff = 25,
          text = "Year"
        ),
        zeroline = FALSE
      ),
      yaxis = list(
        gridcolor = "#c9c9c9",
        ticktext = list("0", "700", "1200", "1500", "1800", "2200", "2400", "2800", "3000", "3400"),
        tickvals = list(0, 700, 1200, 1500, 1800, 2200, 2400, 2800, 3000, 3400),
        title = list(
          font = list(size = 14),
          standoff = 25,
          text = "Degree Days Fahrenheit"
        ),
        zeroline = TRUE,
        zerolinecolor = "#c9c9c9"
      ),
      yaxis2 = list(
        gridcolor = "#c9c9c9",
        matches = "y",
        ticktext = list(
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
        overlaying = "y",
        side = "right",
        zeroline = TRUE,
        zerolinecolor = "#c9c9c9"
      )
    ) %>% 
    
    plotly::style(
      hoverinfo = "none"
    )
  
  figure
  
  
  
  
  
  toDelete <- ggplot2::ggplot(
    #data = inData, 
    data = dataMerge, 
    mapping = aes(x = as.factor(.data$dateYear), y = .data$heatSum)
  ) +
    
    # Cotton growth stages as cumulative heat units (https://www.color-hex.com/color-palette/1041718)
    geom_rect(xmin = -Inf, xmax = Inf, ymin = 1800, ymax = 2200, alpha = 0.1, fill = "#989898") + # Peak Bloom
    geom_rect(xmin = -Inf, xmax = Inf, ymin = 2400, ymax = 2800, alpha = 0.1, fill = "#989898") + # Cutout
    geom_rect(xmin = -Inf, xmax = Inf, ymin = 3000, ymax = 3400, alpha = 0.1, fill = "#989898") + # Terminate
    
    geom_hline( # Cotton growth stages as cumulative heat units
      data = dataCottonGrowthStages, 
      mapping = aes(yintercept = huapValue), 
      alpha = 1.0, color = "#989898", linetype = "solid", linewidth = 0.3
    ) +
    
    geom_col( # Previous growing season
      data = dataOtherYears, 
      mapping = aes(x = as.factor(.data$dateYear), y = .data$heatSum), 
      alpha = 1.0, fill = "#989898"
    ) +
    
    geom_col( # Current growing season
      data = dataCurrentYear, 
      mapping = aes(x = as.factor(.data$dateYear), y = .data$heatSum), 
      alpha = 1.0, fill = "#191919"
    ) +
    
    #geom_label( # Previous growing season
    #  data = dplyr::filter(inData, inData$dateYear < max(inData$dateYear)), 
    #  mapping = aes(label = .data$heatSumLabel, fontface = "bold"), 
    #  color = "#989898", fill = "#FFFFFF", label.size = NA, size = 3.5, vjust = -0.1
    #) +
    
    #geom_label( # Current growing season
    #  data = dplyr::filter(inData, inData$dateYear == max(inData$dateYear)), 
    #  mapping = aes(label = .data$heatSumLabel, fontface = "bold"), 
    #  color = "#191919", fill = "#FFFFFF", label.size = NA, size = 3.5, vjust = -0.1
    #) + 
    
    labs(x = "\nYear", y = "Degree Days Fahrenheit\n") +
    
    scale_y_continuous(
      breaks = dataCottonGrowthStages$huapValue, 
      labels = dataCottonGrowthStages$huapValue,
      expand = expansion(mult = c(0.01, 0.05)),
      sec.axis = (dup_axis(labels = dataCottonGrowthStages$growthStage))
    ) +
    
    theme_minimal() +
    
    theme( # https://ggplot2.tidyverse.org/reference/theme.html
      #line,
      #rect,
      text = element_text(family = "sans"),
      #title,
      #aspect.ratio,
      axis.title = element_text(
        color = "#989898", face = "plain", size = 10, hjust = 0.0, 
        margin = margin(t = 0.2, r = 0, b = 0, l = 0, unit = "cm")
      ),
      #axis.title.x,
      #axis.title.x.top,
      #axis.title.x.bottom,
      #axis.title.y,
      #axis.title.y.left,
      axis.title.y.right = element_blank(),
      axis.text = element_text(color = "#989898", face = "plain", size = 10),
      #axis.text.x,
      #axis.text.x.top,
      #axis.text.x.bottom,
      #axis.text.y = element_text(color = "#343a40", face = "plain", size = 10),
      #axis.text.y,
      #axis.text.y.left,
      #axis.text.y.right,
      #axis.ticks,
      #axis.ticks.x,
      #axis.ticks.x.top,
      #axis.ticks.x.bottom,
      #axis.ticks.y,
      #axis.ticks.y.left,
      #axis.ticks.y.right,
      #axis.ticks.length,
      #axis.ticks.length.x,
      #axis.ticks.length.x.top,
      #axis.ticks.length.x.bottom,
      #axis.ticks.length.y,
      #axis.ticks.length.y.left,
      #axis.ticks.length.y.right,
      #axis.line,
      #axis.line.x,
      #axis.line.x.top,
      #axis.line.x.bottom,
      #axis.line.y,
      #axis.line.y.left,
      #axis.line.y.right,
      #legend.background,
      #legend.margin,
      #legend.spacing,
      #legend.spacing.x,
      #legend.spacing.y,
      #legend.key,
      #legend.key.size,
      #legend.key.height,
      #legend.key.width,
      #legend.text,
      #legend.text.align,
      #legend.title,
      #legend.title.align,
      #legend.position,
      #legend.direction,
      #legend.justification,
      #legend.box,
      #legend.box.just,
      #legend.box.margin,
      #legend.box.background,
      #legend.box.spacing,
      #panel.background,
      #panel.border,
      #panel.spacing,
      #panel.spacing.x,
      #panel.spacing.y,
      #panel.grid,
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
      #panel.grid.major.x,
      #panel.grid.major.y,
      #panel.grid.minor.x,
      #panel.grid.minor.y,
      #panel.ontop,
      #plot.background,
      #plot.title
      #plot.title.position
      #plot.subtitle
      #plot.caption,
      #plot.caption.position,
      #plot.tag,
      #plot.tag.position,
      #plot.margin,
      #strip.background,
      #strip.background.x,
      #strip.background.y,
      #strip.clip,
      #strip.placement,
      #strip.text,
      #strip.text.x,
      #strip.text.y,
      #strip.switch.pad.grid,
      #strip.switch.pad.wrap,
      #...,
      #complete = FALSE,
      #validate = TRUE
    )
  #toDelete
  
  #plotly::ggplotly(toDelete)
  
  
  
  return(figure)
}
