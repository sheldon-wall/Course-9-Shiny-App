library(shiny)
library(plotly)
library(dplyr)
library(tidyr)
library(lubridate)
library(countrycode)

## read the terrorism file
dataDir <- "./data" 
filename <- file.path(dataDir, "globalterrorismdb_0617dist.csv")
terror_activity <- read.csv(filename,na.strings = c("NA","#DIV/0!",""))

## clean up missing values
terror_activity$nkill[is.na(terror_activity$nkill)] <- 0
terror_activity$nwound[is.na(terror_activity$nwound)] <- 0
terror_activity$propextent[is.na(terror_activity$propextent)] <- 0

## create a subset relevant information for plotting 
terror_subset <- 
  terror_activity %>% 
  mutate(event_date = ymd(eventid / 10000)) %>%
  mutate(intensity = (nkill * 10) + (nwound * 5) + 
           ((4 - if_else (terror_activity$propextent == 0, 4,terror_activity$propextent)) * 10 )) %>% 
  mutate(country_iso3 = countrycode(country_txt, "country.name", "iso3c")) %>%
  select(event_date, intensity, 
         country_txt, country_iso3, region_txt, city, country_iso3,
         attacktype1_txt, targtype1_txt, weaptype1_txt, weapsubtype1_txt, 
         gname, gsubname, nperps, 
         nkill, nwound, propextent, propextent_txt) %>%
  filter(nkill > 0 | nwound > 0 | propextent > 0)

## plotting margin and axis values
m <- list(l = 50, r = 70, b = 120, t = 70, pad = 0) 
ay <- list(tickfont = list(color = "red"), overlaying = "y",side = "right",
           title = "Event Frequency")

ui <- fluidPage(
  titlePanel("Plotting with Shiny using Global Terrorism Database"),
  a(href="http://start.umd.edu/gtd/", "Global Terrorism Database"),
  p("This dataset was sourced from the Global Terrorism Database and was retrieved Nov. 19th, 2017.  It is used to examine 170K terrorist activities throughout the world from 1970 - 2016 using plotly.  It is plotting intensity (y1) and frequency (y2) of events summarized by your selection."),
  p("Select summarization options below and click (Apply) for replot.  Click on a point to see details of damage intensity distribution"),
  fluidRow(
    column(3, wellPanel(
      h4("Select these values for plotting choices"),
      radioButtons("inRadio", "Summarize Option:",
                   c("Yearly" = "yearly",
                     "Monthly" = "monthly",
                     "None" = "none"),
                   selected = ),
      dateRangeInput("Event_Range",
                     "Select the date range:",
                     start='1970-01-01',end='2016-12-31',
                     min='1970-01-01',max = '2016-12-31',
                     format = 'yyyy-mm'),
      submitButton("Apply Changes"),
      h4("Click on Plot value for further event information")
    )),
    column(9, plotlyOutput("plot")),
    column(9, plotlyOutput("barchart"), offset = 3)
  )
)

server <- function(input, output) 
{
  
  ## Set the format for summarizing 
  format_dt <- reactive({
    if (input$inRadio == "none") {
      c('%Y-%m-%d')
    } else  { 
      if (input$inRadio == "monthly") {
        c('%Y-%m')
      } else c('%Y')
    }
  })
  
  ## Set the start date
  start_date <- reactive({
    input$Event_Range[1]
  })

  ## Set the end date
  end_date <- reactive({
    input$Event_Range[2]
  })
    
  # A reactive expression with the plot_ly plot
  output$plot <- renderPlotly({
    ## filter and plot
    terror_subset %>%
      filter(event_date >= start_date()) %>%
      filter(event_date <= end_date()) %>%
      mutate(event_date= format(event_date, format_dt())) %>%
      group_by(event_date) %>%
      summarise(avg_int = mean(intensity), n = n()) %>%
      plot_ly(x = ~event_date, source = "scatterplot") %>%
        add_markers(y = ~n, name = "avg freqency", yaxis = "y2") %>%
        add_markers(y = ~avg_int, name = "avg intensity", type = 'bar') %>%
        layout(title = 'Comparing average intensity and frequency of terrorist events',
               margin = m, xaxis = list(title = 'Event Date'),
               yaxis = list(title = 'Intensity (log)', type = 'log'),
               yaxis2 = ay, legend = list(x = 0.05, y = 0.95))
  })

  ## create a linked bar chart which shows intensity distribution
  output$barchart <- renderPlotly({
    s <- event_data("plotly_click", source = "scatterplot")
    if (length(s)) {
      selected_date <- c(s[["x"]])
      chart_title <- sprintf("Intensity by Damage Type for %s", selected_date)
      terror_subset %>% 
        mutate(event_date = format(event_date, format_dt())) %>%
        filter(event_date == selected_date) %>%
        transmute(event_date, intensity, 
                  nkill = nkill * 4, 
                  nwound = nwound * 5, 
                  propextent = (4 - if_else (propextent == 0, 4,propextent)) * 10) %>%
        group_by(event_date) %>%
        summarise(num_killed = mean(nkill), num_wounded = mean(nwound), 
                  prop_damage = mean(propextent)) %>%
        select(num_killed, num_wounded, prop_damage) %>%
        gather(key = damage_type, factor_key = TRUE, value = intensity) %>%
        plot_ly(x = ~damage_type, y = ~intensity, type = "bar") %>%
        layout(title = chart_title, margin = m, xaxis = list(title = 'Damage Type'),
            yaxis = list(title = 'Avg Intensity'))
    } else {
      plotly_empty()
    }
  })
    
}

shinyApp(ui, server)

