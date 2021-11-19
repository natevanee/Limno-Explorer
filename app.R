#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
## Load Packages --------------------------------------------------------------------------------------------------------
library(shiny)
library(plotly)
library(slider)
library(leaflet)
library(leafpop)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(shinythemes)
library(RColorBrewer)
#------------------------------------------------------------------------------------------------------------------------
## Load the data files and initial leaflet maps Set theme and watermark-----------------------------------------------------------------
source("load_files.R")
# Script for Making the ctd plots
source("2020_ctd_cast_plots.R")
# Function for the windrose plotting
source(file = "AppWindRose.R")


# watermark for each page
watermark <- h5("App Created by", 
                img(src = "Signature Blue.png", height = "35px"), "and Built with",
                img(src = "shiny.png", height = "30px"),
                "by",
                img(src = "RStudio-Logo-Blue-Gray.png", height = "30px"),
                ".")

# choose a default plotting scheme in ggplot2
theme_set(theme_minimal())

# Make nicer labels to display in the app---------------------------------------------------------------
# Set up some nicer looking labels for parameter names
nice_labels <- c(
  "discharge_cfs" = "Discharge (cfs)",
  "temp_c" = "Temperature C",
  "sp_cond_u_s" = "Specific Conductivity (uS)",
  "mean_centered_wl_cm" = "Water Level (cm)",
  "turbid_ntu" = "Turbidity (NTU)",
  "do_mg_l" = "Dissolved Oxygen (mg/L)",
  "JI_discharge_cfs" = "Jones Island Discharge (cfs)",
  "ph_lower" = "pH"
)

nice_cities <- c(
  "mke" = "Milwaukee",
  "shb" = "Sheboygan"
)

nice_parameters <- c(
  "tss" = "Total Suspended Solids (mg/L)",
  "discharge_cfs" = "Discharge (cfs)",
  "temp_c" = "Temperature C",
  "sp_cond_u_s" = "Specific Conductivity (uS)",
  "mean_centered_wl_cm" = "Water Level (cm)",
  "turbid_ntu" = "Turbidity (NTU)",
  "awnd_mph" = "Mean Wind (mph)",
  "prcp_in" = "Rain (mm)",
  "tmax_f" = "Max Air Temp (F)",
  "tmin_f" = "Min Air Temp (F)",
  "lunar_tide_power" = "Lunar Tide Power",
  "seiche_power" = "Seiche Power",
  "wsf5_mph" = "5 min Wind Gust Spd (mph)",
  "wdf5_d" = "5 min Wind Gust Dir (Degrees)"
)

nice_choices <- c(
    "SRP (ug/L)" = "srp",
    "DOP (ug/L)" = "dop",
    "Bioavailable-PP (ug/L)" = "bioavailable-pp",
    "Other-PP (ug/L)" = "other-pp",
    "Total Suspended Solids (mg/L)" = "tss",
    "Discharge (cfs)" = "discharge_cfs",
    "Temperature C" = "temp_c",
    "Specific Conductivity (uS)" = "sp_cond_u_s",
    "Water Level Change (cm)" = "mean_centered_wl_cm",
    "Turbidity (NTU)" = "turbid_ntu",
    "Mean Wind (mph)" = "awnd_mph",
    "Rain (mm)" = "prcp_in",
    "Max Air Temp (F)" = "tmax_f",
    "Min Air Temp (F)" = "tmin_f",
    "5 min Wind Gust Spd (mph)" = "wsf5_mph",
    "5 min Wind Gust Dir (Degrees)" = "wdf5_d"
  )


#------------------------------------------------------------------------------------------------------------------------
# Define UI for application
#########################################################################################################################
ui <- fluidPage(
    
    theme = shinytheme("journal"),
    titlePanel(title = div("Limno Explorer", 
                           img(src = "uwm-sfs-logo-wide.png",
                               heigt = "100px",
                               width = "267px",
                               align = "right"))),
    br(),
    fluidRow(
    column(width = 4,
    # Select date range to be plotted for entire App
    dateRangeInput("date", strong("Date Range"), start = as.character(min(hr_sonde$date)), end = "2020-10-18",
                   min = as.character(min(hr_sonde$date)), max = "2020-11-18"), #as.character(max(hr_sonde$date))
    
    HTML("Use the calandar tool above to filter the data set by dates."),
    ),
    
    column(width = 4,
    selectInput("location", label = strong("Location"),
                choices = c(
                  "Milwaukee" = "mke",
                  "Sheboygan" = "shb"
                ), multiple = TRUE,
                selected = "shb"),
    )
    ),
    ## Tab 1 Sonde Explorer--------------------------------------------------------------------------------------------------------------
    tabsetPanel(
        
        tabPanel("Sonde Explorer",   
                 # Sidebar to select parameters to plot (only high resolution data here, not daily summaries) 
                 sidebarLayout(
                     sidebarPanel(
                       
                       selectInput(inputId = "selections", label = strong("Select Parameters to Plot"),
                                   choices = c(
                                     "Discharge (cfs)" = "discharge_cfs",
                                     "Temperature C" = "temp_c",
                                     "Specific Conductivity (uS)" = "sp_cond_u_s",
                                     "Water Level (cm)" = "mean_centered_wl_cm",
                                     "Turbidity (NTU)" = "turbid_ntu",
                                     "Dissolved Oxygen (mg/L)" = "do_mg_l",
                                     "Jones Island Discharge (cfs)" = "JI_discharge_cfs",
                                     "pH" = "ph_lower"
                                   ),
                                   multiple = T,
                                   selected = hr_sonde$parameter[1]),
                      
                         # Check box to smooth the data with a rolling average
                         checkboxInput("Smooth",
                                       "Smooth Timeseries with a Rolling Average",
                                       FALSE),
                         
                         # Check box to highlight the peaks in the data
                         checkboxInput("Peaks",
                                       "Highlight Timeseries Peaks",
                                       FALSE),
                         
                         # Display only if the "Smooth Data" box is checked
                         conditionalPanel(condition = "input.Smooth == true",
                                          sliderInput("Window",
                                                      "Window Size (Hours):",
                                                      min = 0.5,
                                                      max = 12,
                                                      value = 4,
                                                      step = 0.5),
                                          HTML("Low values will retain more of the original data complexity. <br>
                               High values will create smoother curves")
                         ),
                         
                         # Display only if the "Highlight Peaks" box is checked
                         conditionalPanel(condition = "input.Peaks == true",
                                          sliderInput("Span",
                                                      "Window Size (Hours):",
                                                      min = 5,
                                                      max = 49,
                                                      value = 12,
                                                      step = 2,
                                                      animate = F),
                                          HTML("Low values will show short-term patterns and more peaks.
                              <br> High values will show longer-term patterns and fewer peaks")
                         ),
                         
                         # Add my watermark
                         br(), br(),
                         watermark
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         
                         plotlyOutput("tsPlot"), 
                         
                         br(), br(),
                         
                         plotlyOutput("sinaPlot")
                     ) # close main panel
                 ) # close sidebar layout
        ), # close tab 1
#     ) # close the tabSets
# ) # Close the user interface

    # Tab 2
    ## Tab 2 Isco Exploter--------------------------------------------------------------------------------------------------------------
tabPanel("Isco Explorer",   
         # Sidebar to select parameters to plot (Isco and daily summaries) 
         sidebarLayout(
           sidebarPanel(
             
             HTML("Stacked Area Chart"),
             selectInput(inputId = "plot_type", label = strong("Select Chart Type"),
                         choices = c(
                           "Raw Values (ug/L)" = "value",
                           "Proportions" = "proportion"
                         ),
                         multiple = F,
                         selected = "value"),
             
             selectInput(inputId = "pool", label = strong("Select Phosphorus Pool"),
                         choices = c(
                           "All" = "all",
                           "Dissolved" = "dissolved",
                           "Particulate" = "particulate",
                           "Bioavailable" = "bioavailable",
                           "Unavailable" = "unavailable"
                         )),
             
             br(),
             
             # Check box to show supplementary time series data
             checkboxInput("show_timeseries",
                           "Show Supplementary Times Series",
                           FALSE),
             
             # Check box to show biplot
             checkboxInput("show_biplot",
                           "Show Biplot of X Y Data",
                           FALSE),
             
             conditionalPanel(condition = "input.show_timeseries == true",
                              selectInput(inputId = "sup_ts", label = strong("Select Parameters to Plot"),
                                          choices = c(
                                            "Total Suspended Solids (mg/L)" = "tss",
                                            "Discharge (cfs)" = "discharge_cfs",
                                            "Temperature C" = "temp_c",
                                            "Specific Conductivity (uS)" = "sp_cond_u_s",
                                            "Water Level Change (cm)" = "mean_centered_wl_cm",
                                            "Turbidity (NTU)" = "turbid_ntu",
                                            "Mean Wind (mph)" = "awnd_mph",
                                            "Rain (mm)" = "prcp_in",
                                            "Max Air Temp (F)" = "tmax_f",
                                            "Min Air Temp (F)" = "tmin_f",
                                            "5 min Wind Gust Spd (mph)" = "wsf5_mph",
                                            "5 min Wind Gust Dir (Degrees)" = "wdf5_d"
                                          ),
                                          multiple = T,
                                          selected = "tss")
                             
                              ), # close the conditional panel
             
             conditionalPanel(condition = "input.show_biplot == true",
             
             HTML("Biplot"),
             selectInput(inputId = "x", label = strong("X Variable"),
                         choices = nice_choices,
                         multiple = F,
                         selected = "tss"),
             
             selectInput(inputId = "y", label = strong("Y Variable"),
                         choices = nice_choices,
                         multiple = F,
                         selected = "bioavailable-pp"),
             
             checkboxInput(inputId = "lag_x", 
                           label = "Lag X Variable",
                           FALSE),
             
             conditionalPanel(condition = "input.lag_x == true",
                              
                              sliderInput(inputId = "lag_value",
                                          label = "Lag Time (Days)",
                                          min = 1,
                                          max = 7,
                                          step = 1,
                                          value = 1))
             
             ), # close conditonal panel
             
             

             # Add my watermark
             br(), br(),
             watermark
           ),
           
           # Show a plot of the generated distribution
           mainPanel(
             
             plotlyOutput("isco_plot"), 
             
             
             conditionalPanel(condition = "input.show_timeseries == true",
                              plotlyOutput("sup_daily_ts")
             ), # close conditional panel
             
             conditionalPanel(condition = "input.show_biplot == true",
             plotlyOutput("biplot")
             ), # close conditional panel
           ) # close main panel
         ) # close sidebar layout
), # close tab 2
#     ) # close the tabSets
# ) # Close the user interface
    ## Tab 3 - leaflet map survey explorer--------------------------------------------------------------------------------------
        
        tabPanel("Survey Explorer",
                 sidebarLayout(
                     sidebarPanel(

                         selectInput(inputId = "surveys", label = strong("Select Survey Dates to Plot"),
                                     choices = unique(nearshore$Date),
                                     multiple = T,
                                     selected = unique(nearshore$Date)[2]),

                         selectInput(inputId = "parameter", label = strong("Select A Parameter to Plot"),
                                     choices = c("SRP", "TDP", "DOP", "PP", "NaOH P", "TSS", "Chl a"),
                                     multiple = F,
                                     selected = "TSS"),

                         # Check box to show ctd data
                         checkboxInput("ctd",
                                       "Show Available CTD Casts",
                                       FALSE),

                         # Check box to dodge surface and bottom points that overlap
                         checkboxInput("dodge",
                                       "Dodge Overlapping Points",
                                       TRUE),

                         # Display only if the "Smooth Data" box is checked
                         conditionalPanel(condition = "input.dodge == true",
                                          sliderInput("dodge_amount",
                                                      "Dodge Amount (Meters):",
                                                      min = 50,
                                                      max = 250,
                                                      value = 100,
                                                      step = 50),
                                          HTML("Surface measurements on top <br>
                               Bottom measurements on bottom")
                         ),

                         # Add my watermark
                         br(), br(),
                         watermark
                     ), # close the sidebar panel

                     mainPanel(
                         leafletOutput("survey_map", height = "95vh")
                     ) # close the main panel
                 ) # Close sidebar layout
        ), # Close tab 3
 #   ) # close the tabSets
#) # Close the user interface

     
    ## Tab 4 - leaflet map weather explorer (rain and wind data)---------------------------------------------------------

tabPanel("Weather Explorer",
         div(class="outer",
     
             # sidebarLayout(mainPanel,
             #               sidebarPanel,
             #               position = c("left", "right"),
        mainPanel(width = 8,
            HTML("Map shows cummilative precipitation between the dates set with the calandar above. <br>
                 Data are retrived from the <a href='https://synopticdata.com/mesonet-api'>Synoptic Data Mesonet API.</a> <br> 
                 Please keep date ranges short (< 1 month) to minimize API usage"),
            h3("Rain Map"),
            leafletOutput("rain_map", height = "95vh")),
        
        sidebarPanel(position = "right", width = 4,
                     
                     checkboxInput(inputId = "land_use", 
                                   label = strong("Show Land Use")),
                 
            # absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
            #               draggable = TRUE, top = 200, left = "auto", right = 0, bottom = "auto",
            #               width = "400px", height = "auto",
         
                          h3("Wind Trends"),
             # Change the bin resolution
             sliderInput(inputId = "dirres", label = "Degrees per Bin:",
                         min = 20, max = 90, value = 30, step = 10),
       
             # Select whether to subset data with facet wrap
             checkboxInput(inputId = "Monthly", 
                           label = strong("Subset Windrose Data"), 
                           value = FALSE),
         
         plotOutput("windrose", height = 400),
         plotOutput("wind_ts", height = 150),
                 # Add my watermark
                 br(), br(),
                 watermark
           # ) # close absolute panel
        )
   # ) # close the sidebar Layout
   ) # close div outer
  ) # Close tab 4 
 ) # close the tabSets
) # Close the user interface ############################################################################################


# Define server #########################################################################################################
server <- function(input, output) {

    ## Sonde Time Series ----------------------------------------------------------------------------------------------------    
    # For Multiple Sonde Timeseries Plotting
    
    many_selections <- reactive({
        req(input$date)
        req(input$location)
        shiny::validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
        shiny::validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
        shiny::validate(need(length(input$location) >= 1, "Error: Please select a location"))
        
        if(input$Smooth) {
            hr_sonde %>%
                filter(
                    parameter %in% input$selections,
                    date >= as_date(input$date[1]) & date <= as_date(input$date[2],
                    )) %>%
                filter(location %in% input$location) %>%
                group_by(location, parameter) %>%
                mutate(measurement = slide_dbl(.x=measurement, .f=mean, .before = input$Window*2, .after = input$Window*2)) %>%
                ungroup() } else {
                    hr_sonde %>%
                        filter(
                            parameter %in% input$selections,
                            date >= as_date(input$date[1]) & date <= as_date(input$date[2],
                            )) %>%
                    filter(location %in% input$location)
                }
        
        
    })
    
    output$tsPlot <- renderPlotly({
        ggplotly(
            many_selections() %>%
                ggplot(aes(x=date_time, y=measurement, col=location)) +
                geom_line() +
                ggsci::scale_color_uchicago() +
                labs(title = "River Mouth Timeseries",
                     col="",
                     y="") +
                 theme(axis.title.x = element_blank()) +
                facet_wrap(~parameter, scales = "free_y", ncol = 1, labeller = as_labeller(nice_labels)) +
                if(input$Peaks) {
                    ggpmisc::stat_peaks(span = input$Span*4+1, col="black")
                }
        ) %>% layout(legend = list(x=100, y=1))
    })
    
    
    output$sinaPlot <- renderPlotly({
        if(input$Peaks) {
            plotly::ggplotly(
                many_selections() %>%
                    group_by(location, parameter) %>%
                    mutate(local_peak = ggpmisc:::find_peaks(measurement, span=input$Span*4+1)) %>%
                    filter(local_peak == TRUE) %>%
                    mutate(daily = yday(date_time),
                           hourly = hour(date_time),
                           minutes = minute(date_time),
                           Julian = daily+(hourly/24)+(minutes/1440),
                           lag1Julian = lag(Julian),
                           change_hrs = (Julian-lag1Julian)*24) %>%
                    ungroup() %>%
                    ggplot(aes(x=parameter, y=change_hrs, col=location, label=date_time)) +
                    ggforce::geom_sina(show.legend = F) +
                    scale_y_continuous(limits = c(0,80)) +
                    ggsci::scale_color_uchicago() +
                    labs(title = "Distribution of Time Between Peaks",
                         y = "Time Between Peaks (hrs)") #+
                    # theme(axis.title.x = element_blank(),
                    #       axis.text.x = element_blank(),
                    #       axis.line.x = element_blank())
                , tooltip = c("label", "y", "x") )
        }
    })
    ## ISCO related plots/code------------------------------------------------------------------------------------------------

    
    isco <- reactive({
    req(input$date)
    shiny::validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    shiny::validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    
    if(input$plot_type=="value") {
    plotly::ggplotly(
      long_isco %>%
        filter(date >= as_date(input$date[1]) & date <= as_date(input$date[2])) %>%
        filter(location %in% input$location) %>%
        filter(pool == input$pool) %>%
        ggplot(aes(date, value, fill=color, text = fraction)) +
        geom_area() +
        scale_fill_identity() +
        facet_wrap(~location, ncol = 1, scales = "free_y", labeller = as_labeller(nice_cities)) +
        labs(title = "Stacked Area Chart") +
        theme(axis.title = element_blank()),
    tooltip = c("x", "y", "text") )
    } else {
      plotly::ggplotly(
        long_isco %>%
          filter(date >= as_date(input$date[1]) & date <= as_date(input$date[2])) %>%
          filter(location %in% input$location) %>%
          filter(pool == input$pool) %>%
          ggplot(aes(date, proportion, fill=color, text = fraction)) +
          geom_area() +
          scale_fill_identity() +
          facet_wrap(~location, ncol = 1, scales = "free_y", labeller = as_labeller(nice_cities)) +
          labs(title = "Stacked Area Chart") +
          theme(axis.title = element_blank()),
      tooltip = c("x", "y", "text") )
    }
    })
    
    output$isco_plot <- renderPlotly(isco() %>% layout(showlegend=FALSE))
    
    variables <- reactive({ 
      shiny::validate(need(input$x!=input$y, "Please seclect two unique variables"))
      c(input$x, input$y)    
      })
    
   prep_daily_biplot <-  reactive({
    
       if(input$lag_x) {
         daily_ts_wide %>%     
      filter(date >= as_date(input$date[1]) & date <= as_date(input$date[2])) %>%
      select(variables(), location, date) %>%
      filter(location %in% input$location) %>%
      rename(x = 1,
             y = 2) %>%
      mutate(x = dplyr::lag(x, n = input$lag_value))
       } else {
         daily_ts_wide %>%
          filter(date >= as_date(input$date[1]) & date <= as_date(input$date[2])) %>%
          select(variables(), location, date) %>%
          filter(location %in% input$location) %>%
          rename(x = 1,
                 y = 2)
       }
   })
   
  daily_biplot <- reactive ({     
    prep_daily_biplot() %>%   
      ggplot(aes(x, y, col=location, text=date)) +
      geom_point(size = 1.75, alpha = 0.8) +
      scale_color_manual(values = c("#5E3C99", "#E66101")) +
      theme(legend.position = "bottom",
            legend.title = element_blank()) +
        labs(title = "Biplot",
          x = paste(input$x),
             y = paste(input$y))
      
    })
    
   daily_ts <- reactive({
     my_daily_ts %>%
       filter(date >= as_date(input$date[1]) & date <= as_date(input$date[2])) %>%
       #pivot_longer(names_to = "parameter", values_to = "measurement", -c(date, location)) %>%
       filter(name %in% input$sup_ts) %>%
       filter(location %in% input$location) %>%
       ggplot(aes(x=date, y=mean, col=location)) +
       geom_line() +
       scale_color_manual(values = c("#5E3C99", "#E66101")) +
       facet_wrap(~name, ncol = 1, scales = "free_y", labeller = as_labeller(nice_parameters)) +
       labs(title = "Supplementary Time Series", 
            y = "",
            x = "")
   })
   
    output$biplot <- renderPlotly(daily_biplot())
    
    output$sup_daily_ts <- renderPlotly(
      ggplotly(daily_ts()) %>%
        layout(showlegend=FALSE)
      )
    ## Survey related plots/code------------------------------------------------------------------------------------------------
    # Survey related plots/code
    
    map_selections <- reactive({
        req(input$surveys)
        shiny::validate(need(!is.na(input$surveys), "Error: Please select at least one survey date."))

        if(input$dodge){
            nearshore %>% filter(Date %in% input$surveys) %>%
                mutate(Lat = if_else(Type=="Bottom", Lat - input$dodge_amount/dd_to_m,
                                     if_else(Type=="Surface", Lat + input$dodge_amount/dd_to_m,
                                             if_else(Type=="M1", Lat + input$dodge_amount/2/dd_to_m,
                                                     if_else(Type=="M2", Lat - input$dodge_amount/2/dd_to_m,
                                                             Lat))))) %>%
                pivot_longer(names_to = "parameter", values_to = "measurement", SRP:`Chl a`) %>%
                filter(parameter == input$parameter) %>%
                mutate(measurement = if_else(measurement < 0, 0.1, measurement))
        } else {
            nearshore %>%
                filter(Date %in% input$surveys) %>%
                pivot_longer(names_to = "parameter", values_to = "measurement", SRP:`Chl a`) %>%
                filter(parameter == input$parameter) %>%
                mutate(measurement = if_else(measurement < 0, 0.1, measurement))
        }

    })

    ctd_selections <- reactive({
        if(input$ctd){
            ctd_plotting %>% filter(Date %in% input$surveys)
        }
    })

    output$survey_map <-    renderLeaflet({
        m %>% addCircles(data = map_selections(),
                         radius = ~sqrt(measurement)*70,
                         label = ~measurement,
                         #popup =  = ~str_c(SRP, Type),
                         color =  ~ifelse(map_selections()[["Type"]]=="Surface", "#E41A1C",
                                          ifelse(map_selections()[["Type"]]=="Bottom", "#377EB8",
                                                 "#4DAF4A")))
    })

    # leafletOutput('survey_map', height = "1000px")

    observe({

        proxy <-  leafletProxy("survey_map", data = map_selections())
        if(input$ctd) {

            proxy %>%
                addCircles(data = ctd_selections(), lat = ~Lat, lng = ~Long, # if I wanna dodge ctd casts to the right +(50/111329)
                           label = "CTD Cast",
                           opacity = 0.5,
                           color = "black",
                           popup = popupGraph(ctd_selections()[["plot"]], height = 500, width = 400))

        }
    })
    
    ## Wind and rain related plots----------------------------------------------------------------------------------
    # Fetch rain data from Synoptic Mesonet API
    my_precip <- reactive({ 
    
    req(input$date)
    shiny::validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    shiny::validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    
    fromJSON(txt = str_c(get_precip,
                         token,
                         "&radius=43.4,-87.8,40&limit=50&pmode=totals&obtimezone=local&start=",
                         str_pad(str_replace_all(ymd(input$date[1]), pattern = "-", replacement = ""), width = 12, side = "right", pad = 0),
                         "&end=",
                         str_pad(str_replace_all(ymd(input$date[2]), pattern = "-", replacement = ""), width = 12, side = "right", pad = 0))) #&status=active
    })

    r_precip_meta <- reactive({
        jsonlite::flatten(my_precip()[["STATION"]]) %>% tibble() 
    })
    
    # if I am using pmode, I need to have this method for flattening the observation data 
    obs <- reactive({ 
        my_precip()[["STATION"]][["OBSERVATIONS"]] %>% flatten_df() %>% tibble()
    })
    
    r_precip <- reactive({ r_precip_meta() %>% bind_cols(obs()) %>%
        mutate(LONGITUDE = as.numeric(LONGITUDE),
               LATITUDE = as.numeric(LATITUDE)) %>%
            filter(total > 0)
    })
    
    # Create a color palette
    pal <- reactive({ colorNumeric(palette = "Blues",
                        domain = r_precip()$total)
    })
    
    # Map of rain data
    
    output$rain_map <- renderLeaflet({
    r %>% addPolygons(data = c(mke_sf, shb_sf), color = c("forestgreen", "sienna"),
                       label = c("Milwaukee River Watershed", "Sheboygan River Watershed"),
                      group = "watershed_sf")  %>%
          addCircles(data = r_precip(),
                    radius = ~scales::rescale(total, to=c(70, 7000)) ,
                    label = ~paste(round(total, 0), "mm", sep = " "),
                    color = ~pal()(total),
                    opacity = 0.8)
    })  
    
    observe({
      
      r_proxy <- leafletProxy("rain_map", data = r_precip())
      
      if(input$land_use){
        r_proxy %>% 
          clearGroup(group = "watershed_sf") %>%
          addPolygons(data=c(shb_sf, mke_sf), color = "black", fillOpacity = 0, weight = 2) %>%
            addRasterImage(shb_lu, opacity = 0.9,
                           colors = my_lu_pal) %>%
            addRasterImage(mke_lu, opacity = 0.9,
                           colors = my_lu_pal) %>%
            addLegend(position = "bottomright", opacity = 0.9,
                      colors = my_lu_colors,
                      labels = my_lu_labels)
      }
      
    })
    
    # Subset data for windrose plot
    wind_trends <- reactive({
        req(input$date)
        shiny::validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
        shiny::validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
        mke_wind %>%
            select(date_time_chicago, date, wind_sp_ms, wind_dir_deg, everything()) %>%
            filter(date >= as_date(input$date[1]) & date <= as_date(input$date[2])) %>%
            mutate(Week = isoweek(date_time_chicago),
                   Month = month(date_time_chicago, label = T),
                   Day = wday(date_time_chicago, label = T))
    })

    # Windrose Plot  
    windrose <- reactive({
        plot.windrose(data = wind_trends(), 
                      spd = wind_trends()[[3]],
                      dir = wind_trends()[[4]],
                      spdmin = 0,
                      dirres = input$dirres) +
            theme(axis.text.x = element_blank(),
                  axis.title.x = element_blank()) +
             labs(title = "Milwaukee Wind Gauge",
                  subtitle = str_c(input$date[1]," through ",input$date[2]))+
            if(input$Monthly & input$date[2]-input$date[1] <= 7) {
                facet_wrap(~wind_trends()$Day, ncol = 2 )  
            } else if(input$Monthly & input$date[2]-input$date[1] <= 31) {
                facet_wrap(~wind_trends()$Week, ncol = 2 )
            } else if(input$Monthly & input$date[2]-input$date[1] > 31) {
                facet_wrap(~wind_trends()$Month )
            }
        
    })
    
    wind_ts <- reactive({
        wind_trends() %>%
            ggplot(aes(date_time_chicago, wind_sp_ms)) +
            geom_line(col="navyblue") +
            labs(y="Wind Speed (m/s)",
                 x="")
    })
    
    output$windrose <- renderPlot({windrose()})
    output$wind_ts <- renderPlot({wind_ts()})
#--------------------------------------------------------------------------------    
} # Close the Server

# Run the application 
shinyApp(ui = ui, server = server)

