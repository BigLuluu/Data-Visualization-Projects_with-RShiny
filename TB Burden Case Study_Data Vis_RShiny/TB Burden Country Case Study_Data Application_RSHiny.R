#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(readr)
library(dplyr)
#install.packages("leaflet")
#install.packages("shinydashboard")
#install.packages("sf")
#install.packages("tigris")
#install.packages("htmlwidgets")
library(leaflet)
library(shinydashboard)
library(sf)
library(tigris)
library(htmlwidgets)


# import data
TB_df <- as.data.frame(read.csv("data/TB_Burden_Country.csv"))
head(TB_df)


# Data prep, reshape and clean up
TB_df_clean <- TB_df[,c(1,5:7,45,11,31,41,35,18,24,8,28,38,15,21)]
names(TB_df_clean)
colnames(TB_df_clean) <- c("Country","Region","Year","Total_Population","Case_Detection_Rate",
                           "Prevalence","Incidence","Incidence_HIV","Incident_HIV_Percent",
                           "Death_no_HIV","Death_HIV","Prevalence_Rate_per_100k",
                           "Incidence_Rate_per_100k","Incidence_HIV_Rate_per_100k",
                           "Mortality_no_HIV_Rate_per_100k","Mortality_HIV_Rate_per_100k")
TB_case <- TB_df_clean[,c(1:4,6:8,10,11)]
names(TB_case)
TB_rates <- TB_df_clean[,c(1:4,5,9,12:16)]
names(TB_rates)
TB_metrics <- TB_df_clean[,c(4:16)]


# Define UI for application that plots a geo-map
ui <- fluidPage(
  titlePanel("TB Burden Exploratory Geo-map"),
  sidebarLayout(
    sidebarPanel(
      h5("Measure Metrics"),
      h5("All data metrics are aggregated by year (categorized by year).
               Prevalence indicates existing nuber of cases, incident indicates newly added cases,
               and mortality means number of death cases. 
               Retrieved data includes records from 1990 to 2014"),
      selectInput(inputId = "metrics",
                  label = "Select a metric:",
                  choices = names(TB_metrics))
    ),
    
    mainPanel(
      leafletOutput("interactive_map")
    )
  )
)


# Define server logic required to plot a geo-map
server <- function(input, output) {

measure_metrics <- reactive({
  mm <- TB_df_clean[,c(1:4, input$metrics)]
  return(mm)
})

selected_metrics <- reactive({
  sm <- TB_df_clean[,input$metrics]
  return(sm)
})

output$interactive_map <- renderLeaflet({
  # Set color palette
  pal <- colorBin(palette = "OrGn", 12,
                  domain = measure_metrics)
  
  labels = sprintf(
    "<strong>%s</strong><br/>%g number of cases / cases per 100,000 people",
    measure_metrics()$Country, selected_metrics()) %>%
    lapply(htmltools::HTML)
  
  measure_metrics() %>%
    st_transform(crs = "+init=epsg:4326") %>%
    leaflet() %>%
    addProviderTiles(provider = "CartoDB.Positron") %>%
    #setView(x-coord,y-coord,zoom) %>%
    addPolygons(label = labels,
                stroke = FALSE,
                smoothFactor = 0.5,
                opacity = 1,
                fillOpacity = 0.7,
                fillColor = ~pal(selected_metrics()),
                highlightOptions = highlightOptions(weight = 5,
                                                    fillOpacity = 1,
                                                    color = "black",
                                                    opacity = 1,
                                                    bringToFront = TRUE)) %>%
    addLegend("bottomright",
              pal = pal,
              values = ~selected_metrics,
              title = "Cases / Rates",
              Opacity = 0.7)
})

}

server <- function(input, output, session){
  
  map_centre = st_centroid(TB_df_clean %>% filter(Country == "Iraq")) %>% 
    st_coordinates()
  
  # reactive expression for the data subset to what the user selected
  measure_metrics <- reactive({
    mm <- TB_df_clean[,c(1:4, input$metrics)]
    return(mm)
  })
  
  selected_metrics <- reactive({
    sm <- TB_df_clean[,input$metrics]
    return(sm)
  })
  
  
  output$map <- renderLeaflet({
    # Static elements (things don't change)
    leaflet() %>% addTiles() %>%
      setView(lng = map_centre[, "X"], map_centre[, "Y"], zoom = 2)
  })
  
  # Changes to the map performed in an observer
  observe({
    proxy = leafletProxy("interactive_map", data = measure_metrics()) %>% 
      clearShapes()
    # Show or hide legend
    proxy %>% clearControls() %>% addPolygons(fillColor = ~pal(selected_metrics))
    if (input$legend) {
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~Production)
    }
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
