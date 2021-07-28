#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#install.packages("tidyr")
#install.packages("leaflet")
#install.packages("shinydashboard")
#install.packages("sf")
#install.packages("tigris")
#install.packages("htmlwidgets")
library(shiny)
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(leaflet)
library(shinydashboard)
library(sf)
library(tigris)
library(htmlwidgets)
library(rnaturalearth)
library(rgeos)



# import data
TB_data <- as.data.frame(read.csv("TB_Burden_Country.csv"))
head(TB_data)
# get country data with latitude and longitude info
country_data <- as.data.frame(read.csv("countries.csv"))
#View(country_data)



# Data prep, reshape and clean up
#TB_df <- TB_data[,c(1,5:7,45,11,31,41,35,18,24,8,28,38,15,21)]
#colnames(TB_df) <- c(..,"Case_Detection_Rate",..,"Incident_HIV_Percent")
TB_df <- TB_data[,c(1,5:7,11,31,41,18,24,8,28,38,15,21)]
names(TB_df)
colnames(TB_df) <- c("Country","Region","Year","Total_Population","Prevalence","Incidence",
                     "Incidence_HIV","Death_no_HIV","Death_HIV","Prevalence_Rate_per_100k",
                     "Incidence_Rate_per_100k","Incidence_HIV_Rate_per_100k",
                     "Mortality_no_HIV_Rate_per_100k","Mortality_HIV_Rate_per_100k")
TB_df_agg <- aggregate(TB_df[,c(4:14)],
                       by = list(Country = TB_df$Country, Region = TB_df$Region,Year = TB_df$Year),
                       FUN = sum)
TB_df_merge <- merge(TB_df_agg,country_data[,c(2:4)], by.x=c("Country"),by.y=c("name"))
TB_df_metrics <- TB_df_merge[,c(1:6,8:11,13,14)]
TB_df_metrics_num <- TB_df_metrics[,c(4:12)]




# Define UI for application that plots a geo-map
ui <- fluidPage(
    titlePanel("TB Burden Exploratory App - by Region and Country"),
    sidebarLayout(
        sidebarPanel(
            h5("All data metrics are aggregated by country (categorized by year).
               Prevalence indicates existing number of cases, incident indicates newly added cases,
               and mortality means number of death cases. 
               Retrieved data includes TB records from 1990 to 2014"),
            selectInput(inputId = "metrics",
                        label = "Select a measure metric:",
                        choices = names(TB_df_metrics_num)),
            sliderInput(inputId = "bublesize",
                        label = "Adjust bubble Size",
                        min = 1,max = 10,step = 1,value = 3)
        ),
        
        mainPanel(
            leafletOutput(outputId = "map")
        )
    )
)


# Define server logic required to plot a geo-map
server <- function(input, output) {
    
    
    
    output$map <- renderLeaflet({
        
        # Set color palette
        colors <- c("darkorange","gold","darkgreen")
        pal <- colorFactor(colors,domain= TB_df_metrics_num$Prevalence)
        
        # Set popup label
        labels = sprintf(
            "<strong>%s</strong><br/>Region: %s<br/>Total Population: %g/<br/>Number of Cases: %g",
            TB_df_metrics$Country,TB_df_metrics$Region,TB_df_metrics$Total_Population,
            TB_df_metrics[,input$metrics]) %>% lapply(htmltools::HTML)
        
        # Set popup label
        # labels <- TB_df_metrics %>% transmute(popup_info=paste(Country,"<br/>",
        #                                                        "Region:",Region,"<br/>","
        #                                                        Total Population:",Total_Population))
         
        #initialize the leaflet object
        basemap = leaflet() %>%
            addProviderTiles(provider = "CartoDB.Positron") %>%
            addCircleMarkers(data=TB_df_merge,lat = ~latitude,lng = ~longitude, 
                             radius = ~1,popup = labels,color=~pal(Prevalence))
        # addLegend("bottomright",
        #           pal = pal,
        #           values = TB_df_metrics[,input$metrics],
        #           title = "Cases / Rates",
        #           Opacity = 0.7)
    })
    
}


#Run the application 
shinyApp(ui=ui, server=server)
