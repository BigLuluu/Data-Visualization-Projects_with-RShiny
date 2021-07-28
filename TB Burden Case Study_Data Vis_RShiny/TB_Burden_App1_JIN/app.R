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
#TB_data <- as.data.frame("TB_Burden_Country")
head(TB_data)

country_sp <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")

#View(country_sp)
#head(country_sp,10)\

# Data prep, reshape and clean up
#TB_df <- TB_data[,c(1,5:7,45,11,31,41,35,18,24,8,28,38,15,21)]
#colnames(TB_df) <- c(..,"Case_Detection_Rate",..,"Incident_HIV_Percent")

TB_df <- TB_data[,c(1,5:7,11,31,41,18,24,8,28,38,15,21)]
names(TB_df)
colnames(TB_df) <- c("Country","Region","Year","Total_Population","Prevalence","Incidence",
                     "Incidence_HIV","Death_no_HIV","Death_HIV","Prevalence_Rate_per_100k",
                     "Incidence_Rate_per_100k","Incidence_HIV_Rate_per_100k",
                     "Mortality_no_HIV_Rate_per_100k","Mortality_HIV_Rate_per_100k")
TB_case <- TB_df[,c(1:9)]
names(TB_case)
TB_rates <- TB_df[,c(1:4,10:14)]
names(TB_rates)
TB_df_metrics <- aggregate(TB_df[,c(4:14)],
                           by = list(Country = TB_df$Country, Region = TB_df$Region,Year = TB_df$Year), 
                           FUN = sum)
TB_df_metrics_num <- TB_df_metrics[,c(4:14)]


# Define UI for application that plots a geo-map
ui <- fluidPage(
    titlePanel("TB Burden Exploratory App - by Region and Country"),
    sidebarLayout(
        sidebarPanel(
            h5("Measure Metrics"),
            h5("All data metrics are aggregated by country (categorized by country).
               Prevalence indicates existing nuber of cases, incident indicates newly added cases,
               and mortality means number of death cases. 
               Retrieved data includes records from 1990 to 2014"),
            selectInput(inputId = "metrics",
                        label = "Select a metric:",
                        choices = names(TB_df_metrics_num),
                        )
        ),
        
        mainPanel(
            leafletOutput(outputId = "map")
        )
    )
)


# Define server logic required to plot a geo-map
server <- function(input, output) {
    
#    selected_metrics <- reactive({
#        sm <- TB_df_metrics[,input$metrics]
#        return(sm)
#    })
    
    output$map <- renderLeaflet({
       
        # Set color palette
        pal <- colorBin(palette = "OrYlGn", 12,
                        domain = TB_df_metrics[,input$metrics])
    
        # labels = sprintf(
        #     "<strong>%s</strong><br/>%g number of cases / cases per 100,000 people",
        #     TB_df_metrics$Country, TB_df_metrics[,input$metrics]) %>%
        #     lapply(htmltools::HTML)
        
        #initialize the leaflet object
        

        mapTB <- merge(country_sp,TB_df_metrics, by.x = c("name"), by.y = c("Country"))

        
        testTB <- st_transform(mapTB)
        View(testTB)
        basemap = mapTB %>%
            st_transform( crs = "+init=epsg:4326") %>%
            leaflet() %>%
            #addProviderTiles(providers$Stamen.TonerLite,
            #                 options = providerTileOptions(noWrap = TRUE)) #%>%
            addProviderTiles(provider = "CartoDB.Positron") %>%
            addPolygons(#data = TB_df_metrics_num[,input$metrics],
                        label = labels,
                        stroke = FALSE,
                        smoothFactor = 0.5,
                        opacity = 1,
                        fillOpacity = 0.7#,
                        # fillColor = ~pal(TB_df_metrics[,input$metrics]),
                        # highlightOptions = highlightOptions(weight = 5,
                        #                                     fillOpacity = 1,
                        #                                     color = "black",
                        #                                     opacity = 1,
                        #                                     bringToFront = TRUE)
                        ) #%>%
            # addLegend("bottomright",
            #           pal = pal,
            #           values = TB_df_metrics[,input$metrics],
            #           title = "Cases / Rates",
            #           Opacity = 0.7)
    })
    
}


#Run the application 
shinyApp(ui=ui, server=server)
