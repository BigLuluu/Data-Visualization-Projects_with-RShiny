#########################################################
##                                                     ##
##       TB Burden Exploratory App 2 - by year         ##
##    ALY6070 - M4 Assignment  |  Christina Lu Jin     ##
##                                                     ##
#########################################################

## install packages and import library ####
#install.packages("tidyr")
#install.packages("leaflet")
#install.packages("shinydashboard")
#install.packages("sf")
#install.packages("tigris")
#install.packages("htmlwidgets")
# install.packages(c("shinyWidgets", "dslabs",
#                    "tidyverse", "plotly"))
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
#####


# import data
TB_data <- as.data.frame(read.csv("TB_Burden_Country.csv"))
head(TB_data)


# Data prep, reshape and clean up
TB_df <- TB_data[,c(1,5:7,11,31,41,18,24,8,28,38,15,21)]
colnames(TB_df) <- c("Country","Region","Year","Total_Population","Prevalence",
                     "Incidence","Incidence_HIV","Death_no_HIV","Death_HIV",
                     "Prevalence_Rate_per_100k","Incidence_Rate_per_100k",
                     "Incidence_HIV_Rate_per_100k",
                     "Mortality_no_HIV_Rate_per_100k",
                     "Mortality_HIV_Rate_per_100k")
TB_df_agg_rg <- aggregate(TB_df[,c(4:14)],
                          by = list(Region = TB_df$Region,Year = TB_df$Year),
                          FUN = sum)
TB_df_agg_yr <- aggregate(TB_df[,c(4:14)],
                          by = list(Year = TB_df$Year),
                          FUN = sum)
TB_df_metrics_rg <- TB_df_agg_rg[,c(3:13)]
TB_df_metrics_yr <- TB_df_agg_yr[,c(2:12)]



# Define UI for application that plots scatter plot
ui <- fluidPage(
  titlePanel("TB Burden Trend App - by year"),
  sidebarLayout(
    sidebarPanel(
      h5("Retrieved data includes TB records from all around the 
                world, during 1990 to 2014. Prevalence indicates existing 
                number of cases, incident indicates newly added cases,and 
                mortality means number of death cases."),
      selectInput(inputId = "metrics",
                  label = "Select a measure metric:",
                  choices = names(TB_df_metrics_rg)),
      checkboxGroupInput("regions", "Regions to show:",
                         c("African Region" = "AFR",
                           "Region for the Americas" = "AMR",
                           "Eastern Mediterranean Region" = "EMR",
                           "European Region" = "EUR",
                           "South-East Asia Region" = "SEA",
                           "Western Pacific Region" = "WPR",
                           "All Regions" = "ALL"),
                         selected = "AFR", multiple = TRUE),
      h6("Author: Christina Lu Jin")),
    mainPanel(
      plotOutput(outputId = 'linePlot', width = "100%",
                 height = "600px", click = "plot_click")
    )
  )
)



# Define server for application that plots scatter plot
server <- function(input, output, session) {
  
  plot.data <- reactive(
    
  )
  
  output$linePlot <- renderPlot({
    title = "TB Burden Data Metrics Comparison"
    plot(TB_df_agg_yr$Year, TB_df_metrics_yr[,input$metrics], main = title,
         xlab = input$x, ylab = input$y, pch =19,
         col =rgb(0.6,0.4,0.6,0.2))
  })
}

#####
plotData <- reactive(data.frame(country[,input$xData],
                                numericalData[,input$yData]))
output$showText <- renderText(textData())
output$scatterPlot <- renderPlot({
  title = "TB Burden by Year"
  plot(plotData(), main = title,xlab="Year", ylab="Total Number by
Year",
       type="b", col="navy", lwd=5, pch=15)
  


#Run the application 
shinyApp(ui = ui, server = server)

