
#########################################################
##                                                     ##
##            TB Burden Correlation App 3              ##
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
#install.packages("devtools")
#install.packages("ggplot2")
library(ggplot2)
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
TB_df_metrics <- TB_df[,c(4:14)]

# Define UI for application that plots scatter plot
ui <- fluidPage(
    titlePanel("TB Burden Correlation App - between different metrics"),
    sidebarLayout(
        sidebarPanel(
            h5("Retrieved data includes TB records from all around the 
                world, during 1990 to 2014. Prevalence indicates existing 
                number of cases, incident indicates newly added cases,and 
                mortality means number of death cases."),
            selectInput(inputId = "x",
                        label = "Select a independent variable (x):",
                        choices = names(TB_df_metrics),
                        selected = names(TB_df_metrics[[3]])),
            selectInput(inputId = "y",
                        label = "Select a dependent variable (y):",
                        choices = names(TB_df_metrics[,c(2:11)]),
                        selected = names(TB_df_metrics[[3]])),
            h6("Author: Christina Lu Jin")),
    mainPanel(
            plotOutput(outputId = 'scatterPlot', width = "100%",
                       height = "600px", click = "plot_click")
            )
    )
)


# Define server for application that plots scatter plot
server <- function(input, output, session) {
    
    output$scatterPlot <- renderPlot({
        title = "TB Burden Data Metrics Comparison"
        plot(TB_df_metrics[,input$x], TB_df_metrics[,input$y], main = title,
             xlab = input$x, ylab = input$y, pch =19,
             col =rgb(0.6,0.4,0.6,0.2))
        abline(lm(TB_df_metrics[,input$y]~TB_df_metrics[,input$x],
                  data = TB_df_metrics), col =rgb(0.9,0.7,0.6,0.5),
               lty = 8, lwd = 3)
    }, res = 100)
}


#Run the application 
shinyApp(ui = ui, server = server)
