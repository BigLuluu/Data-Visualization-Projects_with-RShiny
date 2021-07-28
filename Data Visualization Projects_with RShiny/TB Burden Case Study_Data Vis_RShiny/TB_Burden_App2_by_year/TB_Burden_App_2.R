
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
#install.packages("reshape2")
#install.packages("viridis")
library("viridis")
library(RColorBrewer)
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
library(reshape2)
library(lattice)
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
                  choices = names(TB_df_metrics_rg),
                  selected = "Prevalence"),
      h6("Author: Christina Lu Jin")),
   mainPanel(
      plotOutput(outputId = 'linePlot', width = "100%",
                 height = "600px", click = "plot_click")
    )
  )
)


# Define server for application that plots scatter plot
server <- function(input, output, session) {
  
  output$linePlot <- renderPlot({
    title = "TB Burden Data Metrics Comparison"
    xyplot(TB_df_metrics_rg[,input$metrics] ~ TB_df_agg_rg$Year, main = title,
           xlab = "Year", ylab = input$metrics,
           groups = TB_df_agg_rg$Region, #auto.key=list(corner=c(1,1)),
           # par.settings = list(superpose.symbol = list(pch = 19, cex = 1,
           #                                             col = cividis(6))),
           type="l", lwd = 4, 
           par.settings = list(superpose.line = 
                                 list(lwd = 4, #col = cividis(6)
                                      col = c("#756bb1","#8ca252","#e6ac5b",
                                              "#e7cd48","#c9777c","#b67fac"))),
           key=list(corner=c(0,1), #col = cividis(6),
                    text=list(c("AFR","AMR","EMR","EUR","SEA","WPR")),
                    lines=list(col = c("#756bb1","#8ca252","#e6ac5b",
                                       "#e7cd48","#c9777c","#b67fac"),
                               lwd=4)
                    )
           )
    })
}


#Run the application 
shinyApp(ui = ui, server = server)
