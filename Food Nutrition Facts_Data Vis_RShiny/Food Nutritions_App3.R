
# ui.R

library(shiny)

Food <- as.data.frame(read.csv("/Users/Sayli/Downloads/USDA_nutrition.csv"))
Food1<- Food[c(2,3,4,5,6,10,14,15)]
fluidPage(
    setBackgroundColor(color = c("#CCFFFF", "#C3E9F9", "#2171B5")), #"#66e0ff", "#00a3cc", "#003d4d")),
    titlePanel("App to view food categories and nutrients"),
    sidebarLayout(
        sidebarPanel(   
            tags$style(".well {background-color:#e6f9ff;}"),
            
            selectInput("type", label ="Choose the food item: ",
                        choices= unique(Food1$crop.group),
                        selected= unique(Food1$crop.group)[5]),
            sliderInput(inputId = "range_calories",
                        label = "Choose the Calories range:",
                        min = 0, max = 900, value = c(40,700)),
            sliderInput(inputId = "B12",
                        label = "Choose the Vitamin B12 range:",
                        min = 0, max = 20, value = c(0,20)),
            sliderInput(inputId = "Protein",
                        label = "Choose the Protein range:",
                        min = 0, max = 40, value = c(0,20)),
        ),
        mainPanel(
            tableOutput("table_nu")
        )
    )
)





# server.R

library(shiny)

shinyServer(function(input, output) {
    output$table_nu <- renderTable({
        
        data_filter<- filter(Food1, between(kcal, input$range_calories[1], input$range_calories[2]),
                             between(Vit.B12, input$B12[1], input$B12[2]), 
                             between(protein, input$Protein[1], input$Protein[2]),
                             grepl(tolower(input$type), tolower(crop.group))
        )
    })
    
})

