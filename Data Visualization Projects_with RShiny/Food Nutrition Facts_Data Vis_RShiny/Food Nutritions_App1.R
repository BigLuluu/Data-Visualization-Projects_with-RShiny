library(shiny)

nutritionDF <- data.frame(read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRcUnOzlSxhesAseaGP0BlleMNgsE_ol2kSy4B-z0pR5cN0GKF0ciwe290yPjQDgbs3YRS5ZnZTHv0Z/pub?gid=1435150571&single=true&output=csv"))
nchoice <- 1:ncol(nutritionDF)
names(nchoice) <- names(nutritionDF)


ui <- fluidPage(
    h2('Nutrition facts (serving size: 100g)'),
    selectInput("columns","Select nutrients",choices=nchoice,multiple = T,selected = nchoice[c("name",
                                                                                             "calories",
                                                                                             "total_fat_g",
                                                                                             "protein_g",
                                                                                             "carbohydrate_g")]),
    dataTableOutput('mytable')
    
    
  )
  
server <- function(input, output) {
    
    observeEvent(input$columns,{
      cols <- as.numeric(input$columns)
      #if there are columns input by users
      #first create a dataset with input columns
      #then get the headings of the columns
      #finally render the table and output
      if(length(input$columns) == 1){
        df <- data.frame(nutritionDF[,cols])
        names(df) <- names(nutritionDF)[cols]
        output$mytable = renderDataTable(df)
      #if there are no columns input, display the table with
        #already selected columns (line10)
      }else{
        output$mytable = renderDataTable(nutritionDF[,cols])
        
      }
    })
    
}

shinyApp(ui, server)

#reference: https://shiny.rstudio.com/gallery/datatables-demo.html
