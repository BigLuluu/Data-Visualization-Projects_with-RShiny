library(ggplot2)
library(Cairo)   ## For nicer ggplot2 output when deployed on Linux
# comments with ## are from original code
# comments with # are by me

foodDF2 <- data.frame(read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTYvYjIDZ4_eZ1_iJDziSlHYqRfTwg5VXnk9D1IxbFvVtaYT9d7DcFazuIYZS4Ie01Lc56qpT6QfKQC/pub?gid=982927371&single=true&output=csv"))
vars <- names(foodDF2)
ui <- fluidPage(
  #heading, interesing it uses the HTML markdown-like syntax
  h2('Nutrition facts'),

  fluidRow(
    selectInput('xcol', 'X Variable', vars, selected = vars[[3]]),
    selectInput('ycol', 'Y Variable', vars, selected = vars[[1]])
  ),
  #plot output with clickable and brush functions
  fluidRow(
    column(width = 10,
           plotOutput("plot1", height = 300,
                      dblclick = "plot1_dblclick", #name the double click
                      click = "plot1_click",#name the sigle click 
                      brush = brushOpts(
                        id = "plot1_brush",#name the brush
                        resetOnNew = T #reset the brush is there are new brush input
                      )
           )
    )
  ),
  #rows to output information on click and on brush(a group of info)
  fluidRow(
    column(width = 10,
           h4("Points near click"),
           verbatimTextOutput("click_info")
    ),
    column(width = 10,
           h4("Brushed points"),
           verbatimTextOutput("brush_info")
    )
  )
)

server <- function(input, output) {
  #ranges is set so we can have coordinates of the mouse (to click and drag)
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  #reactive dataset
  selectedData <- reactive({
    foodDF2[, c(input$xcol, input$ycol)]
  })
  
  # aes_string to set reactive x and y axis input
  output$plot1 <- renderPlot({
    ggplot(selectedData(), aes_string(input$xcol, input$ycol)) +
      geom_point() +
      theme_classic()+
      #coord_cartesian to set limits on the coordinate system to zoom the plot
      #originally code sets expand to FALSE, but here I adjust the expand parameter
      #to TRUE so that it adds a small expansion factor to the limits to ensure data
      # and axes don't overlap
      coord_cartesian(xlim = ranges$x, ylim = ranges$y)
  })
  
  ## When a double-click happens, check if there's a brush on the plot.
  ## If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  output$click_info <- renderPrint({
    ## Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    ## were a base graphics plot, we'd need those.
    nearPoints(foodDF2, input$plot1_click)
  })
  
  output$brush_info <- renderPrint({
    brushedPoints(foodDF2, input$plot1_brush)
  })
}

shinyApp(ui, server)

#reference:
#https://shiny.rstudio.com/gallery/plot-interaction-zoom.html
