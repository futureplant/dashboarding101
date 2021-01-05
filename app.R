## app.R ##
library(shiny)
library(shinydashboard)
library(leaflet)
library(dashboardthemes)

vars <- setdiff(names(iris), "Species")

ui <- dashboardPage(
  dashboardHeader(title = "dashboard101"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Graph", tabName = "graph", icon = icon("dashboard")))
  ),
  
  dashboardBody(
    shinyDashboardThemes( theme = "blue_gradient"),
    
    tabItems(
      # First tab content
      tabItem(tabName = "map",
              fluidRow(column(3,box(width = 12, sliderInput("slider1", "Number of points",
                                             min = 1, max = 20, value = 10),
                                            selectInput("color", "Color", 
                                                        choices = list("blue" = 1, "red" = 2,
                                                                       "yellow" = 3), selected = 1))),
                       column(9, box(width = 12, leafletOutput("mymap")))),
              
              ),
      
      # Second tab content
      tabItem(tabName = "graph",
              column(3,box(width = 12, selectInput('xcol', 'X Variable', vars),
                           selectInput('ycol', 'Y Variable', vars, selected = vars[[2]]),
                           numericInput('clusters', 'Cluster count', 3, min = 1, max = 9))),
              column(9, box(width = 12, plotOutput('plot1')))
              
      )
    )
  )
)

server <- function(input, output) {
  
  #------------------------------------------------------------------------------------
  # Map
  #------------------------------------------------------------------------------------
  
  # Create a random distribution of points based on slider input
  points <- eventReactive(input$slider1, {
    cbind(rnorm(input$slider1, sd = 0.2) + 5, rnorm(input$slider1, sd = 0.2) + 52)
  }, ignoreNULL = FALSE)
  
  
  getColor <- function(color){
    if(color == 1){
      'blue'
      }else if(color == 2){
        'red'
      }else{
        'yellow'
      } 
  }
  
  # Plot point distribution on leaflet map
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addCircleMarkers(data = points(), color = getColor(input$color))
  })
  
  
  
  #------------------------------------------------------------------------------------
  # Graph 
  #------------------------------------------------------------------------------------
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
  })
  
}

shinyApp(ui, server)