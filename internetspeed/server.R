library(shiny)
library(tidyverse)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  #All tests is all speedtests from start to now
  all_test <- read_csv('/srv/shiny-server/internetspeed/all_test.csv')
  vals <- reactiveValues()
  vals <- reactiveValues()
  observe({
    vals$min_date <- input$dateRange[1]
    vals$max_date <- input$dateRange[2]
  })
  
  output$download <- renderPlot({
    filtered <- all_test %>%
      filter(time >= min_date & time <= max_date)
    ggplot(filtered, aes(x = time, y = download)) +
      geom_point() +
      geom_line()
    })
  output$upload <- renderPlot({
    filtered <- all_test %>%
      filter(time >= min_date & time <= max_date)
    ggplot(filtered, aes(x = time, y = upload)) +
      geom_point() +
      geom_line()
  })
  output$download <- renderPlot({
    filtered <- all_test %>%
      filter(time >= min_date & time <= max_date)
    ggplot(filtered, aes(x = time, y = ping)) +
      geom_point() +
      geom_line()
  })
})
