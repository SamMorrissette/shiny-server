library(shiny)
library(tidyverse)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  #All tests is all speedtests from start to now
  all_test <- read_csv('/srv/shiny-server/internetspeed/all_test.csv')
  
  filtered <- reactive({
    all_test %>%
      filter(time >= input$dateRange[1] & time <= input$dateRange[2])
  })
  
  output$download <- renderPlot({
    ggplot(filtered(), aes(x = time, y = download)) +
      geom_point() +
      geom_line()
    })
  output$upload <- renderPlot({
    ggplot(filtered(), aes(x = time, y = upload)) +
      geom_point() +
      geom_line()
  })
  output$ping <- renderPlot({
    ggplot(filtered(), aes(x = time, y = ping)) +
      geom_point() +
      geom_line()
  })
})
