library(shiny)
library(tidyverse)

shinyServer(function(input, output) {
  #All tests is all speedtests from start to now
  all_test <- read_csv('/home/sam/SpeedTesting/all_test.csv')
  all_test$download <- all_test$download / (1024*1024)
  
  filtered <- reactive({
    all_test %>%
      filter(time >= input$dateRange[1] & time <= input$dateRange[2]) %>% 
      mutate(hour = as.numeric(format(as.POSIXct(time), "%H"))) %>%
      filter(hour >= input$timeRange[1] & hour < input$timeRange[2])
  })
  
  output$download <- renderPlot({
    ggplot(filtered(), aes(x = time, y = download)) +
      geom_point() +
      geom_line() + 
      theme_light()
    })
  output$upload <- renderPlot({
    ggplot(filtered(), aes(x = time, y = upload)) +
      geom_point() +
      geom_line()  + 
      theme_light()
  })
  output$ping <- renderPlot({
    ggplot(filtered(), aes(x = time, y = ping)) +
      geom_point() +
      geom_line() + 
      theme_light()
  })
})
