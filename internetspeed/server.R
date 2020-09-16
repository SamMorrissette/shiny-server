library(shiny)
library(tidyverse)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  #All tests is all speedtests from start to now
  all_test <- read_csv('/srv/shiny-server/internetspeed/all_test.csv')
  
  output$download <- renderPlot({
    min_date <- input$dateRange[1]
    max_date <- input$dateRange[2]
    filtered <- all_test %>%
      filter(time >= min_date & time <= max_date)

    ggplot(filtered, aes(x = time, y = download)) +
      geom_point() +
      geom_line()
    })
})
