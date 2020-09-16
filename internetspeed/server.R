library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  #Get last test and append to all tests
  last_test <- read_csv('/srv/shiny-server/internetspeed/last_test.csv')
  write.table(last_test,'/srv/shiny-server/internetspeed/all_test.csv', 
              sep=',', 
              append=TRUE, 
              col.names = FALSE, 
              row.names=FALSE)
  
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
