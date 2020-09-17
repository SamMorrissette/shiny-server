library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Internet Speed"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      dateRangeInput('dateRange',
                     label = 'Select date range',
                     start = Sys.Date() - 3, end = Sys.Date() + 3,
                     min = Sys.Date() - 10, max = Sys.Date() + 10
                     ),
      sliderInput("timeRange", label ="Select Hour Range", min = 0, 
                  max = 24, value = c(0, 24))
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("download"),
       plotOutput("upload"),
       plotOutput("ping")
    )
  )
))
