library(leaflet)
library(plotly)
library(shinythemes)
library(DT)

navbarPage("Covid-19 in Manitoba", theme = shinytheme("cerulean"),
           tabPanel("Dashboard",
                    fluidRow(
                      column(6,
                             leafletOutput("map",width="100%",height=700),
                             div(style='padding:10px 0px 0px 40px',
                             fluidRow(p("Zoom-in and click on any region within Manitoba to display a breakdown of data"))
                             )
                      ),
                      column(6,
                             plotlyOutput("timePlot",height=400),
                             div(style='padding:20px 0px 0px 0px',
                                 plotlyOutput("Tested",height=400)
                             )
                      ),
                      column(12,
                             div(style='padding:50px 0px 50px 0px',
                                 fluidRow(column(6,
                                                 plotlyOutput("ageHist",width="90%",height=300)
                                                 ),
                                          column(3,
                                                 fluidRow(plotlyOutput("DIR",width="80%",height=300))
                                                 ),
                                          column(3,
                                                 plotlyOutput("infected",height=300)
                                                 )
                                          )
                                 )
                             )
                      )
                    ),
           
           # tabPanel("Data",
           #          fluidRow(
           #            column(8,
           #                   DT::dataTableOutput("data.table")),
           #            column(4,
           #                   downloadButton("downloadData", "Export to CSV")
           #            )
           #          )
           # ),
           
           tabPanel("About",
                    h1("Data"),
                    p("All data is scraped from the Manitoba Government ",
                      a('website.',href='https://www.gov.mb.ca/covid19/updates/index.html',target="_blank")),
                    h1("Contact Me"),
                    p("If there are any issues with the dashboard or you have any questions, please feel free to contact me at", a('samuel.morrissette01@gmail.com',href='mailto:samuel.morrissette01@gmail.com',target='_blank')),
                    HTML("<ul>
                        <li>Author: Samuel Morrissette</li>
                        <li>Published: 2020-03-18</li>
                      </ul>")
           )
)