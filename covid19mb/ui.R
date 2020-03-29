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
                             div(style='padding:50px 0px 0px 0px',
                                 fluidRow(column(8,
                                                 plotlyOutput("ageHist",width="100%",height=300)
                                                 ),
                                          column(4,
                                                 plotlyOutput("genderHist",width="100%",height=300)
                                                 )
                                          )
                                 
                                 ),
                             div(style='padding:55px 0px 0px 0px',
                                 column(8,
                                        div(style="position:relative; left: 0px;",
                                        fluidRow(plotlyOutput("DIR",width="60%",height=300))
                                        )
                                 ),
                                 column(4,
                                        
                                        fluidRow(div(style="font-size:20px; text-align:center; position:relative; right: 100px;",
                                                     p("Tests completed")),
                                                 div(style="color:Purple; font-size: 30px; font-weight: bold; text-align:center; 
                                                     position:relative; right: 100px;",
                                                     textOutput("Tested"))
                                        )
                                                 
                                        )
                                 )
                             )
                      )
                    ),
           tabPanel("Data",
                    fluidRow(
                      column(8,
                             DT::dataTableOutput("data.table")),
                      column(4,
                             downloadButton("downloadData", "Export to CSV")
                      )
                    )
           ),
           tabPanel("About",
                    h1("Data"),
                    p("All data is collected from the Novel Coronavirus (COVID-19) Bulletins posted on the Manitoba government website. 
                      At this time there is no web scraping involved and all data entry is completed manually. The bulletins can be accessed through the Manitoba government ",
                      a('website.',href='https://news.gov.mb.ca/news/index.html',target="_blank")),
                    h1("Contact Me"),
                    p("If there are any issues with the dashboard or you have any questions, please feel free to contact me at", a('samuel.morrissette01@gmail.com',href='mailto:samuel.morrissette01@gmail.com',target='_blank')),
                    HTML("<ul>
                        <li>Author: Samuel Morrissette</li>
                        <li>Maintainer: Samuel Morrissette</li>
                        <li>Published: 2020-03-18</li>
                      </ul>")
           )
)